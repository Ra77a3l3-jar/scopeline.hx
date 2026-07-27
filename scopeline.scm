(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/static.scm")
(require "helix/treesitter.scm")
(require "helix/components.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require "glyph/glyph.scm")
(require-builtin helix/core/text as text.)

(provide scopeline-enable
         scopeline-disable
         scopeline-toggle
         scopeline-configure!)

(struct ScopeCrumb (kind name start end))

;; read one env var, steel has no getenv
(define (scopeline-env name)
  (with-handler
    (lambda (_) #f)
    (let* ([p (open-input-file "/proc/self/environ")]
           [raw (read-port-to-string p)])
      (close-input-port p)
      (let* ([key (string-append name "=")]
             [klen (string-length key)]
             [rlen (string-length raw)])
        (let outer ([i 0])
          (cond
            [(> (+ i klen) rlen) #f] ; ran past the end
            [(and (or (= i 0) (= (char->integer (string-ref raw (- i 1))) 0)) ; at a var boundary
                  (let inner ([j 0])
                    (cond
                      [(= j klen) #t]
                      [(char=? (string-ref raw (+ i j)) (string-ref key j)) (inner (+ j 1))]
                      [else #f])))
             (let scan ([k (+ i klen)]) ; read up to next NUL
               (if (or (>= k rlen) (= (char->integer (string-ref raw k)) 0))
                   (substring raw (+ i klen) k)
                   (scan (+ k 1))))]
            [else (outer (+ i 1))]))))))

;; language query dir
(define *scopeline-lang-dir*
  (or (with-handler (lambda (_) #f)
        (let ([m (current-module)]) (and m (string-append (parent-name m) "/languages/"))))
      (let ([sh (scopeline-env "STEEL_HOME")])
        (and sh (string-append sh "/cogs/scopeline/languages/")))
      (string-append (get-helix-cwd) "/languages/")))

(define *scopeline-query-cache* (hash))

(define (scopeline-load-query lang)
  (let ([cached (hash-try-get *scopeline-query-cache* lang)])
    (cond
      [(equal? cached 'none) #f]
      [cached cached]
      [else
       (let* ([path (string-append *scopeline-lang-dir* lang ".tsq")]
              [query
               (and (path-exists? path)
                    (with-handler (lambda (_) #f)
                      (let* ([p (open-input-file path)]
                             [src (read-port-to-string p)])
                        (close-input-port p)
                        (string->tsquery lang src))))])
         (set! *scopeline-query-cache*
               (hash-insert *scopeline-query-cache* lang (or query 'none)))
         query)])))

(define scopeline-loader (tsquery-loader scopeline-load-query))

;; source text of a node
(define (scopeline-node-text node text)
  (let ([s (tsnode-start-byte node)]
        [e (tsnode-end-byte node)])
    (if (and (< s e) (<= e (text.rope-len-bytes text)))
        (text.rope->string (text.rope->byte-slice text s e))
        "")))

(define (scopeline-innermost rctx node)
  (let ([ns (tsnode-start-byte node)]
        [ne (tsnode-end-byte node)])
    (let loop ([xs rctx])
      (cond
        [(empty? xs) #f]
        [(and (<= (tsnode-start-byte (car xs)) ns) (>= (tsnode-end-byte (car xs)) ne))
         (car xs)]
        [else (loop (cdr xs))]))))

;; one crumb per scope
(define (scopeline-dedup crumbs)
  (let loop ([xs crumbs] [seen '()] [acc '()])
    (if (empty? xs)
        (reverse acc)
        (let* ([c (car xs)]
               [key (cons (ScopeCrumb-start c) (ScopeCrumb-end c))])
          (if (member key seen)
              (loop (cdr xs) seen acc) ; skip duplicate range
              (loop (cdr xs) (cons key seen) (cons c acc)))))))

;; match to a sorted crumb list
(define (scopeline-build-crumbs match text)
  (define contexts
    (let ([nodes (tsmatch-capture match "context")])
      (if (list? nodes)
          (sort nodes (lambda (a b) (< (tsnode-start-byte a) (tsnode-start-byte b))))
          '())))
  (define rctx (reverse contexts)) ; inner to outer for lookup
  (define crumbs
    (foldl
     (lambda (cap acc)
       (if (or (equal? cap "context") (starts-with? cap "_"))
           acc
           (let ([nodes (tsmatch-capture match cap)]
                 [kind (string->symbol cap)])
             (if (list? nodes)
                 (foldl
                  (lambda (n a)
                    (let ([ctx (scopeline-innermost rctx n)]) ; conect name to its scope
                      (if ctx
                          (cons (ScopeCrumb kind
                                            (scopeline-node-text n text)
                                            (tsnode-start-byte ctx)
                                            (tsnode-end-byte ctx))
                                a)
                          a)))
                  acc
                  nodes)
                 acc))))
     '()
     (tsmatch-captures match)))
  (scopeline-dedup (sort crumbs (lambda (a b) (< (ScopeCrumb-start a) (ScopeCrumb-start b))))))

(define (scopeline-doc-crumbs doc-id)
  (let ([match (query-document scopeline-loader doc-id)])
    (if (TSMatch? match)
        (scopeline-build-crumbs match (editor->text doc-id))
        '()))) ; no syntax tree avaible

(define (scopeline-cursor-path crumbs doc-id)
  (let* ([text (editor->text doc-id)]
         [pos (min (cursor-position) (text.rope-len-chars text))]
         [byte (text.rope-char->byte text pos)])
    (filter (lambda (c) (and (<= (ScopeCrumb-start c) byte) (>= (ScopeCrumb-end c) byte)))
            crumbs)))

(define (scopeline-current-doc-id)
  (with-handler (lambda (_) #f) (editor->doc-id (editor-focus))))

(define *scopeline-enabled?* #f)
(define *scopeline-hooks?* #f)
(define *scopeline-debounce* (box #f))
(define *scopeline-doc-id* #f)
(define *scopeline-crumbs* '())
(define *scopeline-path* '())
(define *scopeline-file* #f)

(define *scopeline-bg* #f)

(define *scopeline-sep* " › ")

;;@doc
;; Set scopeline options from init.scm, #:bg paints the bar background
(define (scopeline-configure! #:bg [bg #f])
  (set! *scopeline-bg* bg)
  (when *scopeline-enabled?* (helix.redraw)))

(define (scopeline-bar-style)
  (if *scopeline-bg*
      (style-bg (theme-scope-ref "ui.text") (glyph-hex->color *scopeline-bg*))
      (theme-scope-ref "ui.background")))

(define (scopeline-bar-bg)
  (if *scopeline-bg*
      (glyph-hex->color *scopeline-bg*)
      (style->bg (theme-scope-ref "ui.background"))))

(define (scopeline-seg fg bg)
  (let ([s (if fg (style-fg (style) fg) (style))])
    (if bg (style-bg s bg) s)))

;; file then one entry per scope
(define (scopeline-segments base sep bg)
  (define crumbs
    (foldl
     (lambda (c acc)
       (append acc
               (list (cons *scopeline-sep* sep)
                     (cons (glyph-kind-icon (ScopeCrumb-kind c)) ; icon for the kind
                           (scopeline-seg (glyph-hex->color (glyph-kind-color (ScopeCrumb-kind c))) bg))
                     (cons " " base)
                     (cons (ScopeCrumb-name c) base))))
     '()
     *scopeline-path*))
  (if *scopeline-file*
      (let ([name (file-name *scopeline-file*)])
        (append (list (cons (glyph-icon name)
                            (scopeline-seg (glyph-hex->color (glyph-color name)) bg))
                      (cons " " base)
                      (cons name base))
                crumbs))
      '()))

;; paint left to right
(define (scopeline-draw frame y width segs)
  (let loop ([xs segs] [x 1])
    (cond
      [(empty? xs) void]
      [(>= x (- width 1)) void]
      [else
       (let* ([text (car (car xs))]
              [style (cdr (car xs))]
              [room (- width 1 x)]
              [shown (if (> (string-length text) room) (substring text 0 (max 0 room)) text)])
         (frame-set-string! frame x y shown style)
         (loop (cdr xs) (+ x (string-length shown))))])))

(define (scopeline-render state rect frame)
  (when *scopeline-enabled?*
    (with-handler
      (lambda (_) void)
      (let* ([width (area-width rect)]
             [bg (scopeline-bar-bg)]
             [base (scopeline-seg (style->fg (theme-scope-ref "ui.text")) bg)]
             [sep (scopeline-seg (style->fg (theme-scope-ref "comment")) bg)])
        (buffer/clear-with frame (area 0 0 width 1) (scopeline-bar-style))
        (scopeline-draw frame 0 width (scopeline-segments base sep bg))))))

;; recompute the path at the cursor
(define (scopeline-refresh-path!)
  (let ([doc-id (scopeline-current-doc-id)])
    (when doc-id
      (unless (equal? doc-id *scopeline-doc-id*) ; buffer switch makes query again
        (set! *scopeline-doc-id* doc-id)
        (set! *scopeline-crumbs* (scopeline-doc-crumbs doc-id)))
      (set! *scopeline-path* (scopeline-cursor-path *scopeline-crumbs* doc-id))
      (set! *scopeline-file* (with-handler (lambda (_) #f) (cx->current-file))))))

(define (scopeline-refresh-crumbs!)
  (let ([doc-id (scopeline-current-doc-id)])
    (when doc-id
      (set! *scopeline-doc-id* doc-id)
      (set! *scopeline-crumbs* (scopeline-doc-crumbs doc-id))
      (scopeline-refresh-path!)
      (helix.redraw))))

(define (scopeline-debounce ms thunk)
  (unless (unbox *scopeline-debounce*)
    (set-box! *scopeline-debounce* #t)
    (enqueue-thread-local-callback-with-delay
     ms
     (lambda ()
       (thunk)
       (set-box! *scopeline-debounce* #f)))))

(define (scopeline-register-hooks!)
  (unless *scopeline-hooks?*
    (register-hook 'document-changed
                   (lambda (_ _) (when *scopeline-enabled?* (scopeline-debounce 150 scopeline-refresh-crumbs!))))
    (register-hook 'document-opened
                   (lambda (_) (when *scopeline-enabled?* (scopeline-debounce 50 scopeline-refresh-crumbs!))))
    (register-hook 'document-saved
                   (lambda (_) (when *scopeline-enabled?* (scopeline-debounce 50 scopeline-refresh-crumbs!))))
    (register-hook 'selection-did-change
                   (lambda (_) (when *scopeline-enabled?* (scopeline-refresh-path!) (helix.redraw))))
    (set! *scopeline-hooks?* #t)))

;;@doc
;; Show the breadcrumb bar
(define (scopeline-enable)
  (unless *scopeline-enabled?*
    (set! *scopeline-enabled?* #t)
    (scopeline-register-hooks!)
    (set-editor-clip-top! 2)
    (push-component!
     (new-component! "scopeline"
                     #f
                     scopeline-render
                     (hash "cursor" (lambda (state rect) #f)
                           "handle_event" (lambda (state event) event-result/ignore))))
    (scopeline-debounce 50 scopeline-refresh-crumbs!)))

;;@doc
;; Hide the breadcrumb bar
(define (scopeline-disable)
  (when *scopeline-enabled?*
    (set! *scopeline-enabled?* #f)
    (pop-last-component-by-name! "scopeline")
    (set-editor-clip-top! 0)
    (helix.redraw)))

;;@doc
;; Toggle the breadcrumb bar
(define (scopeline-toggle)
  (if *scopeline-enabled?* (scopeline-disable) (scopeline-enable)))

;; on by default, after startup
(enqueue-thread-local-callback scopeline-enable)
