# scopeline

A breadcrumb bar for [Helix](https://github.com/helix-editor/helix) (Steel plugin), like `barbecue.nvim` or `nvim-navic`

![scopeline demo](.github/assets/preview.gif)

## Installation

Install with forge:

```
forge pkg install --git https://github.com/Ra77a3l3-jar/scopeline.git
```

Then load it in your `init.scm`:

```scheme
(require "scopeline/scopeline.scm")
```

If you use [moka.hx](https://github.com/Ra77a3l3-jar/moka.hx)'s bufferline, scopeline sits directly under it. Without moka it draws on the top row. The row is always reserved, so the view never resizes when the breadcrumb is empty.

## Usage

| Command | What it does |
| --- | --- |
| `scopeline-toggle` | Turn the bar off or on for the session |
| `scopeline-enable` | Show the bar |
| `scopeline-disable` | Hide the bar and free the row |

## Configuration

Call `scopeline-configure!` from `init.scm`.

```scheme
(scopeline-configure! #:bg "#1e1e2e"        ; bar background, by default uses theme bg
                      #:separator " › "     ; drawn between levels
                      #:max-depth 0         ; deepest levels to keep, 0 is all
                      #:show-file? #t       ; leading file icon and name
                      #:position 'top-left  ; corner: 'top-left 'top-right 'bottom-left 'bottom-right
                      #:always-reserved? #t) ; keep the row even when nothing is shown
```

By default `#:bg` is `#f`, so the bar uses the theme background and looks transparent.

`#:position` places the bar in the chosen corner. The bottom corners draw one row above moka's statusline, or one row above the native statusline when moka is absent.

`#:always-reserved?` is `#t` by default, so the bar row is always there and the view never resizes. Set it to `#f` for the old behavior: the row is only used while the cursor is inside a scope and freed on blank lines or scope-less lines.

## Languages

Context queries ship for, rust, python, c, cpp, go, java, javascript, typescript, zig, nix, scheme, ocaml, markdown, json, toml, nasm, gas, lua, bash, c-sharp, xml, odin, latex, haskell.
Each lives in `languages/<lang>.tsq` and captures a container as `@context` plus a name whose capture is the kind icon.

## Contributing

PRs to increase the number of supported languages are very aprecieted.

## Credits

This plugin is a fork of the plugin [context.hx](https://codeberg.org/gwid/context.hx)
