# VHS tape for scopeline.hx — https://github.com/charmbracelet/vhs
# Run with: vhs demo.tape
#
# Navigates ~/Dev/ghostty (a Zig codebase) read only, no edits or saves.
# Uses the real helix config, where scopeline sits under moka's bufferline with
# (scopeline-configure! #:show-file? #f), so only the scope path is shown.

Require hx

Output "/home/raffaele/Dev/scopeline/demo.gif"

Set Shell "bash"
Set Theme "VibrantInk"
Set FontFamily "JetBrainsMono Nerd Font Mono"
Set FontSize 18
Set Width 1600
Set Height 1000
Set Padding 20
Set TypingSpeed 60ms
Set WindowBar Colorful

Hide
Type "cd ~/Dev/ghostty && hx src/input/Binding.zig src/config/Config.zig"
Enter
Sleep 2s
Type ":theme catppuccin_mocha_transparent"
Enter
Sleep 1s
Show

Sleep 700ms

# File 1: Binding.zig. Start at the Parser struct, then scroll down through its
# methods (init, parseFlags, next) and the SequenceIterator, watching the
# breadcrumb follow the cursor, then scroll back up.
Type ":74"
Enter
Sleep 1.2s
Down@60ms 68
Sleep 900ms
Down@60ms 60
Sleep 900ms
Up@45ms 120
Sleep 1s

# switch to the second buffer (n opens the buffer menu, n picks buffer-next)
Type "nn"
Sleep 1.2s

# File 2: Config.zig. Jump to the deeply nested Replay iterator, then scroll
# through it: Replay, the nested Iterator, and Iterator.next show three levels.
Type "/const Replay = struct"
Enter
Sleep 1.2s
Down@70ms 70
Sleep 1.4s
Up@55ms 70
Sleep 1s

Hide
Type ":qa!"
Enter
Sleep 500ms
