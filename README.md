# emacs-config
My Emacs configuration and elisp snippets.

It uses .emacs.d/init.el, rather than .emacs, so it's easy to put into a git repo.

On X, it assumes the use of the EXWM window manager. Well, why wouldn't you? ;-)

## Packages
I have (at least) the following packages installed:

- markdown-mode
- exec-path-from-shell
- exwm
- magit

## EXWM randr config
EXWM supports xrandr but it's a little bit "here's some bits, take it or leave it". lisp/exwm-randr-control.el is my attempt to make it a bit more complete.

Specifically, it:

- binds <XF86Display\> to toggle round mirror, split, off.
- binds s-a to move the current workspace to the primary display
- binds s-s to move the current workspace to the second display
- binds s-d to move the current workspace to the third display
- binds s-f to move the current workspace to the fourth display

This has the nice property that it never moves any other buffer. Likewise, using s-n to move focus to a workspace *never* moves anything anywhere, so it's easy to understand what's happening, unlike XMonad's sometimes surprising behaviour.

In split mode, the displays are assumed to be stacked one above the other, with the primary workspace at the bottom. Changing that would be easy, but I haven't thought of a usable UI for it yet.

Note that there is sometimes some odd behaviour with EXWM, where s- key bindings don't always work with X buffers; s-tab seems reliable, so a work around is to s-tab to, say, *GNU Emacs*, move the workspace to the desired display and then s-tab back to the X application.

## TODO
There's far too much stuff in init.el itself.
