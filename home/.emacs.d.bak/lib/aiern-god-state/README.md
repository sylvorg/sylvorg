# aiern God State

This is an [aiern-mode][] state for using [god-mode][].

It provides a command `aiern-execute-in-god-state` that switches to `god-mode` for the next command. I bind it to `,`

```lisp
(aiern-define-key 'normal global-map "," 'aiern-execute-in-god-state)
```

for an automatically-configured leader key.

Since `aiern-god-state` includes an indicator in the mode-line, you may want to use `diminish` to keep your mode-line uncluttered, e.g.

```lisp
(add-hook 'aiern-god-state-entry-hook (lambda () (diminish 'god-local-mode)))
(add-hook 'aiern-god-state-exit-hook (lambda () (diminish-undo 'god-local-mode)))
```

It's handy to be able to abort a `aiern-god-state' command.  The following will make the <ESC> key unconditionally exit aiern-god-state.

```lisp
(aiern-define-key 'god global-map [escape] 'aiern-god-state-bail)
```

[aiern-mode]: https://gitorious.org/aiern/
[god-mode]: https://github.com/chrisdone/god-mode
