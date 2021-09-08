(meq/up exwm
    :init/defun* (post-exwm nil (interactive)
                    (unless (get-buffer "Alacritty") (meq/run "alacritty"))
                    (unless (get-buffer "obsidian") (meq/run "obsidian")))
    :hook (exwm-init . post-exwm)
    :use-package-preconfig (fringe :load-emacs-file-preconfig ("fringe")
                        :config
                            ;; (fringe-mode (quote (1 . 1)) nil (fringe))
                            ;; (fringe-mode '(3 . 0))
                            ;; (fringe-mode 'none)
                            ;; (fringe-mode 1)
                            )
    :use-package-postconfig (dmenu)
    :config
        (require 'scroll-bar)
        ;; Adapted From: https://github.com/ch11ng/exwm/blob/master/exwm-config.el#L34
        (require 'exwm-config)
        ;; Set the initial workspace number.
        (unless (get 'exwm-workspace-number 'saved-value)
            (setq exwm-workspace-number 4))
        ;; Make class name the buffer name
        (add-hook 'exwm-update-class-hook
                    (lambda ()
                    (exwm-workspace-rename-buffer exwm-class-name)))
        ;; Global keybindings.
        (unless (get 'exwm-input-global-keys 'saved-value)
            (setq exwm-input-global-keys
                `(
                    ;; 's-{p|`|z}': Enter the exwm-global deino
                    ([?\s-p] . uru)
                    ([?\s-`] . uru)
                    ([?\s-z] . uru)

                    (,(naked "s-tab") . next-buffer)
                    (,(naked "s-<iso-lefttab>") . previous-buffer)
                    (,(naked "M-s-tab") . previous-buffer)

                    ([?\s-q] . (lambda nil (interactive)
                        (unless meq/var/everything-else-initialized (meq/initialize-everything-else))
                        (deino-buffer/body)))

                    ;; 's-N': Switch to certain workspace.
                    ,@(mapcar (lambda (i)
                                `(,(kbd (format "s-%d" i)) .
                                (lambda ()
                                    (interactive)
                                    (exwm-workspace-switch-create ,i))))
                            (number-sequence 0 9)))))
        ;; Line-editing shortcuts
        (unless (get 'exwm-input-simulation-keys 'saved-value)
            (setq exwm-input-simulation-keys
                '(([?\C-b] . [left])
                    ([?\C-f] . [right])
                    ([?\C-p] . [up])
                    ([?\C-n] . [down])
                    ([?\C-a] . [home])
                    ([?\C-e] . [end])
                    ([?\M-v] . [prior])
                    ([?\C-v] . [next])
                    ([?\C-d] . [delete])
                    ([?\C-k] . [S-end delete]))))
        ;; Enable EXWM
        (exwm-enable)
        ;; Configure Ido
        (exwm-config-ido)
        ;; Other configurations
        (exwm-config-misc)

        ;; (exwm-config-default)
        ;; (exwm-enable)

    ;; Adapted From: https://www.reddit.com/r/emacs/comments/8yf6dx/key_chords_in_exwm/
    :gsetq (exwm-manage-force-tiling t)
        ;; (exwm-input-line-mode-passthrough t)

    :demon ((naked "XF86PowerOff") 'deino-exwm/body)
    :which-key-change-ryo ("e" "exwm")
    :deino (deino-exwm nil "e e"
                ("`" nil "cancel")
                ("XF86PowerOff" deino-exwm/power/body "power")
                ("s" deino-exwm/shells/body "shells"))
            (deino-exwm/power (:color blue) "e p"
                ("r" (meq/run "reboot") "reboot")
                ("q" (meq/run "poweroff") "poweroff")
                ("XF86PowerOff" (meq/run "systemctl suspend" "suspend") "suspend"))
            (deino-exwm/shells (:color blue) "e s"
                ("a" (meq/run "alacritty") "alacritty"))
    :uru (exwm-mode t deino-exwm-global (:color blue) "e g"
        ("`" nil "cancel")
        ("c" exwm-input-release-keyboard "char mode")
        ("l" exwm-input-grab-keyboard "line mode")
        ("r" exwm-reset "reset")
        ("w" exwm-workspace-switch "workspace switch")
        ("i" meq/run-interactive "run")
        ("b" deino-buffer/body "buffers")))
