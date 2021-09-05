;;; alamode.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jeet Ray

;; Author: Jeet Ray <aiern@protonmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Put a description of the package here

;;; Code:

(require 'dash)
(require 'meq)
(require 'deino)
(require 'prime)
(require 'uru)

(defvar meq/var/all-keymaps-map nil)
(defvar meq/var/alamode-aiern-was-on (member "aiern" meq/var/ignored-modal-prefixes))
(defvar meq/var/alamode-evil-was-on (member "evil" meq/var/ignored-modal-prefixes))

;; Adapted From: https://gitlab.com/jjzmajic/cosmoem.el/-/blob/master/cosmoem.el#L83
;;;###autoload
(defun meq/toggle-inner (mode prefix mode-on map &optional use-cosmoem force) (interactive)
    (meq/disable-all-modal-modes nil (not mode-on))
    (if mode-on
        (when force (meq/which-key--show-popup map force))
        (funcall mode 1)
        (with-eval-after-load 'which-key
            (if use-cosmoem (ignore-errors (funcall (meq/inconcat "meq/" prefix "-cosmoem-show")))
                (meq/which-key-show-top-level map)))))

;; Adapted From: https://github.com/emacsorphanage/god-mode/blob/master/god-mode.el#L392
;;;###autoload
(defun meq/execute-with-current-bindings-inner (mode prefix mode-on map &optional use-cosmoem called-interactively)
    (interactive "d")
    (unless mode-on
        (letrec ((caller this-command)
                (buffer (current-buffer))
                (cleanup
                    (lambda ()
                    ;; Perform cleanup in original buffer even if the command
                    ;; switched buffers.
                    (if (buffer-live-p buffer)
                        (with-current-buffer buffer
                            (unwind-protect
                                (setq overriding-terminal-local-map meq/var/alamode-backup-terminal-local-map)
                                (funcall mode -1)
                                (when meq/var/alamode-aiern-was-on (aiern-mode 1))
                                (when meq/var/alamode-evil-was-on (evil-mode 1))
                                (meq/which-key-show-top-level)
                                (remove-hook 'post-command-hook post-hook)))
                        (remove-hook 'post-command-hook post-hook))))
                (kill-transient-map
                    (set-transient-map
                    map 'meq/god-prefix-command-p cleanup))
                (post-hook
                    (lambda ()
                    (unless (and
                            (eq this-command caller)
                            ;; If we've entered the minibuffer, this implies
                            ;; a non-prefix command was run, even if
                            ;; `this-command' has not changed.  For example,
                            ;; `execute-extended-command' behaves this way.
                            (not (window-minibuffer-p)))
                        (funcall kill-transient-map)))))
            (add-hook 'post-command-hook post-hook)
            ;; Pass the current prefix argument along to the next command.
            (setq prefix-arg current-prefix-arg)
            ;; Technically we don't need to activate %p mode since the
            ;; transient keymap is already in place, but it's useful to provide
            ;; a mode line lighter and run any hook functions the user has set
            ;; up.  This could be made configurable in the future.
            (funcall mode 1)
            (with-eval-after-load 'which-key (meq/which-key-show-top-level map))
            (setq meq/var/alamode-backup-terminal-local-map overriding-terminal-local-map)
            (setq deino-enabled-temporarily t
                overriding-terminal-local-map (symbol-value map))
            (when (string= prefix "god")
                (when (meq/fbatp aiern-mode) (setq meq/var/alamode-aiern-was-on t) (aiern-mode -1))
                (when (meq/fbatp evil-mode) (setq meq/var/alamode-evil-was-on t) (evil-mode -1)))
            (message (format "Switched to %s mode for the next command ..." prefix)))))

;;;###autoload
(defdeino+ toggles (:color blue) ("a" meq/toggle-aiern "aiern"))

;;;###autoload
(defdeino+ all-keymaps (:color blue) ("a" meq/aiern-show-top-level "aiern"))

;;;###autoload
(defminorua 4 aiern-mode deino-ala-aiern nil "; m a" ("`" nil "cancel"))

;;;###autoload
(cosmoem-def
    :show-funs #'meq/aiern-cosmoem-show
    :hide-funs #'meq/aiern-cosmoem-hide
    :toggle-funs #'meq/aiern-cosmoem-toggle
    :keymap 'aiern-normal-state-map
    ;; :transient t
)

;;;###autoload
(prime "t a" meq/toggle-aiern-cosmoem "aiern")

;;;###autoload
(defun meq/aiern-cosmoem-toggle nil (interactive) (with-eval-after-load 'aiern))

;;;###autoload
(defun meq/aiern-show-top-level nil (interactive)
    (setq meq/var/all-keymaps-map 'aiern-normal-state-map)
    (with-eval-after-load 'aiern (meq/which-key-show-top-level 'aiern-normal-state-map)))

;;;###autoload
(defun meq/toggle-aiern (ua) (interactive "p")
    (with-eval-after-load 'aiern (if (= ua 4)
        (funcall 'meq/toggle-inner 'aiern-mode "aiern" (meq/fbatp aiern-mode) 'aiern-normal-state-map nil t)
        (funcall 'meq/toggle-inner 'aiern-mode "aiern" (meq/fbatp aiern-mode) 'aiern-normal-state-map))))

;;;###autoload
(defun meq/toggle-aiern-cosmoem (ua) (interactive "p")
    (with-eval-after-load 'aiern (if (= ua 4)
        (funcall 'meq/toggle-inner 'aiern-mode "aiern" (meq/fbatp aiern-mode) 'aiern-normal-state-map t t)
        (funcall 'meq/toggle-inner 'aiern-mode "aiern" (meq/fbatp aiern-mode) 'aiern-normal-state-map t))))

;;;###autoload
(defun meq/aiern-execute-with-current-bindings (&optional called-interactively) (interactive "d")
    (with-eval-after-load 'aiern (funcall 'meq/execute-with-current-bindings-inner 'aiern-mode "aiern" (meq/fbatp aiern-mode) 'aiern-normal-state-map nil called-interactively)))

;;;###autoload
(defun meq/aiern-cosmoem-execute-with-current-bindings (&optional called-interactively) (interactive "d")
    (with-eval-after-load 'aiern (funcall 'meq/execute-with-current-bindings-inner 'aiern-mode "aiern" (meq/fbatp aiern-mode) 'aiern-normal-state-map t called-interactively)))

(with-eval-after-load 'aiern (add-to-list 'meq/var/modal-modes 'aiern-mode) (add-to-list 'meq/var/modal-prefixes "aiern"))

;;;###autoload
(defdeino+ toggles (:color blue) ("r" meq/toggle-ryo "ryo"))

;;;###autoload
(defdeino+ all-keymaps (:color blue) ("r" meq/ryo-show-top-level "ryo"))

;;;###autoload
(defminorua 4 ryo-modal-mode deino-ala-ryo nil "; m r" ("`" nil "cancel"))

;;;###autoload
(cosmoem-def
    :show-funs #'meq/ryo-cosmoem-show
    :hide-funs #'meq/ryo-cosmoem-hide
    :toggle-funs #'meq/ryo-cosmoem-toggle
    :keymap 'ryo-modal-mode-map
    ;; :transient t
)

;;;###autoload
(prime "t r" meq/toggle-ryo-cosmoem "ryo")

;;;###autoload
(defun meq/ryo-cosmoem-toggle nil (interactive) (with-eval-after-load 'ryo-modal))

;;;###autoload
(defun meq/ryo-show-top-level nil (interactive)
    (setq meq/var/all-keymaps-map 'ryo-modal-mode-map)
    (with-eval-after-load 'ryo-modal (meq/which-key-show-top-level 'ryo-modal-mode-map)))

;;;###autoload
(defun meq/toggle-ryo (ua) (interactive "p")
    (with-eval-after-load 'ryo-modal (if (= ua 4)
        (funcall 'meq/toggle-inner 'ryo-modal-mode "ryo" (meq/fbatp ryo-modal-mode) 'ryo-modal-mode-map nil t)
        (funcall 'meq/toggle-inner 'ryo-modal-mode "ryo" (meq/fbatp ryo-modal-mode) 'ryo-modal-mode-map))))

;;;###autoload
(defun meq/toggle-ryo-cosmoem (ua) (interactive "p")
    (with-eval-after-load 'ryo-modal (if (= ua 4)
        (funcall 'meq/toggle-inner 'ryo-modal-mode "ryo" (meq/fbatp ryo-modal-mode) 'ryo-modal-mode-map t t)
        (funcall 'meq/toggle-inner 'ryo-modal-mode "ryo" (meq/fbatp ryo-modal-mode) 'ryo-modal-mode-map t))))

;;;###autoload
(defun meq/ryo-execute-with-current-bindings (&optional called-interactively) (interactive "d")
    (with-eval-after-load 'ryo-modal (funcall 'meq/execute-with-current-bindings-inner 'ryo-modal-mode "ryo" (meq/fbatp ryo-modal-mode) 'ryo-modal-mode-map nil called-interactively)))

;;;###autoload
(defun meq/ryo-cosmoem-execute-with-current-bindings (&optional called-interactively) (interactive "d")
    (with-eval-after-load 'ryo-modal (funcall 'meq/execute-with-current-bindings-inner 'ryo-modal-mode "ryo" (meq/fbatp ryo-modal-mode) 'ryo-modal-mode-map t called-interactively)))

(with-eval-after-load 'ryo-modal (add-to-list 'meq/var/modal-modes 'ryo-modal-mode) (add-to-list 'meq/var/modal-prefixes "ryo"))

;;;###autoload
(defdeino+ toggles (:color blue) ("s" meq/toggle-sorrow "sorrow"))

;;;###autoload
(defdeino+ all-keymaps (:color blue) ("s" meq/sorrow-show-top-level "sorrow"))

;;;###autoload
(defminorua 4 sorrow-mode deino-ala-sorrow nil "; m s" ("`" nil "cancel"))

;;;###autoload
(cosmoem-def
    :show-funs #'meq/sorrow-cosmoem-show
    :hide-funs #'meq/sorrow-cosmoem-hide
    :toggle-funs #'meq/sorrow-cosmoem-toggle
    :keymap 'sorrow-mode-map
    ;; :transient t
)

;;;###autoload
(prime "t s" meq/toggle-sorrow-cosmoem "sorrow")

;;;###autoload
(defun meq/sorrow-cosmoem-toggle nil (interactive) (with-eval-after-load 'sorrow))

;;;###autoload
(defun meq/sorrow-show-top-level nil (interactive)
    (setq meq/var/all-keymaps-map 'sorrow-mode-map)
    (with-eval-after-load 'sorrow (meq/which-key-show-top-level 'sorrow-mode-map)))

;;;###autoload
(defun meq/toggle-sorrow (ua) (interactive "p")
    (with-eval-after-load 'sorrow (if (= ua 4)
        (funcall 'meq/toggle-inner 'sorrow-mode "sorrow" (meq/fbatp sorrow-mode) 'sorrow-mode-map nil t)
        (funcall 'meq/toggle-inner 'sorrow-mode "sorrow" (meq/fbatp sorrow-mode) 'sorrow-mode-map))))

;;;###autoload
(defun meq/toggle-sorrow-cosmoem (ua) (interactive "p")
    (with-eval-after-load 'sorrow (if (= ua 4)
        (funcall 'meq/toggle-inner 'sorrow-mode "sorrow" (meq/fbatp sorrow-mode) 'sorrow-mode-map t t)
        (funcall 'meq/toggle-inner 'sorrow-mode "sorrow" (meq/fbatp sorrow-mode) 'sorrow-mode-map t))))

;;;###autoload
(defun meq/sorrow-execute-with-current-bindings (&optional called-interactively) (interactive "d")
    (with-eval-after-load 'sorrow (funcall 'meq/execute-with-current-bindings-inner 'sorrow-mode "sorrow" (meq/fbatp sorrow-mode) 'sorrow-mode-map nil called-interactively)))

;;;###autoload
(defun meq/sorrow-cosmoem-execute-with-current-bindings (&optional called-interactively) (interactive "d")
    (with-eval-after-load 'sorrow (funcall 'meq/execute-with-current-bindings-inner 'sorrow-mode "sorrow" (meq/fbatp sorrow-mode) 'sorrow-mode-map t called-interactively)))

(with-eval-after-load 'sorrow (add-to-list 'meq/var/modal-modes 'sorrow-mode) (add-to-list 'meq/var/modal-prefixes "sorrow"))

;;;###autoload
(defdeino+ toggles (:color blue) ("e" meq/toggle-evil "evil"))

;;;###autoload
(defdeino+ all-keymaps (:color blue) ("e" meq/evil-show-top-level "evil"))

;;;###autoload
(defminorua 4 evil-mode deino-ala-evil nil "; m e" ("`" nil "cancel"))

;;;###autoload
(cosmoem-def
    :show-funs #'meq/evil-cosmoem-show
    :hide-funs #'meq/evil-cosmoem-hide
    :toggle-funs #'meq/evil-cosmoem-toggle
    :keymap 'evil-normal-state-map
    ;; :transient t
)

;;;###autoload
(prime "t e" meq/toggle-evil-cosmoem "evil")

;;;###autoload
(defun meq/evil-cosmoem-toggle nil (interactive) (with-eval-after-load 'evil))

;;;###autoload
(defun meq/evil-show-top-level nil (interactive)
    (setq meq/var/all-keymaps-map 'evil-normal-state-map)
    (with-eval-after-load 'evil (meq/which-key-show-top-level 'evil-normal-state-map)))

;;;###autoload
(defun meq/toggle-evil (ua) (interactive "p")
    (with-eval-after-load 'evil (if (= ua 4)
        (funcall 'meq/toggle-inner 'evil-mode "evil" (meq/fbatp evil-mode) 'evil-normal-state-map nil t)
        (funcall 'meq/toggle-inner 'evil-mode "evil" (meq/fbatp evil-mode) 'evil-normal-state-map))))

;;;###autoload
(defun meq/toggle-evil-cosmoem (ua) (interactive "p")
    (with-eval-after-load 'evil (if (= ua 4)
        (funcall 'meq/toggle-inner 'evil-mode "evil" (meq/fbatp evil-mode) 'evil-normal-state-map t t)
        (funcall 'meq/toggle-inner 'evil-mode "evil" (meq/fbatp evil-mode) 'evil-normal-state-map t))))

;;;###autoload
(defun meq/evil-execute-with-current-bindings (&optional called-interactively) (interactive "d")
    (with-eval-after-load 'evil (funcall 'meq/execute-with-current-bindings-inner 'evil-mode "evil" (meq/fbatp evil-mode) 'evil-normal-state-map nil called-interactively)))

;;;###autoload
(defun meq/evil-cosmoem-execute-with-current-bindings (&optional called-interactively) (interactive "d")
    (with-eval-after-load 'evil (funcall 'meq/execute-with-current-bindings-inner 'evil-mode "evil" (meq/fbatp evil-mode) 'evil-normal-state-map t called-interactively)))

(with-eval-after-load 'evil (add-to-list 'meq/var/modal-modes 'evil-mode) (add-to-list 'meq/var/modal-prefixes "evil"))

;;;###autoload
(defdeino+ toggles (:color blue) ("g" meq/toggle-god "god"))

;;;###autoload
(defdeino+ all-keymaps (:color blue) ("g" meq/god-show-top-level "god"))

;;;###autoload
(defminorua 4 god-local-mode deino-ala-god nil "; m g" ("`" nil "cancel"))

;;;###autoload
(cosmoem-def
    :show-funs #'meq/god-cosmoem-show
    :hide-funs #'meq/god-cosmoem-hide
    :toggle-funs #'meq/god-cosmoem-toggle
    :keymap 'global-map
    ;; :transient t
)

;;;###autoload
(prime "t g" meq/toggle-god-cosmoem "god")

;;;###autoload
(defun meq/god-cosmoem-toggle nil (interactive) (with-eval-after-load 'god-mode))

;;;###autoload
(defun meq/god-show-top-level nil (interactive)
    (setq meq/var/all-keymaps-map 'global-map)
    (with-eval-after-load 'god-mode (meq/which-key-show-top-level 'global-map)))

;;;###autoload
(defun meq/toggle-god (ua) (interactive "p")
    (with-eval-after-load 'god-mode (if (= ua 4)
        (funcall 'meq/toggle-inner 'god-local-mode "god" (meq/fbatp god-local-mode) 'global-map nil t)
        (funcall 'meq/toggle-inner 'god-local-mode "god" (meq/fbatp god-local-mode) 'global-map))))

;;;###autoload
(defun meq/toggle-god-cosmoem (ua) (interactive "p")
    (with-eval-after-load 'god-mode (if (= ua 4)
        (funcall 'meq/toggle-inner 'god-local-mode "god" (meq/fbatp god-local-mode) 'global-map t t)
        (funcall 'meq/toggle-inner 'god-local-mode "god" (meq/fbatp god-local-mode) 'global-map t))))

;;;###autoload
(defun meq/god-execute-with-current-bindings (&optional called-interactively) (interactive "d")
    (with-eval-after-load 'god-mode (funcall 'meq/execute-with-current-bindings-inner 'god-local-mode "god" (meq/fbatp god-local-mode) 'global-map nil called-interactively)))

;;;###autoload
(defun meq/god-cosmoem-execute-with-current-bindings (&optional called-interactively) (interactive "d")
    (with-eval-after-load 'god-mode (funcall 'meq/execute-with-current-bindings-inner 'god-local-mode "god" (meq/fbatp god-local-mode) 'global-map t called-interactively)))

(with-eval-after-load 'god-mode (add-to-list 'meq/var/modal-modes 'god-local-mode) (add-to-list 'meq/var/modal-prefixes "god"))

;;;###autoload
(defdeino+ toggles (:color blue) ("x" meq/toggle-xah "xah"))

;;;###autoload
(defdeino+ all-keymaps (:color blue) ("x" meq/xah-show-top-level "xah"))

;;;###autoload
(defminorua 4 xah-fly-keys deino-ala-xah nil "; m x" ("`" nil "cancel"))

;;;###autoload
(cosmoem-def
    :show-funs #'meq/xah-cosmoem-show
    :hide-funs #'meq/xah-cosmoem-hide
    :toggle-funs #'meq/xah-cosmoem-toggle
    :keymap 'xah-fly-command-map
    ;; :transient t
)

;;;###autoload
(prime "t x" meq/toggle-xah-cosmoem "xah")

;;;###autoload
(defun meq/xah-cosmoem-toggle nil (interactive) (with-eval-after-load 'xah-fly-keys))

;;;###autoload
(defun meq/xah-show-top-level nil (interactive)
    (setq meq/var/all-keymaps-map 'xah-fly-command-map)
    (with-eval-after-load 'xah-fly-keys (meq/which-key-show-top-level 'xah-fly-command-map)))

;;;###autoload
(defun meq/toggle-xah (ua) (interactive "p")
    (with-eval-after-load 'xah-fly-keys (if (= ua 4)
        (funcall 'meq/toggle-inner 'xah-fly-keys "xah" (meq/fbatp xah-fly-keys) 'xah-fly-command-map nil t)
        (funcall 'meq/toggle-inner 'xah-fly-keys "xah" (meq/fbatp xah-fly-keys) 'xah-fly-command-map))))

;;;###autoload
(defun meq/toggle-xah-cosmoem (ua) (interactive "p")
    (with-eval-after-load 'xah-fly-keys (if (= ua 4)
        (funcall 'meq/toggle-inner 'xah-fly-keys "xah" (meq/fbatp xah-fly-keys) 'xah-fly-command-map t t)
        (funcall 'meq/toggle-inner 'xah-fly-keys "xah" (meq/fbatp xah-fly-keys) 'xah-fly-command-map t))))

;;;###autoload
(defun meq/xah-execute-with-current-bindings (&optional called-interactively) (interactive "d")
    (with-eval-after-load 'xah-fly-keys (funcall 'meq/execute-with-current-bindings-inner 'xah-fly-keys "xah" (meq/fbatp xah-fly-keys) 'xah-fly-command-map nil called-interactively)))

;;;###autoload
(defun meq/xah-cosmoem-execute-with-current-bindings (&optional called-interactively) (interactive "d")
    (with-eval-after-load 'xah-fly-keys (funcall 'meq/execute-with-current-bindings-inner 'xah-fly-keys "xah" (meq/fbatp xah-fly-keys) 'xah-fly-command-map t called-interactively)))

(with-eval-after-load 'xah-fly-keys (add-to-list 'meq/var/modal-modes 'xah-fly-keys) (add-to-list 'meq/var/modal-prefixes "xah"))

;;;###autoload
(defdeino+ toggles (:color blue) ("o" meq/toggle-objed "objed"))

;;;###autoload
(defdeino+ all-keymaps (:color blue) ("o" meq/objed-show-top-level "objed"))

;;;###autoload
(defminorua 4 objed-mode deino-ala-objed nil "; m o" ("`" nil "cancel"))

;;;###autoload
(cosmoem-def
    :show-funs #'meq/objed-cosmoem-show
    :hide-funs #'meq/objed-cosmoem-hide
    :toggle-funs #'meq/objed-cosmoem-toggle
    :keymap 'objed-map
    ;; :transient t
)

;;;###autoload
(prime "t o" meq/toggle-objed-cosmoem "objed")

;;;###autoload
(defun meq/objed-cosmoem-toggle nil (interactive) (with-eval-after-load 'objed))

;;;###autoload
(defun meq/objed-show-top-level nil (interactive)
    (setq meq/var/all-keymaps-map 'objed-map)
    (with-eval-after-load 'objed (meq/which-key-show-top-level 'objed-map)))

;;;###autoload
(defun meq/toggle-objed (ua) (interactive "p")
    (with-eval-after-load 'objed (if (= ua 4)
        (funcall 'meq/toggle-inner 'objed-mode "objed" (meq/fbatp objed-mode) 'objed-map nil t)
        (funcall 'meq/toggle-inner 'objed-mode "objed" (meq/fbatp objed-mode) 'objed-map))))

;;;###autoload
(defun meq/toggle-objed-cosmoem (ua) (interactive "p")
    (with-eval-after-load 'objed (if (= ua 4)
        (funcall 'meq/toggle-inner 'objed-mode "objed" (meq/fbatp objed-mode) 'objed-map t t)
        (funcall 'meq/toggle-inner 'objed-mode "objed" (meq/fbatp objed-mode) 'objed-map t))))

;;;###autoload
(defun meq/objed-execute-with-current-bindings (&optional called-interactively) (interactive "d")
    (with-eval-after-load 'objed (funcall 'meq/execute-with-current-bindings-inner 'objed-mode "objed" (meq/fbatp objed-mode) 'objed-map nil called-interactively)))

;;;###autoload
(defun meq/objed-cosmoem-execute-with-current-bindings (&optional called-interactively) (interactive "d")
    (with-eval-after-load 'objed (funcall 'meq/execute-with-current-bindings-inner 'objed-mode "objed" (meq/fbatp objed-mode) 'objed-map t called-interactively)))

(with-eval-after-load 'objed (add-to-list 'meq/var/modal-modes 'objed-mode) (add-to-list 'meq/var/modal-prefixes "objed"))

;;;###autoload
(defdeino+ toggles (:color blue) ("m" meq/toggle-modalka "modalka"))

;;;###autoload
(defdeino+ all-keymaps (:color blue) ("m" meq/modalka-show-top-level "modalka"))

;;;###autoload
(defminorua 4 modalka-mode deino-ala-modalka nil "; m m" ("`" nil "cancel"))

;;;###autoload
(cosmoem-def
    :show-funs #'meq/modalka-cosmoem-show
    :hide-funs #'meq/modalka-cosmoem-hide
    :toggle-funs #'meq/modalka-cosmoem-toggle
    :keymap 'modalka-mode-map
    ;; :transient t
)

;;;###autoload
(prime "t m" meq/toggle-modalka-cosmoem "modalka")

;;;###autoload
(defun meq/modalka-cosmoem-toggle nil (interactive) (with-eval-after-load 'modalka))

;;;###autoload
(defun meq/modalka-show-top-level nil (interactive)
    (setq meq/var/all-keymaps-map 'modalka-mode-map)
    (with-eval-after-load 'modalka (meq/which-key-show-top-level 'modalka-mode-map)))

;;;###autoload
(defun meq/toggle-modalka (ua) (interactive "p")
    (with-eval-after-load 'modalka (if (= ua 4)
        (funcall 'meq/toggle-inner 'modalka-mode "modalka" (meq/fbatp modalka-mode) 'modalka-mode-map nil t)
        (funcall 'meq/toggle-inner 'modalka-mode "modalka" (meq/fbatp modalka-mode) 'modalka-mode-map))))

;;;###autoload
(defun meq/toggle-modalka-cosmoem (ua) (interactive "p")
    (with-eval-after-load 'modalka (if (= ua 4)
        (funcall 'meq/toggle-inner 'modalka-mode "modalka" (meq/fbatp modalka-mode) 'modalka-mode-map t t)
        (funcall 'meq/toggle-inner 'modalka-mode "modalka" (meq/fbatp modalka-mode) 'modalka-mode-map t))))

;;;###autoload
(defun meq/modalka-execute-with-current-bindings (&optional called-interactively) (interactive "d")
    (with-eval-after-load 'modalka (funcall 'meq/execute-with-current-bindings-inner 'modalka-mode "modalka" (meq/fbatp modalka-mode) 'modalka-mode-map nil called-interactively)))

;;;###autoload
(defun meq/modalka-cosmoem-execute-with-current-bindings (&optional called-interactively) (interactive "d")
    (with-eval-after-load 'modalka (funcall 'meq/execute-with-current-bindings-inner 'modalka-mode "modalka" (meq/fbatp modalka-mode) 'modalka-mode-map t called-interactively)))

(with-eval-after-load 'modalka (add-to-list 'meq/var/modal-modes 'modalka-mode) (add-to-list 'meq/var/modal-prefixes "modalka"))

;;;###autoload
(defdeino+ toggles (:color blue) ("l" meq/toggle-lispy "lispy"))

;;;###autoload
(defdeino+ all-keymaps (:color blue) ("l" meq/lispy-show-top-level "lispy"))

;;;###autoload
(defminorua 4 lispy-mode deino-ala-lispy nil "; m l" ("`" nil "cancel"))

;;;###autoload
(cosmoem-def
    :show-funs #'meq/lispy-cosmoem-show
    :hide-funs #'meq/lispy-cosmoem-hide
    :toggle-funs #'meq/lispy-cosmoem-toggle
    :keymap 'lispy-mode-map
    ;; :transient t
)

;;;###autoload
(prime "t l" meq/toggle-lispy-cosmoem "lispy")

;;;###autoload
(defun meq/lispy-cosmoem-toggle nil (interactive) (with-eval-after-load 'lispy))

;;;###autoload
(defun meq/lispy-show-top-level nil (interactive)
    (setq meq/var/all-keymaps-map 'lispy-mode-map)
    (with-eval-after-load 'lispy (meq/which-key-show-top-level 'lispy-mode-map)))

;;;###autoload
(defun meq/toggle-lispy (ua) (interactive "p")
    (with-eval-after-load 'lispy (if (= ua 4)
        (funcall 'meq/toggle-inner 'lispy-mode "lispy" (meq/fbatp lispy-mode) 'lispy-mode-map nil t)
        (funcall 'meq/toggle-inner 'lispy-mode "lispy" (meq/fbatp lispy-mode) 'lispy-mode-map))))

;;;###autoload
(defun meq/toggle-lispy-cosmoem (ua) (interactive "p")
    (with-eval-after-load 'lispy (if (= ua 4)
        (funcall 'meq/toggle-inner 'lispy-mode "lispy" (meq/fbatp lispy-mode) 'lispy-mode-map t t)
        (funcall 'meq/toggle-inner 'lispy-mode "lispy" (meq/fbatp lispy-mode) 'lispy-mode-map t))))

;;;###autoload
(defun meq/lispy-execute-with-current-bindings (&optional called-interactively) (interactive "d")
    (with-eval-after-load 'lispy (funcall 'meq/execute-with-current-bindings-inner 'lispy-mode "lispy" (meq/fbatp lispy-mode) 'lispy-mode-map nil called-interactively)))

;;;###autoload
(defun meq/lispy-cosmoem-execute-with-current-bindings (&optional called-interactively) (interactive "d")
    (with-eval-after-load 'lispy (funcall 'meq/execute-with-current-bindings-inner 'lispy-mode "lispy" (meq/fbatp lispy-mode) 'lispy-mode-map t called-interactively)))

(with-eval-after-load 'lispy (add-to-list 'meq/var/modal-modes 'lispy-mode) (add-to-list 'meq/var/modal-prefixes "lispy"))

(provide 'alamode)
;;; alamode.el ends here
