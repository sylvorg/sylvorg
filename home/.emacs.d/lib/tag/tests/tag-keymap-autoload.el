;;; tag-keymap-autoload.el --- For testing "autoloaded" keymaps. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defvar tag-autoload-map (make-sparse-keymap))
(define-key tag-autoload-map "f" #'forward-char)

(provide 'tag-keymap-autoload)
;;; tag-keymap-autoload.el ends here
