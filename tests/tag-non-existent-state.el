;;; tag-non-existent-state.el --- For testing delayed keybindings.
;;; Commentary:
;;; Code:
(require 'evil)

(evil-define-state non-existent
  "I don't normally exist")

(provide 'tag-non-existent-state)
;;; tag-non-existent-state.el ends here
