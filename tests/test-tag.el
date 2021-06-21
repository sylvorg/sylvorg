;;; test-tag.el --- Tests for tag.el. -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Tests for tag.el

;; Conventions:
;; - `tag-temp-map' is used for defs that will be tested by lookup
;; - `tag-test-mode-map' is used for defs whose behavior will be tested
;; - `tag-temp-map' should be empty at the beginning/end of each spec; it
;;   should be guaranteed that any lookup initially returns nil
;; - `tag-test-mode-map' should never be set to (make-sparse-keymap); doing
;;   this breaks a lot tests for no clear reason (evil-normalize-keymaps does
;;   not help); therefore, tests should not rely on `tag-test-mode-map'
;;   being empty
;; - global keymaps such as `evil-normal-state-map' should never be altered;
;;   they should be let bound when key definitions take place or the key
;;   definitions should be reverted
;; - `tag-define-key' args should appear starting on the next line and in
;;   the order that they appear in the definition
;; - Only `evil-local-mode' should be turned on/off within `tag-with' (so
;;   the global value isn't affected when testing in Emacs directly)

;; TODO
;; - way to set the current global map
;; - way to wrap all specs with let automatically
;; - test :major-modes (and with aliases; should work now but previously didn't)
;; - test :definer
;; - test rest of which-key keywords
;; - test 'global
;; - test all local keywords in extended definitions
;; - test all def types in extended definitions
;; - implement other xits
;; - implement other todos

;;; Code:
;; * Setup
(when (require 'undercover nil t)
  (undercover "*.el"
              (:exclude "test-*.el")
              (:report-type :codecov)
              (:send-report nil)))

(require 'buttercup)
(require 'tag)
(require 'evil)
(require 'which-key)
(require 'use-package)

(push "tests/" load-path)

(setq evil-want-change-word-to-end nil
      evil-move-cursor-back nil
      evil-move-beyond-eol t)

(defvar tag-test-mode-map (make-sparse-keymap))

(define-minor-mode tag-test-mode
  "A minor mode for tag.el tests."
  :lighter ""
  :keymap tag-test-mode-map
  (evil-normalize-keymaps))

(defvar tag-temp-map (make-sparse-keymap))
(push '(temp . tag-temp-map) tag-keymap-aliases)

;; * Testing Helpers
(defmacro tag-with (in &rest body)
  "This is `lispy-with' modified for tag.
Note that | is considered to be \"on\" a character, meaning that it is included
in a visual selection. ~ on the other hand is not considered to be on a
character, so when it represents the region end, the character after it is not
considered as part of the region."
  (declare (indent 1))
  `(let ((temp-buffer (generate-new-buffer " *temp*"))
         (tag--simulate-next-as-is t))
     (save-window-excursion
       (unwind-protect
           (progn
             (switch-to-buffer temp-buffer)
             (emacs-lisp-mode)
             (tag-test-mode)
             (transient-mark-mode)
             (evil-local-mode)
             (insert ,in)
             (goto-char (point-min))
             (when (search-forward "~" nil t)
               (backward-delete-char 1)
               (set-mark (point)))
             (goto-char (point-max))
             (search-backward "|")
             (delete-char 1)
             (setq current-prefix-arg nil)
             ,@(mapcar (lambda (x)
                         (if (stringp x)
                             `(evil-execute-macro 1 (kbd ,x))
                           x))
                       body)
             (insert "|")
             (when (region-active-p)
               (exchange-point-and-mark)
               ;; because not considering ~ as "on" like |
               (when (= (point) (region-end))
                 (forward-char))
               (insert "~"))
             (buffer-substring-no-properties
              (point-min)
              (point-max)))
         (and (buffer-name temp-buffer)
              (kill-buffer temp-buffer))))))

(defun tag-test-keys (states keymaps &rest maps)
  "Look in the keymaps for STATES and KEYMAPS for MAPS.
Return t if successful or a cons corresponding to the failed key and def."
  (declare (indent 2))
  (unless (listp states)
    (setq states (list states)))
  (unless (and (listp keymaps)
               (not (keymapp keymaps)))
    (setq keymaps (list keymaps)))
  (let (actual-keymaps)
    (if states
        (dolist (state states)
          (dolist (keymap keymaps)
            (if state
                (push (evil-get-auxiliary-keymap keymap state t)
                      actual-keymaps)
              (setq actual-keymaps (append actual-keymaps keymaps)))))
      (setq actual-keymaps keymaps))
    (dolist (keymap actual-keymaps)
      (while maps
        (let* ((key (kbd (pop maps)))
               (def (pop maps))
               (actual-def (lookup-key keymap key)))
          (unless (equal actual-def
                         (if (stringp def)
                             (kbd def)
                           def))
            (buttercup-fail "Key \"%s\" is not bound to \"%s\" but to \"%s\""
                            (key-description key)
                            def
                            actual-def))))))
  t)

;; * Key Definition
;; ** Main Definer
(describe "tag-define-key"
  ;; NOTE: this applies to all specs even when nesting describes
  (after-each
    (setq tag-temp-map (make-sparse-keymap)))
  (it "should define/undefine keys in (current-global-map) by default"
    (tag-define-key "a" #'a)
    (expect (tag-test-keys nil (current-global-map)
              "a" #'a))
    (tag-define-key "a" nil)
    (expect (tag-test-keys nil (current-global-map)
              "a" nil))
    (tag-define-key "a" #'self-insert-command))
  (it "should allow defining/undefining keys in a specified keymap"
    (tag-define-key
     :keymaps 'tag-temp-map
     "a" #'a)
    (expect (tag-test-keys nil tag-temp-map
              "a" #'a))
    (tag-define-key
     :keymaps 'tag-temp-map
     "a" nil)
    (expect (tag-test-keys nil tag-temp-map
              "a" nil)))
  (it "should allow defining/undefining keys in a specified state and keymap"
    (tag-define-key
     :states 'normal
     :keymaps 'tag-temp-map
     "a" #'a)
    (expect (tag-test-keys 'normal tag-temp-map
              "a" #'a))
    (tag-define-key
     :keymaps 'tag-temp-map
     :states 'normal
     "a" nil)
    (expect (tag-test-keys 'normal tag-temp-map
              "a" nil)))
  (it "should use the evil global keymaps for :keymaps 'global :states ..."
    (let ((evil-normal-state-map (make-sparse-keymap)))
      (tag-define-key
       :states 'normal
       "a" 'a)
      (expect (tag-test-keys 'normal (current-global-map)
                "a" nil))
      (expect (tag-test-keys nil evil-normal-state-map
                "a" #'a))))
  (xit "should allow defining/undefining keys in multiple states and keymaps")
  (it "should support keymap/state aliasing"
    (let ((evil-normal-state-map (make-sparse-keymap)))
      (tag-define-key
       :keymaps 'normal
       "a" #'a)
      (tag-define-key
       :keymaps 'n
       "b" #'b)
      (expect (tag-test-keys nil evil-normal-state-map
                "a" #'a
                "b" #'b)))
    (tag-define-key
     :states 'n
     :keymaps 'tag-temp-map
     "a" #'a)
    (expect (tag-test-keys 'normal tag-temp-map
              "a" #'a)))
  (it "should allow defining local keybindings"
    ;; Note: local is not a keymap alias (it is special and turns on
    ;; `tag-override-local-mode')
    (expect (tag-with "fo|o"
              (evil-local-mode -1)
              (tag-define-key
               :keymaps 'local
               "C-a" #'backward-char)
              "C-a")
            :to-equal "f|oo")
    ;; should not affect other buffers
    (expect (tag-with "fo|o"
              (evil-local-mode -1)
              (tag-override-local-mode)
              "C-a")
            :to-equal "|foo")
    (tag-define-key
     :states 'normal
     :keymaps 'tag-test-mode-map
     "a" #'forward-char)
    (expect (tag-with "fo|o"
              (tag-define-key
               :states 'normal
               :keymaps 'local
               "a" #'backward-char)
              "a")
            :to-equal "f|oo"))
  (describe "should support all valid define-key defs"
    ;; NOTE: See key lookup in the manual and the help for define-key
    (it "including nil (tested previously)")
    (it "including symbols/commands (tested previously)")
    (it "including strings and vectors (arrays)"
      (tag-define-key
       :keymaps 'tag-temp-map
       "a" "a")
      (expect (tag-test-keys nil tag-temp-map
                "a" "a"))
      (tag-define-key
       :keymaps 'tag-temp-map
       "test" [116 101 115 116])
      (expect (tag-test-keys nil tag-temp-map
                "test" [116 101 115 116])))
    (it "including keymaps (list)"
      (tag-define-key
       :keymaps 'tag-temp-map
       "t" tag-temp-map)
      (expect (tag-test-keys nil tag-temp-map
                "t" tag-temp-map)))
    (it "including interactive lambdas (list)"
      (tag-define-key
       :keymaps 'tag-temp-map
       "l" (lambda () (interactive) (forward-char 1)))
      (expect (tag-test-keys nil tag-temp-map
                "l" (lambda () (interactive) (forward-char 1)))))
    (it "including extended menu items (list)"
      (tag-define-key
       :keymaps 'tag-temp-map
       "m" '(menu-item "" nil
                       :filter (lambda (&optional _)
                                 (when t
                                   'next-line))))
      (expect (tag-test-keys nil tag-temp-map
                "m" #'next-line)))
    (xit "including conses of the form (STRING . DEFN)")
    (xit "including conses of the form (MAP . CHAR)"))
  (it "should automatically wrap keys with (kbd) (and work for vector keys)"
    (tag-define-key
     :keymaps 'tag-temp-map
     "SPC a" #'spc-a
     [remap kill-line] #'my-kill-line
     [24 108] #'C-x--l
     [?\C-x ?a] #'C-x--a)
    (expect (tag-test-keys nil tag-temp-map
              "SPC a" #'spc-a
              "<remap> <kill-line>" #'my-kill-line
              "C-x l" #'C-x--l
              "C-x a" #'C-x--a)))
  (it "should automatically wrap string definitions with (kbd)"
    (tag-define-key
     :keymaps 'tag-temp-map
     "SPC b" "C-x b")
    (expect (tag-test-keys nil tag-temp-map
              "SPC b" "C-x b")))
  (describe "should support prefixes"
    (it "in the basic case"
      (tag-define-key
       :keymaps 'tag-temp-map
       :prefix ","
       "a" #'comma-a)
      (tag-define-key
       :states '(normal insert)
       :keymaps 'tag-temp-map
       :prefix ","
       "a" #'comma-a)
      (expect (tag-test-keys '(insert normal nil) tag-temp-map
                ",a" #'comma-a)))
    (it "and binding them with \"\""
      (tag-define-key
       :keymaps 'tag-temp-map
       "," #'bound)
      (tag-define-key
       :keymaps 'tag-temp-map
       :prefix ","
       ;; unbind first
       "" nil
       "a" #'comma-a)
      (expect (tag-test-keys nil tag-temp-map
                ",a" #'comma-a)))
    (describe "and creating"
      (it "prefix commands and keymaps"
        (tag-define-key
         :keymaps 'tag-temp-map
         :prefix ","
         :prefix-command 'my-comma-prefix
         :prefix-map 'my-comma-prefix-map
         :prefix-name "my comma prefix map"
         "b" #'comma-b)
        ;; the prefix should be bound to the prefix keymap
        (tag-define-key
         :keymaps 'my-comma-prefix-map
         "c" #'comma-c)
        (expect (fboundp 'my-comma-prefix))
        (expect (and (boundp 'my-comma-prefix-map)
                     (keymapp my-comma-prefix-map)))
        (expect (keymap-prompt my-comma-prefix-map)
                :to-equal "my comma prefix map")
        ;; previously created keymaps should not be cleared
        (tag-define-key
         :keymaps 'tag-temp-map
         :prefix ","
         :prefix-command 'my-comma-prefix
         :prefix-map 'my-comma-prefix-map
         :prefix-name "my comma prefix map")
        (expect (tag-test-keys nil tag-temp-map
                  ",b" #'comma-b
                  ",c" #'comma-c))
        ;; cleanup
        (fmakunbound 'my-comma-prefix)
        (makunbound 'my-comma-prefix-map)
        (expect (not (or (boundp 'my-comma-prefix-map)
                         (fboundp 'my-comma-prefix)))))
      (it "just prefix keymaps"
        (tag-define-key
         :keymaps 'tag-temp-map
         :prefix ","
         :prefix-map 'my-comma-prefix-map
         :prefix-name "my comma prefix map"
         "b" #'comma-b)
        (expect (not (fboundp 'my-comma-prefix)))
        (expect (and (boundp 'my-comma-prefix-map)
                     (keymapp my-comma-prefix-map)))
        (expect (keymap-prompt my-comma-prefix-map)
                :to-equal "my comma prefix map")
        ;; the prefix should be bound to the prefix keymap
        (tag-define-key
         :keymaps 'my-comma-prefix-map
         "c" #'comma-c)
        ;; previously created keymaps should not be cleared
        (tag-define-key
         :keymaps 'tag-temp-map
         :prefix ","
         :prefix-map 'my-comma-prefix-map
         :prefix-name "my comma prefix map")
        (expect (tag-test-keys nil tag-temp-map
                  ",b" #'comma-b
                  ",c" #'comma-c))
        ;; cleanup
        (makunbound 'my-comma-prefix-map)
        (expect (not (boundp 'my-comma-prefix-map)))))
    (it "with a vector key and/or vector prefix"
      (tag-define-key
       :keymaps 'tag-temp-map
       :prefix ","
       [?c] #'comma-c)
      (tag-define-key
       :prefix [?,]
       :keymaps 'tag-temp-map
       "d" #'comma-d)
      (tag-define-key
       :prefix [?,]
       :keymaps 'tag-temp-map
       [?e] #'comma-e)
      (expect (tag-test-keys nil tag-temp-map
                ",c" #'comma-c
                ",d" #'comma-d
                ",e" #'comma-e)))
    (it "with :non-normal-prefix, :global-prefix, and :infix"
      (tag-define-key
       :states '(normal insert emacs)
       :keymaps 'tag-temp-map
       :prefix ","
       :non-normal-prefix "M-,"
       :global-prefix "C-,"
       :infix "f"
       "g" #'comma-f-g)
      (expect (tag-test-keys 'normal tag-temp-map
                "M-," nil
                "C-, f g" #'comma-f-g
                ",fg" #'comma-f-g))
      (expect (tag-test-keys '(insert emacs) tag-temp-map
                "," nil
                "C-, f g" #'comma-f-g
                "M-, f g" #'comma-f-g))
      ;; case where non-normal maps but no non-normal-prefix
      (tag-define-key
       :states 'insert
       :keymaps 'tag-temp-map
       :global-prefix "C-,"
       "a" #'comma-a)
      (expect (tag-test-keys 'insert tag-temp-map
                "C-, a" #'comma-a))
      (let ((evil-normal-state-map (make-sparse-keymap))
            (evil-insert-state-map (make-sparse-keymap)))
        ;; TODO with just prefix and global and just prefix and non-normal
        (tag-define-key
         :keymaps '(evil-normal-state-map
                    evil-insert-state-map)
         :prefix ","
         :non-normal-prefix "M-,"
         :global-prefix "C-,"
         :infix "f"
         "g" #'comma-f-g)
        (expect (tag-test-keys nil evil-normal-state-map
                  "M-," nil
                  "C-, f g" #'comma-f-g
                  ",fg" #'comma-f-g))
        (expect (tag-test-keys nil evil-insert-state-map
                  "," nil
                  "C-, f g" #'comma-f-g
                  "M-, f g" #'comma-f-g)))))
  (it "should support creating and binding in prefix keymaps with no prefix"
    (tag-define-key
     :prefix-command 'tag-unbound-prefix-map
     "a" #'a)
    (expect (tag-test-keys nil tag-unbound-prefix-map
              "a" #'a))
    (tag-define-key
     :prefix-map 'tag-unbound-prefix2-map
     "a" #'a)
    (expect (tag-test-keys nil tag-unbound-prefix2-map
              "a" #'a))
    ;; cleanup
    (makunbound 'tag-unbound-prefix-map)
    (makunbound 'tag-unbound-prefix2-map)
    (expect (not (boundp 'tag-unbound-prefix-map)))
    (expect (not (boundp 'tag-unbound-prefix2-map))))
  (it "should support predicates"
    (tag-define-key
     :keymaps 'tag-temp-map
     "a" #'beginning-of-line)
    (tag-define-key
     :states 'normal
     :keymaps 'tag-test-mode-map
     :predicate '(looking-at "\\'")
     "<left>" tag-temp-map
     "<right>" #'beginning-of-buffer)
    (expect (tag-with "a |b c" "<right>")
            :to-equal "a b| c")
    (expect (tag-with "a b c|" "<right>")
            :to-equal "|a b c")
    (expect (tag-with "a |b c" "<left>")
            :to-equal "a| b c")
    (expect (tag-with "a b c|" "<left> a")
            :to-equal "|a b c"))
  (it "should support local predicates"
    (tag-define-key
     :states 'normal
     :keymaps 'tag-test-mode-map
     :predicate '(not t)
     "<right>" '(beginning-of-buffer :predicate (looking-at "\\'")))
    (expect (tag-with "a |b c" "<right>")
            :to-equal "a b| c")
    (expect (tag-with "a b c|" "<right>")
            :to-equal "|a b c"))
  (describe "should support extended definitions"
    (it "including support for :ignore, which should not bind the key"
      (tag-define-key
       :keymaps 'tag-temp-map
       "a" '(:ignore))
      (expect (tag-test-keys nil tag-temp-map
                "a" nil)))
    (it "including support for :prefix-command, :prefix-map, and :prefix-name"
      (tag-define-key
       :keymaps 'tag-temp-map
       "a" '(:prefix-command a-command :prefix-map a-map :prefix-name "a map")
       "ab" #'a-b)
      (expect (fboundp 'a-command))
      (expect (and (boundp 'a-map)
                   (keymapp a-map)))
      (expect (keymap-prompt a-map)
              :to-equal "a map")
      (expect (tag-test-keys nil tag-temp-map
                "ab" #'a-b)))
    (it "including support for \"autoloading\" keymaps with :keymap"
      ;; TODO consider splitting this into multiple specs
      ;; should error without :package
      (expect (tag-define-key
               :keymaps 'tag-temp-map
               "," '(:keymap tag-autoload-map))
              :to-throw 'error)
      (tag-define-key
       :states 'normal
       :keymaps 'tag-test-mode-map
       "," '(:keymap tag-autoload-map :package does-not-exist))
      ;; should error when non-existent package is specified
      (expect (tag-with ",") :to-throw)
      ;; package properly specified
      (tag-define-key
       :states 'normal
       :keymaps 'tag-test-mode-map
       :package 'this-has-lower-precedence
       "," '(:keymap tag-autoload-map
             :package tag-keymap-autoload)
       "SPC" '(:keymap does-not-exist-map
               :package tag-keymap-autoload))
      ;; not yet bound
      (expect (tag-test-keys 'normal tag-test-mode-map
                ",f" #'forward-char)
              :to-throw)
      (expect (tag-with "a |b c" ",f")
              :to-equal "a b| c")
      (expect (tag-test-keys 'normal tag-test-mode-map
                ",f" #'forward-char))
      ;; should error if keymap doesn't exist in package
      (expect (tag--simulate-keys nil "SPC" 'normal tag-test-mode-map)
              :to-throw))
    (it "including support for :which-key"
      (defvar tag-command-map (make-sparse-keymap))
      (setq which-key-replacement-alist nil)
      (tag-define-key
       :keymaps 'tag-temp-map
       :prefix ","
       ;; basic replacement
       "" '(:ignore t :which-key "tag prefix")
       "f" '(:ignore t :which-key "file prefix")
       ;; should be quoted
       "[" '(:ignore t :which-key "open square bracket")
       ;; use a cons as a replacement
       "g" '(:ignore t :wk ("g-key" . "git prefix"))
       ;; toggle lispy; use a function as a replacement to show if currently on
       "l" '(lispy-mode :wk my-lispy-which-key-display)
       ;; for a keymap, only the keys will be matched;
       ;; :no-match-binding is not necessary
       "G" '(:keymap tag-command-map :wk "tag prefix")
       "z" '(no-display-command :wk t))
      (tag-define-key
       :keymaps 'tag-temp-map
       :wk-full-keys nil
       "A" '(:prefix-command apropos-prefix-map :which-key "apropos"))
      ;; should behave the same as previous
      (tag-define-key
       :keymaps 'tag-temp-map
       :prefix "A"
       :prefix-command 'apropos-prefix-map
       :wk-full-keys nil
       "" '(:ignore t :which-key "new apropos description"))
      (expect which-key-replacement-alist
              :to-equal '((("A\\'" . "apropos-prefix-map")
                           nil . "new apropos description")
                          (("A\\'" . "apropos-prefix-map")
                           nil . "apropos")
                          (("\\`, z\\'" . "no-display-command")
                           . t)
                          (("\\`, G\\'")
                           nil . "tag prefix")
                          (("\\`, l\\'" . "lispy-mode")
                           . my-lispy-which-key-display)
                          (("\\`, g\\'")
                           "g-key" . "git prefix")
                          (("\\`, \\[\\'")
                           nil . "open square bracket")
                          (("\\`, f\\'")
                           nil . "file prefix")
                          (("\\`,\\'")
                           nil . "tag prefix"))))
    (describe "including support for :properties, :repeat, and :jump"
      (it "globally"
        (tag-define-key
         :keymaps 'tag-temp-map
         :properties '(:repeat t :jump t)
         "a" 'tag-should-repeat-and-jump)
        (expect (evil-get-command-properties 'tag-should-repeat-and-jump)
                :to-equal '(:repeat t :jump t))
        (evil-set-command-properties 'tag-should-repeat-and-jump)
        (expect (evil-get-command-properties 'tag-should-repeat-and-jump)
                :to-be nil))
      (it "locally"
        (tag-define-key
         :keymaps 'tag-temp-map
         "a" '(tag-should-repeat-and-jump :properties (:repeat t :jump t)))
        (expect (evil-get-command-properties 'tag-should-repeat-and-jump)
                :to-equal '(:repeat t :jump t))
        (evil-set-command-properties 'tag-should-repeat-and-jump)
        (expect (evil-get-command-properties 'tag-should-repeat-and-jump)
                :to-be nil)
        (tag-define-key
         :keymaps 'tag-temp-map
         "a" '(tag-should-repeat-and-jump :repeat t :jump t))
        (expect (evil-get-command-properties
                 'tag-should-repeat-and-jump)
                :to-equal '(:repeat t :jump t))
        (evil-set-command-properties 'tag-should-repeat-and-jump)
        (expect (evil-get-command-properties 'tag-should-repeat-and-jump)
                :to-be nil))
      (it "locally with a global default"
        (tag-define-key
         :keymaps 'tag-temp-map
         :properties '(:repeat nil :jump nil)
         "a" '(tag-should-repeat-and-jump :properties (:repeat t :jump t)))
        (expect (evil-get-command-properties 'tag-should-repeat-and-jump)
                :to-equal '(:repeat t :jump t))
        (evil-set-command-properties 'tag-should-repeat-and-jump)
        (expect (evil-get-command-properties 'tag-should-repeat-and-jump)
                :to-be nil)
        (tag-define-key
         :keymaps 'tag-temp-map
         :repeat nil
         :jump nil
         "a" '(tag-should-repeat-and-jump :repeat t :jump t))
        (expect (evil-get-command-properties
                 'tag-should-repeat-and-jump)
                :to-equal '(:repeat t :jump t))
        (evil-set-command-properties 'tag-should-repeat-and-jump)
        (expect (evil-get-command-properties 'tag-should-repeat-and-jump)
                :to-be nil))))
  (it "should support delaying keybindings until the keymap exists"
    (tag-define-key
     :keymaps 'tag-delay-map
     "a" #'a)
    (expect (not (boundp 'tag-delay-map)))
    (require 'tag-delay)
    (expect (tag-test-keys nil tag-delay-map
              "a" #'a)))
  (it "should support delaying keybindings until the state exists"
    (tag-define-key
     :states 'non-existent
     "a" #'a)
    (expect (not (boundp 'evil-non-existent-state-map)))
    (require 'tag-non-existent-state)
    (expect (tag-test-keys nil evil-non-existent-state-map
              "a" #'a))))

;; ** Positional Definers
(describe "tag-emacs-define-key"
  (after-each
    (setq tag-temp-map (make-sparse-keymap)))
  (it "should take a positional argument for the keymap"
    (tag-emacs-define-key tag-temp-map
      "a" #'a)
    (expect (tag-test-keys nil tag-temp-map
              "a" #'a))
    ;; quoting is fine as well
    (tag-emacs-define-key 'tag-temp-map
      "b" #'b)
    (expect (tag-test-keys nil tag-temp-map
              "b" #'b))
    (tag-emacs-define-key (tag-temp-map
                               tag-test-mode-map)
      "c" #'c)
    (expect (tag-test-keys nil (list tag-temp-map
                                         tag-test-mode-map)
              "c" #'c))
    (tag-emacs-define-key '(tag-temp-map
                                tag-test-mode-map)
      "d" #'d)
    (expect (tag-test-keys nil (list tag-temp-map
                                         tag-test-mode-map)
              "d" #'d))))

(describe "tag-evil-define-key"
  (after-each
    (setq tag-temp-map (make-sparse-keymap)))
  (it "should take two positional arguments for the state and keymap"
    (tag-evil-define-key normal tag-temp-map
      "a" #'a)
    (expect (tag-test-keys 'normal tag-temp-map
              "a" #'a))
    ;; quoting is fine as well
    (tag-evil-define-key 'normal tag-temp-map
      "b" #'b)
    (expect (tag-test-keys 'normal tag-temp-map
              "b" #'b))
    (tag-evil-define-key (normal insert) tag-temp-map
      "c" #'c)
    (expect (tag-test-keys '(normal insert) tag-temp-map
              "c" #'c))
    (tag-evil-define-key '(normal insert) tag-temp-map
      "d" #'d)
    (expect (tag-test-keys '(normal insert) tag-temp-map
              "d" #'d))))

(describe "tag-def"
  (after-each
    (setq tag-temp-map (make-sparse-keymap)))
  (it "should act as `tag-define-key' when given 0 positional args"
    (tag-def :keymaps 'tag-temp-map
      "a" #'a)
    (expect (tag-test-keys nil tag-temp-map
              "a" #'a)))
  (it "should act as `tag-emacs-define-key' when given 1 positional arg"
    (tag-def tag-temp-map
      "a" #'a)
    (expect (tag-test-keys nil tag-temp-map
              "a" #'a)))
  (it "should act as `tag-evil-define-key' when given 2 positional args"
    (tag-def 'normal tag-temp-map
      "a" #'a)
    (expect (tag-test-keys 'normal tag-temp-map
              "a" #'a))))

(describe "tag-defs"
  (after-each
    (setq tag-temp-map (make-sparse-keymap)))
  (it "should split into multiple `tag-def's"
    (tag-defs
      tag-temp-map
      :states 'normal
      "a" #'a
      [?b] #'b
      'visual tag-temp-map
      "c" #'c
      :states 'insert :keymaps 'tag-temp-map
      "d" #'d)
    (expect (tag-test-keys 'normal tag-temp-map
              "a" #'a
              "b" #'b))
    (expect (tag-test-keys 'visual tag-temp-map
              "c" #'c))
    (expect (tag-test-keys 'insert tag-temp-map
              "d" #'d))))

;; ** Unbind Wrapper
(describe "tag-unbind"
  (after-each
    (setq tag-temp-map (make-sparse-keymap)))
  (it "should allow unbinding/ignoring keys en masse"
    (tag-define-key
     :keymaps 'tag-temp-map
     "a" #'a
     "b" #'b
     "c" #'c)
    (tag-unbind
      "a"
      "b"
      "c"
      ;; keywords should work at odd positions (e.g. keywords are often added at
      ;; end as defaults)
      :keymaps 'tag-temp-map)
    (expect (tag-test-keys nil tag-temp-map
              "a" nil
              "b" nil
              "c" nil))
    (tag-unbind
      :with #'ignore
      :keymaps 'tag-temp-map
      "a"
      "b"
      "c")
    (expect (tag-test-keys nil tag-temp-map
              "a" #'ignore
              "b" #'ignore
              "c" #'ignore)))
  (it "should allow positional arguments (wraps tag-def)"
    (tag-define-key
     :keymaps 'tag-temp-map
     "a" #'a
     "b" #'b
     "c" #'c)
    (tag-unbind tag-temp-map
      "a"
      "b"
      "c")
    (expect (tag-test-keys nil tag-temp-map
              "a" nil
              "b" nil
              "c" nil))
    ;; :with keyword should work at an odd position (must be handled internally)
    (tag-unbind 'tag-temp-map
      :with #'ignore
      "a"
      "b"
      "c")
    (expect (tag-test-keys nil tag-temp-map
              "a" #'ignore
              "b" #'ignore
              "c" #'ignore))))

;; ** User-created Definers
(describe "wrappers created with `tag-create-definer'"
  (before-all
    (tag-create-definer tag-nmap :states 'normal)
    (tag-create-definer tag-emacs-def
      :wrapping tag-emacs-define-key
      :states 'normal))
  (after-each
    (setq tag-temp-map (make-sparse-keymap)))
  (it "should use the specified arguments by default"
    (let ((evil-normal-state-map (make-sparse-keymap)))
      (tag-nmap "a" 'a)
      (expect (tag-test-keys nil evil-normal-state-map
                "a" #'a)))
    (tag-nmap
      :keymaps 'tag-temp-map
      "a" #'a)
    (expect (tag-test-keys 'normal tag-temp-map
              "a" #'a)))
  (it "should allow positional arguments (tag-def)"
    (tag-nmap tag-temp-map
      "a" #'a)
    (expect (tag-test-keys 'normal tag-temp-map
              "a" #'a)))
  (it "should allow overriding the default arguments"
    (tag-nmap
      :states 'visual
      :keymaps 'tag-temp-map
      "a" #'a)
    (expect (tag-test-keys 'normal tag-temp-map
              "a" nil))
    (expect (tag-test-keys 'visual tag-temp-map
              "a" #'a)))
  (it "should allow wrapping any definer"
    (let ((key "a"))
      (tag-emacs-def tag-temp-map key #'a)
      (tag-emacs-def tag-temp-map
        key #'a
        :states nil))
    (expect (tag-test-keys 'normal tag-temp-map
              "a" #'a))
    (expect (tag-test-keys nil tag-temp-map
              "a" #'a))))

;; ** Use-package Keywords
;; *** :tag
(describe "the :tag use-package keyword"
  (before-all
    (tag-create-definer tag-nmap
      :states 'normal))
  (after-each
    (setq tag-temp-map (make-sparse-keymap)))
  (it "should work as tag-def by default"
    (use-package some-package
      :ensure nil
      :tag
      ;; tag-define-key
      (:keymaps 'tag-temp-map
       "a" #'a)
      ("b" "b" :keymaps 'tag-temp-map)
      ("c" #'c :keymaps 'tag-temp-map)
      ;; tag-emacs-define-key
      (tag-temp-map "d" #'d)
      ;; tag-evil-define-key
      ('normal tag-temp-map "e" #'e))
    (expect (tag-test-keys nil tag-temp-map
              "a" #'a
              "b" "b"
              "c" #'c
              "d" #'d))
    (expect (tag-test-keys 'normal tag-temp-map
              "e" #'e)))
  (xit "should support any definer when it is manually specified"
    (expect (and (fboundp 'tag-nmap)
                 (macrop 'tag-nmap)))
    (use-package some-package
      :ensure nil
      :tag
      (tag-nmap tag-temp-map "a" #'a))
    ;; TODO macro expansion is correct and this works in emacs, but it does not
    ;; work here fo some reason
    ;; (message (format "%s"
    ;;                  (macroexpand '(use-package some-package
    ;;                                  :ensure nil
    ;;                                  :tag
    ;;                                  (tag-nmap tag-temp-map "a" #'a)))))
    (expect (tag-test-keys 'normal tag-temp-map
              "a" #'a)))
  (it "should correctly extract definitions from tag definer arglists"
    (expect (plist-get (use-package-normalize/:tag
                        nil
                        nil
                        '((tag-def "key" #'command)))
                       :commands)
            :to-equal '(command))
    (expect (plist-get (use-package-normalize/:tag
                        nil
                        nil
                        '((tag-def 'normal "key" #'command)))
                       :commands)
            :to-equal '(command))
    (expect (plist-get (use-package-normalize/:tag
                        nil
                        nil
                        '((tag-def 'normal tag-temp-map
                            "key" #'command)))
                       :commands)
            :to-equal '(command))
    (expect (plist-get (use-package-normalize/:tag
                        nil
                        nil
                        '((tag-defs
                            'normal
                            "key1" #'command1
                            'visual
                            "key2" #'command2)))
                       :commands)
            :to-equal '(command1 command2)))
  (it "should correctly extract symbols/commands to create autoloads from"
    (expect (tag--extract-autoloadable-symbol nil)
            :to-be nil)
    (expect (tag--extract-autoloadable-symbol "macro")
            :to-be nil)
    (expect (tag--extract-autoloadable-symbol [?m ?a ?c ?r ?o])
            :to-be nil)
    (expect (tag--extract-autoloadable-symbol tag-temp-map)
            :to-be nil)
    (expect (tag--extract-autoloadable-symbol (lambda () (interactive)))
            :to-be nil)
    (expect (tag--extract-autoloadable-symbol '(menu-item))
            :to-be nil)
    (expect (tag--extract-autoloadable-symbol 'symbol/command)
            :to-equal 'symbol/command)
    ;; conses
    (expect (tag--extract-autoloadable-symbol
             '("describe keybindings" . tag-describe-keybindings))
            :to-equal 'tag-describe-keybindings)
    ;; not sure of the exact syntax since this type of binding is broken in
    ;; recent emacs versions
    (expect (tag--extract-autoloadable-symbol
             '(fake-map . "char"))
            :to-equal nil)
    (expect (tag--extract-autoloadable-symbol
             '(:ignore t :wk "replacement"))
            :to-be nil)
    (expect (tag--extract-autoloadable-symbol
             '(:keymap create-autoload-map))
            :to-be nil)
    ;; created by tag; don't need autoloads
    (expect (tag--extract-autoloadable-symbol
             '(:prefix-command create-prefix-command))
            :to-be nil)
    (expect (tag--extract-autoloadable-symbol
             '(nil :keyword val))
            :to-be nil)
    (expect (tag--extract-autoloadable-symbol
             '(:def nil :keyword val))
            :to-be nil)
    (expect (tag--extract-autoloadable-symbol
             '(:def "macro" :wk "replacement"))
            :to-be nil)
    (expect (tag--extract-autoloadable-symbol
             '(:def [?m ?a ?c ?r ?o] :wk "replacement"))
            :to-be nil)
    (expect (tag--extract-autoloadable-symbol
             (list :def tag-temp-map :wk "replacement"))
            :to-be nil)
    (expect (tag--extract-autoloadable-symbol
             '(:def (lambda () (interactive)) :wk "replacement"))
            :to-be nil)
    (expect (tag--extract-autoloadable-symbol
             '(:def (menu-item) :wk "replacement"))
            :to-be nil)
    (expect (tag--extract-autoloadable-symbol
             '(symbol/command :wk "replacement"))
            :to-equal 'symbol/command)
    (expect (tag--extract-autoloadable-symbol
             '(:def symbol/command :wk "replacement"))
            :to-equal 'symbol/command)
    ;; conses
    (expect (tag--extract-autoloadable-symbol
             '(("describe keybindings" . tag-describe-keybindings)
               :keyword val))
            :to-equal 'tag-describe-keybindings)
    (expect (tag--extract-autoloadable-symbol
             '(:def ("describe keybindings" . tag-describe-keybindings)))
            :to-equal 'tag-describe-keybindings)
    (expect (tag--extract-autoloadable-symbol
             '(:def (fake-map . "char")))
            :to-equal nil)))

;; *** :ghook
(describe "the :ghook use-package keyword"
  (before-all
    (defvar tag-hook1 nil)
    (defvar tag-hook2 nil)
    (defvar tag-hook3 nil)
    (defvar tag-hook4 nil)
    (defvar tag-hooks '(tag-hook1 tag-hook2)))
  (before-each
    (setq tag-hook1 nil
          tag-hook2 nil
          tag-hook3 nil
          tag-hook4 nil))
  ;; :defer t isn't necessary since it is implied for :ghook
  (describe "specified with a variable"
    (it "should infer the function to add based on the package name"
      (use-package tag-fake
        :ghook tag-hooks)
      (expect tag-hook1
              :to-equal '(tag-fake-mode))
      (expect tag-hook2
              :to-equal '(tag-fake-mode))
      ;; test with `let'
      (let ((hooks '(tag-hook3 tag-hook4)))
        (use-package tag-fake-mode
          :ghook hooks))
      ;; should not add an extra -mode
      (expect tag-hook3
              :to-equal '(tag-fake-mode))
      (expect tag-hook4
              :to-equal '(tag-fake-mode))))
  (describe "specified with a hook symbol"
    (it "should infer the function to add based on the package name"
      (use-package tag-fake
        :ghook 'tag-hook1)
      (expect tag-hook1
              :to-equal '(tag-fake-mode))
      (use-package tag-fake-mode
        :ghook 'tag-hook2)
      ;; should not add an extra -mode
      (expect tag-hook2
              :to-equal '(tag-fake-mode))))
  (describe "specified with a `tag-add-hook' arglist"
    (it "should infer the function when there is no function arg"
      (use-package tag-fake
        :ghook
        ('tag-hook1))
      (expect tag-hook1
              :to-equal '(tag-fake-mode)))
    (it "should infer the function when the function is null or a non-symbol"
      (use-package tag-fake
        :ghook
        ('tag-hook1 nil)
        ('tag-hook2 ""))
      (expect tag-hook1
              :to-equal '(tag-fake-mode))
      (expect tag-hook2
              :to-equal '(tag-fake-mode)))
    (it "should work with a list of hooks"
      (use-package tag-fake
        :ghook
        ('(tag-hook1 tag-hook2)))
      (expect tag-hook1
              :to-equal '(tag-fake-mode))
      (expect tag-hook2
              :to-equal '(tag-fake-mode)))
    (it "should work, for example, with variable containing a list of hooks"
      (use-package tag-fake
        :ghook
        (tag-hooks))
      (expect tag-hook1
              :to-equal '(tag-fake-mode))
      (expect tag-hook2
              :to-equal '(tag-fake-mode)))
    (it "should work with explicitly specified functions"
      (use-package tag-fake
        :ghook
        ('tag-hook1 #'tag-fake-func1)
        ('tag-hook1 '(tag-fake-func2 tag-fake-func3))
        ('tag-hook1 (list #'tag-fake-func4))
        ('tag-hook1 (lambda ())))
      (expect tag-hook1
              :to-equal `(,(lambda ())
                          tag-fake-func4
                          tag-fake-func3
                          tag-fake-func2
                          tag-fake-func1)))
    (it "should support the extra APPEND (and LOCAL) args"
      (use-package tag-fake
        :ghook
        ('tag-hook1 #'tag-fake-func1 t)
        ('tag-hook1 #'tag-fake-func2 t))
      (expect tag-hook1
              :to-equal '(tag-fake-func1
                          tag-fake-func2)))
    (it "should add autoloads for non-lambda functions"
      (expect (not (functionp #'tag-autoload-me-mode)))
      (expect (not (functionp #'tag-autoload-me1)))
      (expect (not (functionp #'tag-autoload-me2)))
      (expect (not (functionp #'tag-autoload-me3)))
      (expect (not (functionp #'tag-autoload-me4)))
      (defvar tag-some-var nil)
      (defun tag-some-call ())
      (use-package tag-autoload-me
        :ghook 'tag-hook1
        ('tag-hook1 tag-some-var)
        ('tag-hook1 (lambda ()))
        ('tag-hook1 #'tag-autoload-me1)
        ('tag-hook1 'tag-autoload-me2)
        ('tag-hook1 '(tag-autoload-me3))
        ('tag-hook1 (list #'tag-autoload-me4 tag-some-var
                              (tag-some-call) (lambda ()))))
      (expect (not (functionp 'tag-some-var)))
      (expect (functionp #'tag-autoload-me-mode))
      (expect (functionp #'tag-autoload-me1))
      (expect (functionp #'tag-autoload-me2))
      (expect (functionp #'tag-autoload-me3))
      (expect (functionp #'tag-autoload-me4)))
    (it "should ignore macro/function calls and not fail"
      (use-package tag-autoload-me
        :ghook ('tag-hook1 (progn
                                 (defun some-func ())
                                 #'some-func))))))

;; *** :gfhook
(describe "the :gfhook use-package keyword"
  (before-all
    (defvar tag-fake-mode-hook nil)
    (defvar tag-hook1 nil)
    (defvar tag-fake-functions '(tag-fake-func1 tag-fake-func2)))
  (before-each
    (setq tag-fake-mode-hook nil
          tag-hook1 nil))
  (describe "specified with a variable"
    (it "should infer the hook to add to based on the package name"
      (use-package tag-fake
        :defer t
        :gfhook tag-fake-functions)
      (expect tag-fake-mode-hook
              :to-equal '(tag-fake-func2 tag-fake-func1))))
  (describe "specified with a function symbol"
    (it "should infer the hook to add to based on the package name"
      (use-package tag-fake
        :defer t
        :gfhook #'tag-fake-func1)
      (expect tag-fake-mode-hook
              :to-equal '(tag-fake-func1))
      (use-package tag-fake-mode
        :defer t
        :gfhook
        'tag-fake-func2
        #'tag-fake-func3)
      ;; should not add an extra -mode
      (expect tag-fake-mode-hook
              :to-equal '(tag-fake-func3
                          tag-fake-func2
                          tag-fake-func1))))
  (describe "specified with a `tag-add-hook' arglist"
    (it "should infer the hook when its arg is null or a non-symbol"
      (use-package tag-fake
        :defer t
        :gfhook
        (nil 'tag-fake-func1)
        ("" 'tag-fake-func2))
      (expect tag-fake-mode-hook
              :to-equal '(tag-fake-func2
                          tag-fake-func1)))
    (it "should work with a list of functions"
      (use-package tag-fake
        :defer t
        :gfhook
        (nil '(tag-fake-func1 tag-fake-func2))
        (nil (list #'tag-fake-func3 #'tag-fake-func4))
        (nil (lambda ())))
      (expect tag-fake-mode-hook
              :to-equal `(,(lambda ())
                          tag-fake-func4
                          tag-fake-func3
                          tag-fake-func2
                          tag-fake-func1)))
    (it "should work, for example, with a macro that expands to a function"
      (defmacro disable (mode)
        `(lambda () (,mode -1)))
      (use-package tag-fake
        :defer t
        :gfhook
        (nil (disable visual-line-mode)))
      (expect tag-fake-mode-hook
              :to-equal `(,(lambda () (visual-line-mode -1)))))
    (it "should work with explicitly specified hooks"
      (use-package tag-fake
        :defer t
        :gfhook
        ('tag-hook1 #'tag-fake-func1)
        ('tag-hook1 '(tag-fake-func2 tag-fake-func3))
        ('tag-hook1 (list #'tag-fake-func4)))
      (expect tag-hook1
              :to-equal '(tag-fake-func4
                          tag-fake-func3
                          tag-fake-func2
                          tag-fake-func1)))
    (it "should support the extra APPEND (and LOCAL) args"
      (use-package tag-fake
        :defer t
        :gfhook
        (nil #'tag-fake-func1 t)
        (nil #'tag-fake-func2 t))
      (expect tag-fake-mode-hook
              :to-equal '(tag-fake-func1
                          tag-fake-func2)))
    (it "should NOT add autoloads for any functions"
      (use-package tag-fake
        :defer t
        :gfhook 'tag-undefined)
      (expect (not (functionp #'tag-undefined))))
    (it "should not imply :defer t"
      ;; package doesn't exist, so should warn
      (spy-on 'display-warning)
      (use-package tag-fake
        :gfhook 'tag-fake-func1)
      (expect 'display-warning :to-have-been-called))))

;; * Global Override Mode
(describe "keybindings in `tag-override-mode-map'"
  (it "should override keybindings defined in other minor mode keymaps"
    (tag-define-key
     :keymaps 'tag-test-mode-map
     "C-a" #'forward-char
     "C-b" #'forward-char)
    (tag-define-key
     :keymaps 'tag-override-mode-map
     "C-a" #'backward-char)
    ;; test keymap alias
    (tag-define-key
     :keymaps 'override
     "C-b" #'backward-char)
    (tag-override-mode)
    (expect (tag-with "foo|"
              (evil-local-mode -1)
              "C-a C-b")
            :to-equal "f|oo")
    (tag-override-mode -1)))

;; * Displaying keybindings
;; TODO

;; * Other Key Definition Helpers
;; ** Key Simulation
;; TODO test with 'self-insert-command
;; TODO test when simulate command bound to a multi-key sequence
;; TODO test repeat with different LOOKUP args
;; TODO test case where STATE and no KEYMAPS (e.g. normal should inherit from
;; motion)
(describe "tag-simulate-key"
  (after-each
    (setq tag-temp-map (make-sparse-keymap)))
  (it "should automatically generate named functions"
    (tag-define-key
     :states 'normal
     :keymaps 'tag-temp-map
     "a" (tag-simulate-key "a")
     "b" (tag-simulate-key "b" :state 'emacs)
     "c" (tag-simulate-key "c" :keymap some-map)
     "d" (tag-simulate-key "d" :state 'insert :keymap some-map)
     "e" (tag-simulate-key (#'evil-delete "iw")))
    (expect (tag-test-keys 'normal tag-temp-map
              "a" #'tag-simulate-a
              "b" #'tag-simulate-b-in-emacs-state
              "c" #'tag-simulate-c-in-some-map
              "d" #'tag-simulate-d-in-insert-state-in-some-map
              "e" #'tag-simulate-evil-delete-iw)))
  (it "should allow explicitly specifying a function name"
    (tag-define-key
     :states 'normal
     :keymaps 'tag-temp-map
     "a" (tag-simulate-key "C-c" :name tag-C-c))
    (expect (tag-test-keys 'normal tag-temp-map
              "a" #'tag-C-c)))
  (it "should allow specifying a which-key description"
    (setq which-key-replacement-alist nil)
    (tag-simulate-key "a" :which-key "simulate a")
    (expect which-key-replacement-alist
            :to-equal '(((nil . "tag-simulate-a")
                         nil . "simulate a"))))
  (describe "should simulate the keys for a complete binding"
    (it "in the specified keymap"
      (tag-define-key
       :states 'normal
       :keymaps 'tag-test-mode-map
       "a" (tag-simulate-key "j" :keymap evil-motion-state-map))
      (expect (tag-with "|one\ntwo"
                "a")
              :to-equal "one\n|two"))
    (it "in the specified state"
      (tag-define-key
       :states 'normal
       :keymaps 'tag-test-mode-map
       "a" (tag-simulate-key "C-n" :state 'emacs))
      (expect (tag-with "|one\ntwo" "a")
              :to-equal "one\n|two")
      (tag-define-key
       :states 'normal
       :keymaps 'tag-test-mode-map
       ;; checks inheritance from motion state
       "a" (tag-simulate-key "j" :state 'normal))
      (expect (tag-with "|one\ntwo" "a")
              :to-equal "one\n|two"))
    (it "in the specified state and keymap"
      (tag-define-key
       :states 'normal
       :keymaps 'tag-temp-map
       "w" #'evil-forward-word-begin)
      (tag-define-key
       :states 'normal
       :keymaps 'tag-test-mode-map
       "a" (tag-simulate-key "w" :state 'normal :keymap tag-temp-map))
      (expect (tag-with "|one two" "a")
              :to-equal "one |two")))
  (it "should allow state and keymap aliases"
    (tag-define-key
     :states 'normal
     :keymaps 'tag-temp-map
     "w" #'evil-forward-word-begin)
    (tag-define-key
     :states 'normal
     :keymaps 'tag-test-mode-map
     "a" (tag-simulate-key "w" :state 'n :keymap temp))
    (expect (tag-with "|one two" "a")
            :to-equal "one |two"))
  ;; TODO haven't found a way to test incomplete bindings
  ;; if is possible, add tests for repeating (e.g. "di" then "w")
  (describe "should simulate the keys for an incomplete binding"
    (xit "in the specified state and keymap"
      (tag-define-key
       :states 'normal
       :keymaps 'tag-temp-map
       "bc" #'next-line)
      (tag-define-key
       :states 'normal
       :keymaps 'tag-test-mode-map
       "a" (tag-simulate-key "b" :state 'normal :keymap tag-temp-map))
      (expect (tag-with "|one\ntwo"
                ;; "ac"
                ;; (tag--simulate-keys nil "ac")
                (tag--simulate-keys nil "a")
                (tag--simulate-keys nil "c"))
              :to-equal "one\n|two")))
  ;; NOTE this can't be tested either even though it works below with a count
  (xit "should work when a command is specified"
    (tag-define-key
     :states 'normal
     :keymaps 'tag-test-mode-map
     "a" (tag-simulate-key (#'evil-delete "iw")))
    (expect (tag-with "|one two" "a")
            :to-equal "| two"))
  (it "should work with a prefix argument"
    (tag-define-key
     :states 'normal
     :keymaps 'tag-test-mode-map
     "a" (tag-simulate-key "j" :keymap evil-motion-state-map)
     "b" (tag-simulate-key (#'evil-delete "iw")))
    (expect (tag-with "|one\ntwo\nthree"
              "2a")
            :to-equal "one\ntwo\n|three")
    (expect (tag-with "|one two"
              "2b")
            :to-equal "|two")
    ;; should not affect next command
    (let ((tag--simulate-as-is t))
      (expect (tag-with "|one\ntwo\nthree\nfour\nfive"
                "2aa")
              :to-equal "one\ntwo\nthree\n|four\nfive")
      (expect (tag-with "|one two\nthree\nfour"
                "2ba")
              :to-equal "two\n|three\nfour")))
  ;; TODO a way to actually test recording the macro
  (describe "run during macro and evil-repeat recording/playback"
    (it "should work in the basic case"
      (tag-define-key
       :states 'normal
       :keymaps 'tag-test-mode-map
       "a" (tag-simulate-key "j" :keymap evil-motion-state-map))
      (expect (tag-with "|one\ntwo\nthree"
                (evil-declare-repeat #'evil-next-line)
                "a.")
              :to-equal "one\ntwo\n|three")
      (evil-declare-motion #'evil-next-line))
    ;; can't test but works manually
    (xit "should work when a command is specified"
      (tag-define-key
       :states 'normal
       :keymaps 'tag-test-mode-map
       "a" (tag-simulate-key (#'evil-delete "iw")))
      (expect (tag-with "|one two" "a.")
              :to-equal "|two"))
    (it "should work with a prefix argument"
      (tag-define-key
       :states 'normal
       :keymaps 'tag-test-mode-map
       "a" (tag-simulate-key "j" :keymap evil-motion-state-map))
      (evil-declare-repeat #'evil-next-line)
      (expect (tag-with "|one\ntwo\nthree\nfour\nfive" "2a.")
              :to-equal "one\ntwo\nthree\nfour\n|five")
      (evil-declare-motion #'evil-next-line)
      ;; (expect (tag-with "|one\ntwo\nthree\nfour\nfive"
      ;;           (evil-set-register ?\q "2a")
      ;;           "2a@q")
      ;;         :to-equal "one\ntwo\nthree\nfour\n|five")
      )
    (it "should test whether the command is repeatable when lookup is used"
      (tag-define-key
       :states 'normal
       :keymaps 'tag-test-mode-map
       "a" (tag-simulate-key "j" :keymap evil-motion-state-map)
       "b" (tag-simulate-key "k" :keymap evil-motion-state-map))
      (evil-declare-repeat #'evil-next-line)
      (let ((tag--simulate-as-is t))
        ;; should repeat next line and not previous line
        (expect (tag-with "|one\ntwo\nthree\nfour\nfive" "2ab.")
                :to-equal "one\ntwo\nthree\n|four\nfive"))
      (evil-declare-motion #'evil-next-line)))
  (it "should optionally check to see if commands have been remapped"
    (tag-define-key
     :states 'normal
     :keymaps 'tag-test-mode-map
     "a" (tag-simulate-key ('left-char "C-q a"))
     [remap left-char] #'right-char)
    (expect (tag-with "b|b" "a")
            :to-equal "bba|")
    (tag-define-key
     :states 'normal
     :keymaps 'tag-test-mode-map
     "a" (tag-simulate-key ('left-char "C-q a") :remap nil))
    (expect (tag-with "b|b" "a")
            :to-equal "a|bb")
    (tag-define-key
     :states 'normal
     :keymaps 'tag-test-mode-map
     [remap left-char] nil)))

(describe "tag-key"
  (it "should support custom setup and teardown"
    (tag-define-key
     :states 'normal
     :keymaps 'tag-test-mode-map
     "C-a" (tag-key "C-f")
     "C-f" #'backward-char)
    (expect (tag-with "fo|o"
              "C-a")
            :to-equal "f|oo")

    (tag-define-key
     :states 'normal
     :keymaps 'tag-test-mode-map
     "C-a" (tag-key "C-f"
             :setup (evil-local-mode -1)
             :teardown (evil-local-mode)))
    (expect (tag-with "fo|o"
              "C-a")
            :to-equal "foo|")
    (expect (tag-with "fo|o"
              "C-a" "C-f")
            :to-equal "fo|o")))

;; ** General Key Dispatch
;; TODO
;; - add tests for REMAP

;; ** General Predicate Dispatch
(describe "tag-predicate-dispatch"
  (it "should allow dispatching to multiple definitions based on predicates"
    (tag-define-key
     :states 'normal
     :keymaps 'tag-test-mode-map
     "<right>"
     (tag-predicate-dispatch #'right-char
       :docstring "Move right or to the bol."
       (eolp) #'beginning-of-line))
    (expect (tag-with "|foo" "<right>")
            :to-equal "f|oo")
    (expect (tag-with "foo|"
              "<right>")
            :to-equal "|foo")))

;; ** Translate Key
(describe "tag-translate-key"
  (before-each
    (tag-define-key
     :keymaps '(tag-temp-map tag-test-mode-map)
     "a" #'a
     "b" #'b
     "c" #'c)
    (tag-define-key
     :states '(normal visual)
     :keymaps 'tag-temp-map
     "a" #'a
     "b" #'b
     "c" #'c))
  (after-each
    (setq tag-temp-map (make-sparse-keymap)))
  (it "should bind each key to the definition of another key in the same keymap"
    (tag-translate-key nil '(tag-temp-map tag-test-mode-map)
      "a" "b"
      "b" "c"
      "c" "a")
    (expect (tag-test-keys nil (list tag-temp-map
                                         tag-test-mode-map)
              "a" #'b
              "b" #'c
              "c" #'a))
    (tag-translate-key '(normal visual) 'tag-temp-map
      "a" "b"
      "b" "c"
      "c" "a")
    (expect (tag-test-keys '(normal visual) tag-temp-map
              "a" #'b
              "b" #'c
              "c" #'a)))
  (it "should support translating keys using the original keymap for reference"
    (tag-translate-key nil 'tag-temp-map
      "a" "b")
    (tag-translate-key nil 'tag-temp-map
      "b" "c")
    (tag-translate-key nil 'tag-temp-map
      "c" "a")
    (expect (tag-test-keys nil tag-temp-map
              "a" #'b
              "b" #'c
              "c" #'a)))
  (it "should support destructively translating keys"
    (tag-translate-key nil 'tag-temp-map
      :destructive t
      "a" "b")
    (tag-translate-key nil 'tag-temp-map
      :destructive t
      "b" "c")
    (tag-translate-key nil 'tag-temp-map
      :destructive t
      "c" "a")
    (expect (tag-test-keys nil tag-temp-map
              "a" #'b
              "b" #'c
              "c" #'b)))
  (it "should support keymap and state aliases"
    (tag-translate-key 'n 'temp
      "a" "b"
      "b" "c"
      "c" "a")
    (expect (tag-test-keys 'normal tag-temp-map
              "a" #'b
              "b" #'c
              "c" #'a)))
  (xit "should support 'local and 'global")
  (it "should use kbd when `tag-implicit-kbd' is non-nil"
    (tag-translate-key nil 'tag-temp-map
      "C-a" "a")
    (expect (tag-test-keys nil tag-temp-map
              "C-a" #'a))
    (let (tag-implicit-kbd)
      (tag-translate-key nil 'tag-temp-map
        (kbd "C-b") "b")
      (expect (tag-test-keys nil tag-temp-map
                "C-b" #'b))))
  (it "should just make the backup keymap if MAPS and DESCTRUCTIVE are nil"
    (makunbound 'tag-tag-temp-map-backup-map)
    (tag-translate-key nil 'tag-temp-map)
    (expect (boundp 'tag-tag-temp-map-backup-map)))
  (it "should allow unbinding keys"
    (tag-define-key :keymaps 'tag-temp-map
      "a" #'a)
    (tag-test-keys nil tag-temp-map
      "a" #'a)
    (tag-translate-key nil 'tag-temp-map
      "a" nil)
    (tag-test-keys nil tag-temp-map
      "a" nil)))

(describe "tag-swap-key"
  (after-each
    (setq tag-temp-map (make-sparse-keymap)))
  (it "should swap keys in a keymap"
    (tag-define-key
     :keymaps 'tag-temp-map
     "a" #'a
     "b" #'b)
    (tag-swap-key nil 'tag-temp-map
      :destructive t
      "a" "b")
    (expect (tag-test-keys nil tag-temp-map
              "a" #'b
              "b" #'a))
    (tag-define-key
     :states 'normal
     :keymaps 'tag-temp-map
     "a" #'a
     "b" #'b)
    (tag-swap-key 'normal 'tag-temp-map
      :destructive t
      "a" "b")
    (expect (tag-test-keys 'normal tag-temp-map
              "a" #'b
              "b" #'a))))

;; ** Automatic Unbinding
(describe "tag-auto-unbind-keys"
  (after-each
    (setq tag-temp-map (make-sparse-keymap)))
  (it "should automatically unbind non-prefix keys to prevent errors"
    (define-key tag-temp-map "a" #'a)
    (expect (tag-define-key
             :keymaps 'tag-temp-map
             "ab" #'ab)
            :to-throw)
    (tag-auto-unbind-keys)
    (expect (define-key tag-temp-map "ab" #'ab)
            :to-throw)
    ;; tag--definer-p
    (expect (tag-define-key
             :keymaps 'tag-temp-map
             "ab" #'ab
             "abc" #'abc
             ;; vector keys
             [97 98 99 100] #'abcd)
            :not :to-throw)
    (tag-auto-unbind-keys t)
    (expect (tag-define-eky
             :keymaps 'tag-temp-map
             "abcde" #'abcde)
            :to-throw)))

;; ** Key-chord Helper
;; TODO

;; * Other Configuration Helpers
;; ** Settings
(describe "tag-setq"
  (it "should act as a drop in replacement for setq"
    (defvar tag-dummy-var-a nil)
    (defvar tag-dummy-var-b nil)
    (tag-setq tag-dummy-var-a t
                  tag-dummy-var-b t)
    (expect tag-dummy-var-a)
    (expect tag-dummy-var-b)
    (makunbound 'tag-dummy-var-a)
    (makunbound 'tag-dummy-var-b))
  (it "should correctly use a defined variable's custom setter"
    (defcustom tag-dummy-var-with-setter nil
      ""
      :group 'tag
      :set (lambda (sym _val)
             (set-default 'tag-dummy-var-with-setter
                          1)))
    (tag-setq tag-dummy-var-with-setter 'not-1)
    (expect tag-dummy-var-with-setter
            :to-equal 1)
    (makunbound 'tag-dummy-var-with-setter))
  (it "should work for an undefined variable with a custom setter"
    (expect (not (boundp 'tag-dummy-var-with-setter)))
    (tag-setq tag-dummy-var-with-setter 'not-1)
    (defcustom tag-dummy-var-with-setter nil
      ""
      :group 'tag
      :set (lambda (sym _val)
             (set-default 'tag-dummy-var-with-setter
                          1)))
    (expect tag-dummy-var-with-setter
            :to-equal 1)
    (makunbound 'tag-dummy-var-with-setter)))

;; ** Hooks
(describe "tag-add-hook and tag-remove-hook"
  (before-each
    (makunbound 'tag--test-hook)
    (defvar tag--test-hook nil)
    (makunbound 'tag--test-hook2)
    (defvar tag--test-hook2 nil))
  (it "should act as a drop in replacement for `add-hook' or `remove-hook'"
    ;; add
    (tag-add-hook 'tag--test-hook #'b)
    (tag-add-hook 'tag--test-hook #'a)
    (tag-add-hook 'tag--test-hook #'c t)
    (expect tag--test-hook
            :to-equal (list #'a #'b #'c))
    (with-temp-buffer
      (tag-add-hook 'tag--test-hook #'d t))
    (expect tag--test-hook
            :to-equal (list #'a #'b #'c #'d))
    (with-temp-buffer
      (tag-add-hook 'tag--test-hook #'e t t))
    (expect tag--test-hook
            :to-equal (list #'a #'b #'c #'d))
    ;; remove
    (tag-remove-hook 'tag--test-hook #'a)
    (tag-remove-hook 'tag--test-hook #'b)
    (tag-remove-hook 'tag--test-hook #'c)
    (tag-remove-hook 'tag--test-hook #'d)
    (expect (null tag--test-hook)))
  (it "should allow the hooks and functions to be lists"
    ;; add
    (tag-add-hook '(tag--test-hook
                        tag--test-hook2)
                      (list #'b #'a))
    (expect tag--test-hook
            :to-equal (list #'a #'b))
    (expect tag--test-hook2
            :to-equal (list #'a #'b))
    (tag-add-hook '(tag--test-hook
                        tag--test-hook2)
                      (list #'c #'d)
                      t)
    (expect tag--test-hook
            :to-equal (list #'a #'b #'c #'d))
    (expect tag--test-hook2
            :to-equal (list #'a #'b #'c #'d))
    ;; remove
    (tag-remove-hook '(tag--test-hook
                           tag--test-hook2)
                         (list #'a #'b #'c #'d))
    (expect (null tag--test-hook))
    (expect (null tag--test-hook2)))
  (it "should allow lambdas as functions"
    ;; add
    (tag-add-hook 'tag--test-hook (lambda ()))
    (expect tag--test-hook
            :to-equal (list (lambda ())))
    (tag-add-hook 'tag--test-hook (list (lambda () 1)) t)
    (expect tag--test-hook
            :to-equal (list (lambda ())
                            (lambda () 1)))
    ;; remove
    (tag-remove-hook 'tag--test-hook (lambda ()))
    (tag-remove-hook 'tag--test-hook (list (lambda () 1)))
    (expect (null tag--test-hook)))
  (describe "should allow creating \"transient\" hooks"
    (it "that are removed after one run"
      (let ((test-val 0))
        (tag-add-hook 'tag--test-hook (lambda () (cl-incf test-val))
                          nil nil t)
        (run-hooks 'tag--test-hook)
        (run-hooks 'tag--test-hook)
        (expect test-val
                :to-equal 1)))
    (it "that are removed after success"
      (let ((test-val 0))
        (tag-add-hook 'tag--test-hook
                          (lambda ()
                            (cl-incf test-val)
                            (if (= test-val 2)
                                t
                              nil))
                          nil nil #'identity)
        (run-hooks 'tag--test-hook)
        (run-hooks 'tag--test-hook)
        (run-hooks 'tag--test-hook)
        (expect test-val
                :to-equal 2)))
    (it "that are removed after any other condition"
      (let ((test-val 0))
        (tag-add-hook 'tag--test-hook
                          (lambda ()
                            (cl-incf test-val))
                          nil nil (lambda (val) (= val 3)))
        (run-hooks 'tag--test-hook)
        (run-hooks 'tag--test-hook)
        (run-hooks 'tag--test-hook)
        (run-hooks 'tag--test-hook)
        (expect test-val
                :to-equal 3)))))

;; ** Advice
(describe "tag-advice-add and tag-advice-remove"
  (before-all
    (define-error 'tag--test-error "Error")
    (define-error 'tag--test-error2 "Error 2")
    (defun tag--error ()
      (signal 'tag--test-error nil))
    (defun tag--error2 ()
      (signal 'tag--test-error2 nil)))
  (before-each
    (fmakunbound 'tag--test-func)
    (setplist 'tag--test-func nil)
    (defun tag--test-func () 1)
    (fmakunbound 'tag--test-func2)
    (setplist 'tag--test-func nil)
    (defun tag--test-func2 () 2))
  (it "should act as a drop in replacement for `advice-add' or `advice-remove'"
    ;; add
    (tag-advice-add 'tag--test-func :before #'tag--error)
    (expect (tag--test-func)
            :to-throw 'tag--test-error)
    ;; remove
    (tag-advice-remove 'tag--test-func #'tag--error)
    (expect (tag--test-func)
            :to-equal 1))
  (it "should allow the symbols and functions to be lists"
    ;; add
    (tag-advice-add '(tag--test-func
                          tag--test-func2)
                        :before
                        (list #'tag--error #'tag--error2))
    (expect (tag--test-func)
            :to-throw 'tag--test-error2)
    (expect (tag--test-func2)
            :to-throw 'tag--test-error2)
    ;; remove
    (tag-advice-remove '(tag--test-func tag--test-func2)
                           #'tag--error2)
    (expect (tag--test-func)
            :to-throw 'tag--test-error)
    (expect (tag--test-func2)
            :to-throw 'tag--test-error)
    (tag-advice-add '(tag--test-func
                          tag--test-func2)
                        :before
                        (list #'tag--error #'tag--error2))
    (tag-advice-remove '(tag--test-func tag--test-func2)
                           (list #'tag--error #'tag--error2))
    (expect (tag--test-func)
            :to-equal 1)
    (expect (tag--test-func2)
            :to-equal 2))
  (it "should allow lambdas as functions"
    ;; add
    (tag-advice-add 'tag--test-func
                        :before (lambda () (signal 'tag--test-error nil)))
    (expect (tag--test-func)
            :to-throw 'tag--test-error)
    (tag-advice-add 'tag--test-func
                        :before
                        (list (lambda () (signal 'tag--test-error2 nil))))
    (expect (tag--test-func)
            :to-throw 'tag--test-error2)
    ;; remove
    (tag-remove-advice 'tag--test-func
                           (lambda () (signal 'tag--test-error nil)))
    (tag-remove-advice 'tag--test-func
                           (list (lambda () (signal 'tag--test-error2 nil))))
    (expect (tag--test-func)
            :to-equal 1))
  (describe "should allow creating \"transient\" advice"
    (it "that is removed after one run"
      (defun tag-1 ()
        1)
      (tag-add-advice 'tag-1 :override (lambda (&rest _) 2)
                          nil t)
      (expect (tag-1) :to-equal 2)
      (expect (tag-1) :to-equal 1)
      (fmakunbound 'tag-1))
    (it "that is removed after success"
      (let ((test-val 0))
        (defun tag-1 ()
          1)
        (tag-add-advice 'tag-1
                            :override (lambda (&rest _)
                                        (if (= test-val 3)
                                            t
                                          (cl-incf test-val)
                                          nil))
                            nil #'identity)
        (expect (tag-1) :to-equal nil)
        (expect (tag-1) :to-equal nil)
        (expect (tag-1) :to-equal nil)
        (expect (tag-1) :to-equal t)
        (expect (tag-1) :to-equal 1)
        (fmakunbound 'tag-1)))
    (it "that is removed after any other condition"
      (let ((test-val 0))
        (defun tag-1 ()
          1)
        (tag-add-advice 'tag-1
                            :override (lambda (&rest _)
                                        (cl-incf test-val))
                            nil (lambda (val) (= val 3)))
        (expect (tag-1) :to-equal 1)
        (expect (tag-1) :to-equal 2)
        (expect (tag-1) :to-equal 3)
        (expect (tag-1) :to-equal 1)
        (fmakunbound 'tag-1)))))

(provide 'test-tag)
;;; test-tag.el ends here
