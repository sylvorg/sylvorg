;; Electric Sheep

;; First of all, I don't know how to check if I'm running on my phone, so I pass in a command-line argument:


;; [[file:README.org::*Electric Sheep][Electric Sheep:1]]
(defvar meq/var/phone (member "-p" command-line-args))
(delete "-p" command-line-args)
;; Electric Sheep:1 ends here

;; Double Dutch can be beat

;; Then I'll remove the double dashes from scripts:


;; [[file:README.org::*Double Dutch can be beat][Double Dutch can be beat:1]]
(when (string= (car (last command-line-args)) "--") (delete "--" command-line-args))
;; Double Dutch can be beat:1 ends here

;; XLR8!

;; These next few lines are unabashedly stolen from
;; [[https://github.com/hlissner][Henrik Lissner's]]
;; [[https://github.com/hlissner/doom-emacs/blob/develop/early-init.el][Doom Emacs' ~early-init.el~]]:

;; #+begin_quote
;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.
;; #+end_quote

;; #+begin_quote
;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
;; #+end_quote


;; [[file:README.org::*XLR8!][XLR8!:1]]
(setq gc-cons-threshold most-positive-fixnum)
;; XLR8!:1 ends here



;; And for the ~file-name-handler-alist~:


;; [[file:README.org::*XLR8!][XLR8!:2]]
(setq meq/var/file-name-handler-alist file-name-handler-alist)
(unless (or (daemonp) noninteractive)
;; XLR8!:2 ends here



;; #+begin_quote
;; `file-name-handler-alist' is consulted on each `require', `load' and
;; various path/io functions. You get a minor speed up by unsetting this.
;; Some warning, however: this could cause problems on builds of Emacs where
;; its site lisp files aren't byte-compiled and we're forced to load the
;; *.el.gz files (e.g. on Alpine).
;; #+end_quote


;; [[file:README.org::*XLR8!][XLR8!:3]]
(setq-default file-name-handler-alist nil)
;; XLR8!:3 ends here



;; #+begin_quote
;; ...but restore `file-name-handler-alist' later, because it is needed for
;; handling encrypted or compressed files, among other things.
;; #+end_quote


;; [[file:README.org::*XLR8!][XLR8!:4]]
(defun meq/reset-file-handler-alist-h ()
  (setq file-name-handler-alist
;; XLR8!:4 ends here



;; #+begin_quote
;; Merge instead of overwrite because there may have bene changes to
;; `file-name-handler-alist' since startup we want to preserve.
;; #+end_quote


;; [[file:README.org::*XLR8!][XLR8!:5]]
(delete-dups (append file-name-handler-alist
                             meq/var/file-name-handler-alist))))
(add-hook 'emacs-startup-hook #'meq/reset-file-handler-alist-h 101))
;; XLR8!:5 ends here

;; Bite the gold

;; I would like to always prefer newer byte-compiled files, therefore, I use
;; [[https://emacs.stackexchange.com/a/186/31428][this answer]], by
;; [[https://emacs.stackexchange.com/users/50/malabarba][Malabarba]]:


;; [[file:README.org::*Bite the gold][Bite the gold:1]]
(setq load-prefer-newer t)
;; Bite the gold:1 ends here

;; Remember your origins

;; If I ever need it, this will give me the initial directory I was in; the code is adapted from
;; [[https://emacs.stackexchange.com/users/1979/stefan][Stefan's]]
;; [[https://emacs.stackexchange.com/a/31662/31428][answer here]]:


;; [[file:README.org::*Remember your origins][Remember your origins:1]]
(setq meq/var/initial-directory default-directory)
;; Remember your origins:1 ends here

;; ¯\_(ツ)_/¯

;; The next few bits are adapted from
;; [[https://www.reddit.com/r/emacs/comments/dppmqj/do_i_even_need_to_leverage_earlyinitel_if_i_have/?utm_source=amp&utm_medium=&utm_content=post_body][here]],
;; with a few quotes from myself and other scattered here and there, such as this bit
;; [[https://www.reddit.com/r/emacs/comments/41m7x3/why_are_you_changing_gcconsthreshold/cz3t775?utm_source=share&utm_medium=web2x&context=3][about ~gc-cons-percentage~]]:

;; #+begin_quote
;; ... There's also gc-cons-percentage which performs a gc if the amount of new memory used as a percentage
;; of the total has increased by a certain amount.
;; If you set gc-cons-threshold to a large number that effectively puts gc-cons-percentage into the driving seat.
;; The default gc-cons-threshold is 400000 bytes, not 800000. ...
;; #+end_quote


;; [[file:README.org::*¯\_(ツ)_/¯][¯\_(ツ)_/¯:1]]
(defvar meq/var/gc-cons-percentage gc-cons-percentage)

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-percentage meq/var/gc-cons-percentage)

            (defun meq/gc-on-lose-focus ()
              (unless (frame-focus-state)
                (garbage-collect)))

            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function #'meq/gc-on-lose-focus))))

(setq-default gc-cons-percentage 0.6
;; ¯\_(ツ)_/¯:1 ends here



;; Dunno /quite/ what this bit does...


;; [[file:README.org::*¯\_(ツ)_/¯][¯\_(ツ)_/¯:2]]
auto-window-vscroll nil
frame-inhibit-implied-resize t
inhibit-compacting-font-caches t)
;; ¯\_(ツ)_/¯:2 ends here



;; I don't like typing ~yes~ or ~no~ all the time, so we'll shorten the answer statement a bit.


;; [[file:README.org::*¯\_(ツ)_/¯][¯\_(ツ)_/¯:3]]
(fset 'yes-or-no-p 'y-or-n-p)
;; ¯\_(ツ)_/¯:3 ends here



;; Dunno what /this/ bit does either...


;; [[file:README.org::*¯\_(ツ)_/¯][¯\_(ツ)_/¯:4]]
(fset 'view-hello-file 'ignore)
(fset 'display-startup-echo-area-message 'ignore)

(put 'narrow-to-region 'disabled nil)
(put 'up-case-rgion 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-appearance . nil) default-frame-alist)
(push '(internal-border . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)
(push '(left-fringe . 0) default-frame-alist)
(push '(right-fringe . 0) default-frame-alist)
;; ¯\_(ツ)_/¯:4 ends here

;; Here's your profile

;; Now that that's over with, let's get the profile name; this is done by searching through the ~command-line-args~
;; list for the ~--profile~ argument. If found, get the profile name from the index of the ~--profile~ argument
;; plus 1, otherwise, set it to the default name of ~damascus~.


;; [[file:README.org::*Here's your profile][Here's your profile:1]]
(defvar meq/var/profiled t)
(defvar meq/var/profile-name (if (member "--profile" command-line-args)
    (let* ((value (nth (1+ (seq-position command-line-args "--profile")) command-line-args)))
        (unwind-protect
            value
;; Here's your profile:1 ends here



;; While we're at it, we'll delete the the appropriate command-line arguments as well:


;; [[file:README.org::*Here's your profile][Here's your profile:2]]
(delete "--profile" command-line-args)
(delete value command-line-args))) "damascus"))
;; Here's your profile:2 ends here

;; Did I forget something...?

;; This next bit defines a function which will add arguments to the ~command-line-args~ list, if and only if it
;; doesn't already exist in the list and the argument is an option, as when prefixed by ~-~ or ~--~.


;; [[file:README.org::*Did I forget something...?][Did I forget something...?:1]]
(require 'cl)
(defun meq/push-to-cla (args)
    (dolist (arg* args)
        (let* ((arg (if (stringp arg*) arg* (symbol-name arg*)))
                (already-in-list (member arg command-line-args)))
            (when (and
                    (or (string-prefix-p "-" arg) (string-prefix-p "--" arg))
                    (not already-in-list)) (add-to-list 'command-line-args arg t)))))
;; Did I forget something...?:1 ends here



;; Next, for specific profiles, if applicable, we will add any arguments necessary to be able to run the profile.


;; [[file:README.org::*Did I forget something...?][Did I forget something...?:2]]
(pcase meq/var/profile-name
    ("damascus" (meq/push-to-cla '(--literate-config)))
    ("nano" (meq/push-to-cla '(--profile-lib profiles/nano/lisp/nano.el)))
    ("graphene" (meq/push-to-cla '(--profile-lib profiles/graphene/lisp/graphene.el --literate-config))))
;; Did I forget something...?:2 ends here

;; After Vars


;; [[file:README.org::*After Vars][After Vars:1]]
(defvar meq/var/literate-config (member "--literate-config" command-line-args))
(delete "--literate-config" command-line-args)
;; After Vars:1 ends here

;; Silva Scientiae

;; Let's byte-compile the library directories and add them to the load-path now;
;; the following bits are adapted from [[https://emacs.stackexchange.com/users/14825/nickd][NickD's answer]]
;; [[https://emacs.stackexchange.com/a/55415/31428][here]], and
;; [[https://www.emacswiki.org/emacs/LoadPath#h5o-2][from this section of the Emacs Wiki]].

;; The first directory to compile and add is the directory of emacs packages, as git submodules, of my project,
;; managed by... well... you'll see.


;; [[file:README.org::*Silva Scientiae][Silva Scientiae:1]]
(let ((default-directory (concat pre-user-emacs-directory "lib")))
    (byte-recompile-directory default-directory nil)
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))
;; Silva Scientiae:1 ends here



;; The second directory consists of the packages I develop, as git subtrees:


;; [[file:README.org::*Silva Scientiae][Silva Scientiae:2]]
(let ((default-directory (concat pre-user-emacs-directory "siluam")))
    (byte-recompile-directory default-directory nil)
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))
;; Silva Scientiae:2 ends here

;; RESISTANCE IS FUTILE

;; #+begin_export html
;; <p align="center"><a href="https://github.com/emacscollective/borg"><img src="borg.gif"></a></p>
;; #+end_export


;; [[file:README.org::*RESISTANCE IS FUTILE][RESISTANCE IS FUTILE:1]]
(setq package-enable-at-startup nil)
(require 'borg)
;; RESISTANCE IS FUTILE:1 ends here



;; I would like to force adding the git submodules:


;; [[file:README.org::*RESISTANCE IS FUTILE][RESISTANCE IS FUTILE:2]]
(defun meq/borg-assimilate-advice (package url &optional partially)
  "Assimilate the package named PACKAGE from URL.
If `epkg' is available, then only read the name of the package
in the minibuffer and use the url stored in the Epkg database.
If `epkg' is unavailable, the package is not in the database, or
with a prefix argument, then also read the url in the minibuffer.
With a negative prefix argument only add the submodule but don't
build and activate the drone."
  (interactive
   (nconc (borg-read-package "Assimilate package: " current-prefix-arg)
          (list (< (prefix-numeric-value current-prefix-arg) 0))))
  (borg--maybe-confirm-unsafe-action "assimilate" package url)
  (message "Assimilating %s..." package)
  (let ((default-directory borg-top-level-directory))
    (borg--maybe-reuse-gitdir package)
    (borg--call-git package "submodule" "add" "-f" "--name" package url
                    (file-relative-name (borg-worktree package)))
    (borg--sort-submodule-sections ".gitmodules")
    (borg--call-git package "add" ".gitmodules")
    (borg--maybe-absorb-gitdir package))
  (unless partially
    (borg-build package)
    (borg-activate package))
  (borg--refresh-magit)
  (message "Assimilating %s...done" package))
(advice-add #'borg-assimilate :override #'meq/borg-assimilate-advice)
;; RESISTANCE IS FUTILE:2 ends here

;; [[file:README.org::*RESISTANCE IS FUTILE][RESISTANCE IS FUTILE:3]]
(setq borg-rewrite-urls-alist '(("git@github.com:" . "https://github.com/")
                                ("git@gitlab.com:" . "https://gitlab.com/")))
(borg-initialize)
;; RESISTANCE IS FUTILE:3 ends here

;; The one, the only...

;; Now for the fun part: let's [[https://github.com/jwiegley/use-package][use-package]]
;; with [[https://github.com/jwiegley][John Wiegley]]!


;; [[file:README.org::*The one, the only...][The one, the only...:1]]
(with-no-warnings
  (setq use-package-verbose t)
  (setq use-package-enable-imenu-support t))
(require 'use-package)
;; The one, the only...:1 ends here

;; Sometimes defer package loading

;; Quoted from [[https://github.com/jwiegley/use-package#loading-packages-in-sequence][Use-Package's Loading packages in sequence]]:

;; #+begin_quote
;; NOTE: pay attention if you set use-package-always-defer to t, and also use the :after keyword, as you will need to specify how the
;; declared package is to be loaded: e.g., by some :bind. If you're not using one of the mechanisms that registers autoloads, such as
;; :bind or :hook, and your package manager does not provide autoloads, it's possible that without adding :defer 2 to those declarations,
;; your package will never be loaded.
;; #+end_quote

;; Quoted from [[https://github.com/jwiegley/use-package#notes-about-lazy-loading][Use-Package's Notes about lazy loading]]:

;; #+begin_quote
;; In almost all cases you don't need to manually specify :defer t. This is implied whenever :bind or :mode or :interpreter is used.
;; Typically, you only need to specify :defer if you know for a fact that some other package will do something to cause your package to
;; load at the appropriate time, and thus you would like to defer loading even though use-package isn't creating any autoloads for you.
;; You can override package deferral with the :demand keyword. Thus, even if you use :bind, using :demand will force loading to occur
;; immediately and not establish an autoload for the bound key.
;; #+end_quote

;; Quoted from [[https://github.com/jwiegley/use-package#modes-and-interpreters][Use-Package's Modes and interpreters]]:

;; #+begin_quote
;; Similar to :bind, you can use :mode and :interpreter to establish a deferred binding within the auto-mode-alist and interpreter-mode-alist variables.
;; ...
;; If you aren't using :commands, :bind, :bind*, :bind-keymap, :bind-keymap*, :mode, :interpreter, or :hook
;; (all of which imply :defer; see the docstring for use-package for a brief description of each), you can still defer loading with the :defer keyword...
;; #+end_quote

;; Quoted from [[https://github.com/jwiegley/use-package#magic-handlers][Use-Package's Magic handlers]]:

;; #+begin_quote
;; Similar to :mode and :interpreter, you can also use :magic and :magic-fallback to cause certain function to be run if the beginning of a file matches
;; a given regular expression.
;; ...
;; This registers an autoloaded command for pdf-view-mode, defers loading of pdf-tools, and runs pdf-view-mode if the beginning of a buffer matches the string "%PDF".
;; #+end_quote

;; Quoted from [[https://github.com/Kungsgeten/ryo-modal#use-package-keyword][RYO-Modal's Use-package keyword]]:

;; #+begin_quote
;; Ryo-modal also provides a use-package keyword: :ryo, which is similar to :bind in that it implies :defer t and create autoloads for the bound commands.
;; The keyword is followed by one or more key-binding commands, using the same syntax as used by ryo-modal-keys...
;; #+end_quote

;; Quoted from [[https://github.com/noctuid/general.el#use-package-keywords][General's Use-package Keywords]]:

;; #+begin_quote
;; :general is similar to :bind in that it implies :defer t whenever there are bound commands that can be autoloaded
;; (e.g. it will not imply :defer t if the only bound command is to a lambda, for example). Whenever autoloadable commands are bound,
;; use-package will create autoloads for them (though this is usually not necessary).
;; #+end_quote

;; Quoted from [[https://github.com/noctuid/general.el#ghook-keyword][General's :ghook Keyword]]:

;; #+begin_quote
;; :ghook is intended to be used to add a package’s minor mode enabling function to a user-specified hook, so that when hook is run,
;; the package will be loaded and the mode enabled. This means that :ghook will usually imply :defer t. While it does not always imply :defer t,
;; it will add any non-lambda functions to :commands (this is the same behavior as :hook).
;; Though this is usually unnecessary (the commands probably already have autoloads), it will in turn imply :defer t.
;; #+end_quote

;; Quoted from [[https://github.com/noctuid/general.el#gfhook-keyword][General's :gfhook Keyword]]:

;; #+begin_quote
;; Unlike :ghook, :gfhook never adds functions to :commands and therefore never implies :defer t.
;; This is because the functions specified are ones that should be run when turning on (or toggling) the mode(s) the package provides.
;; The specified functions are external to the package, could be called elsewhere, and therefore should not trigger the package to load.
;; #+end_quote

;; Also see [[https://github.com/jwiegley/use-package/issues/738#issuecomment-447631609][this comment]].

;; Note that I assume that [[https://github.com/jwiegley/use-package#use-package-chords][chords]]
;; also defer and create autoloads.

;; And in my experience... Not a good idea; much too confusing. Use
;; [[https://www.reddit.com/r/emacs/comments/j2xezg/usepackage_best_practices/][the arguments here]]
;; to decide whether to use this or =:defer <n>= instead.


;; [[file:README.org::*Sometimes defer package loading][Sometimes defer package loading:1]]
(setq use-package-always-defer (member "--always-defer" command-line-args))
(delete "--always-defer" command-line-args)
;; Sometimes defer package loading:1 ends here

;; And the rest

;; Similar to what's happening above, this bit searches the ~command-line-args~ list for the ~--always-demand~
;; argument, and sets ~use-package-always-demand~ accordingly and deletes the argument from the list;
;; it also sets the variable if Emacs is running as a daemon.


;; [[file:README.org::*And the rest][And the rest:1]]
(setq use-package-always-demand (or (member "--always-demand" command-line-args) (daemonp)))
(delete "--always-demand" command-line-args)
;; And the rest:1 ends here

;; The Maid

;; This package has Emacs store most / all local files in a specific subdirectory:


;; [[file:README.org::*The Maid][The Maid:1]]
(use-package no-littering :demand t)
;; The Maid:1 ends here



;; And as stated before, from
;; [[https://github.com/hlissner/doom-emacs/blob/develop/early-init.el][Doom Emacs' ~early-init.el~]]:


;; [[file:README.org::*The Maid][The Maid:2]]
(use-package gcmh :demand t :config (gcmh-mode 1))
;; The Maid:2 ends here

;; Extra, extra!

;; This sets up the following:
;; - [[https://github.com/plexus/a.el][a.el]] by [[https://github.com/plexus][Arne Brasseur]]
;; - [[https://github.com/rejeep/f.el][f.el]] by [[https://github.com/rejeep][Johan Andersson]]
;; - [[https://github.com/magnars/dash.el][dash.el]] by [[https://github.com/magnars][Magnar Sveen]]
;; - [[https://github.com/magnars/s.el][s.el]] by [[https://github.com/magnars][Magnar Sveen]]
;; - [[https://github.com/shadowrylander/meq][meq]] by yours truely! :D
;; - [[https://github.com/conao3/leaf.el][leaf.el]] by [[https://github.com/conao3][Naoya Yamashita]]

;; And finally:
;; - [[https://github.com/shadowrylander/use-package-extras][use-package-extras]] by yours truely! :D


;; [[file:README.org::*Extra, extra!][Extra, extra!:1]]
(use-package use-package-extras
    :demand t
    :init (require 'a) (require 'dash) (require 's) (require 'f)
    :config
        (meq/up meq :load-emacs-file-preconfig ("naked"))
        (meq/up leaf :use-package-preconfig
            (use-package-ensure-system-package)
            (leaf-keywords)))
;; Extra, extra!:1 ends here

;; Yellow Brick Executable Road

;; Unless I'm on Windows or a DOS-based OS, I'll need to make sure every executable available on my ~$PATH~ can be
;; found by Emacs as well:


;; [[file:README.org::*Yellow Brick Executable Road][Yellow Brick Executable Road:1]]
(unless (member system-type '(windows-nt ms-dos))
    (meq/up exec-path-from-shell
        :custom
            (exec-path-from-shell-check-startup-files nil)
            (exec-path-from-shell-variables '("PATH" "MANPATH" "CACHE_HOME" "FPATH" "PYENV_ROOT"))
            (exec-path-from-shell-arguments '("-l"))
        :config
            (exec-path-from-shell-initialize)))
;; Yellow Brick Executable Road:1 ends here

;; That was fast

;; These are two settings I like for ~native compilation~, adapted from
;; [[https://github.com/daviwil/dotfiles/blob/master/Emacs.org#native-compilation][here]]:

;; #+begin_quote
;; Silence compiler warnings as they can be pretty disruptive
;; #+end_quote


;; [[file:README.org::*That was fast][That was fast:1]]
(ignore-errors
    (setq native-comp-async-report-warnings-errors nil)
;; That was fast:1 ends here



;; #+begin_quote
;; Set the right directory to store the native comp cache
;; #+end_quote


;; [[file:README.org::*That was fast][That was fast:2]]
(add-to-list 'native-comp-eln-load-path (meq/ued-local "eln-cache/")))
;; That was fast:2 ends here

;; There's no place like ~user-emacs-directory~

;; Now that we have ~f.el~ set up, we can set the ~user-emacs-directory~ to the root directory of the profile
;; being used:


;; [[file:README.org::*There's no place like ~user-emacs-directory~][There's no place like ~user-emacs-directory~:1]]
(setq user-emacs-directory (f-full (meq/ued* "profiles" meq/var/profile-name)))
;; There's no place like ~user-emacs-directory~:1 ends here



;; And, unless it's [[https://github.com/hlissner/doom-emacs][Doom Emacs]] being run,
;; we'll ~byte-compile~ the profile directory as well:


;; [[file:README.org::*There's no place like ~user-emacs-directory~][There's no place like ~user-emacs-directory~:2]]
(unless (string= meq/var/profile-name "doom") (byte-recompile-directory user-emacs-directory nil))
;; There's no place like ~user-emacs-directory~:2 ends here



;; Then we'll set the custom file for the profile:


;; [[file:README.org::*There's no place like ~user-emacs-directory~][There's no place like ~user-emacs-directory~:3]]
(setq custom-file (meq/ued "custom.el"))
(meq/cl custom-file)
;; There's no place like ~user-emacs-directory~:3 ends here



;; And then finally the ~auto-save-list-prefix~, as adapted from
;; [[https://emacs.stackexchange.com/users/2731/ebpa][ebpa's]]
;; [[https://emacs.stackexchange.com/a/18682/31428][answer here]]:


;; [[file:README.org::*There's no place like ~user-emacs-directory~][There's no place like ~user-emacs-directory~:4]]
(setq auto-save-list-file-prefix user-emacs-directory)
;; There's no place like ~user-emacs-directory~:4 ends here

;; Mmm... Orange...

;; And now for my favorite part: ORANGE! Erm... Sorry, I meant themes. Let's byte-compile them first:


;; [[file:README.org::*Mmm... Orange...][Mmm... Orange...:1]]
(byte-recompile-directory (meq/ued* "themes") nil)
;; Mmm... Orange...:1 ends here



;; Now we can add them to the ~custom-theme-load-path~ list:


;; [[file:README.org::*Mmm... Orange...][Mmm... Orange...:2]]
(add-to-list 'custom-theme-load-path (meq/ued* "themes"))
;; Mmm... Orange...:2 ends here



;; Also, my themes are safe (I think...):


;; [[file:README.org::*Mmm... Orange...][Mmm... Orange...:3]]
(setq custom-safe-themes t)
;; Mmm... Orange...:3 ends here

;; Would you like fries with that?

;; These are any last-minute steps before running any specific Emacs profile.


;; [[file:README.org::*Would you like fries with that?][Would you like fries with that?:1]]
(let* ((spacemacs-path (meq/ued-profiles
                            "spacemacs"
                            "layers"
                            "+distributions"
                            "spacemacs-bootstrap")))
    (pcase meq/var/profile-name
        ("doom" (load (meq/ued-lib "ido-completing-read+" "ido-completing-read+.el")))
        ("spacemacs" (progn
                        (load (concat spacemacs-path "packages.el"))
                        (load (concat spacemacs-path "funcs.el"))
                        (spacemacs-bootstrap/init-use-package)))
        ("patrick" (advice-add #'reload-config :override #'(lambda nil (interactive)
                    (meq/cl (meq/ued-profiles "patrick" "readme.org")))))
        ("alhassy" (require 'quelpa-use-package))))
;; Would you like fries with that?:1 ends here

;; Party in the CLA

;; This function can be succinctly explained in a few steps.

;; When the ~arg~ passed to this function exists in ~command-line-args~...


;; [[file:README.org::*Party in the CLA][Party in the CLA:1]]
(defun meq/load-from-cla (arg &optional byte-compile)
    (eval `(meq/when-item-in-cla ,arg
;; Party in the CLA:1 ends here



;; Get the item after the ~arg~:


;; [[file:README.org::*Party in the CLA][Party in the CLA:2]]
(let* ((item (meq/get-next-in-cla ,arg))
;; Party in the CLA:2 ends here



;; Let's assume the item is a file:


;; [[file:README.org::*Party in the CLA][Party in the CLA:3]]
(file (expand-file-name item))
;; Party in the CLA:3 ends here



;; Does the file exist?


;; [[file:README.org::*Party in the CLA][Party in the CLA:4]]
(exists (f-exists? file))
;; Party in the CLA:4 ends here



;; If it does, is it /really/ a directory?


;; [[file:README.org::*Party in the CLA][Party in the CLA:5]]
(is-dir (and exists (f-directory? file)))
;; Party in the CLA:5 ends here



;; If it's a directory, then, well, it's a directory; if not, assume it's a file, and get it's parent directory.


;; [[file:README.org::*Party in the CLA][Party in the CLA:6]]
(dir (if is-dir file (f-dirname file))))
;; Party in the CLA:6 ends here



;; Assuming the item is a file, is it an ~org~ file?


;; [[file:README.org::*Party in the CLA][Party in the CLA:7]]
(org-file* (f-ext file))
(org-file (and org-file* (string= org-file* "org")))
;; Party in the CLA:7 ends here



;; Now. If the file doesn't exist...


;; [[file:README.org::*Party in the CLA][Party in the CLA:8]]
(if (not exists)
;; Party in the CLA:8 ends here



;; Assume it's a function, macro, or command, and run it:


;; [[file:README.org::*Party in the CLA][Party in the CLA:9]]
(eval (intern item))
;; Party in the CLA:9 ends here



;; Otherwise, if we told ~meq/load-from-cla~ to byte-compile ~dir~:


;; [[file:README.org::*Party in the CLA][Party in the CLA:10]]
(when ,byte-compile (byte-recompile-directory dir nil))
;; Party in the CLA:10 ends here



;; Then add ~dir~ to the ~load-path~:


;; [[file:README.org::*Party in the CLA][Party in the CLA:11]]
(add-to-list 'load-path dir)
;; Party in the CLA:11 ends here



;; And finally, if ~item~ was originally a file that exists, load it, and if it's an ~org~ file,
;; ~org-babel-load-file~ it:


;; [[file:README.org::*Party in the CLA][Party in the CLA:12]]
(meq/cl file))))))
;; Party in the CLA:12 ends here

;; Yay, we're early!

;; This bit here will load an alternate ~early-init~ for a profile if ~--profile-early-init~ is passed on the
;; command-line, and otherwise, load the profile's usual ~early-init~ if it exists.


;; [[file:README.org::*Yay, we're early!][Yay, we're early!:1]]
(meq/if-item-in-cla "--profile-early-init"
    (meq/load-from-cla "--profile-early-init")
    (meq/cl "early-init.el"))
;; Yay, we're early!:1 ends here

;; A quiet place

;; If there's an alternate library directory or file the user wants to load,
;; ~--profile-early-lib~ will do the trick:


;; [[file:README.org::*A quiet place][A quiet place:1]]
(meq/load-from-cla "--profile-early-lib" t)
;; A quiet place:1 ends here
