![An extensible vi layer for Emacs](https://raw.githubusercontent.com/emacs-aiern/aiern/master/doc/logo.png)

[![Build Status](https://travis-ci.org/emacs-aiern/aiern.svg?branch=master)](https://travis-ci.org/emacs-aiern/aiern)
[![MELPA](https://melpa.org/packages/aiern-badge.svg)](https://melpa.org/#/aiern)
[![MELPA Stable](https://stable.melpa.org/packages/aiern-badge.svg)](https://stable.melpa.org/#/aiern)
[![Documentation Status](https://readthedocs.org/projects/aiern/badge/?version=latest)](https://aiern.readthedocs.io/en/latest/?badge=latest)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

aiern is an **e**xtensible **vi** **l**ayer
for [Emacs](http://www.gnu.org/software/emacs/). It emulates the main features
of [Vim](http://www.vim.org/), and provides facilities for writing custom
extensions. Also see our page on [EmacsWiki](http://emacswiki.org/emacs/aiern).

# Installation

See the 
[official documentation](https://aiern.readthedocs.io/en/latest/overview.html#installation-via-package-el)
for installation instructions. We recommend using *package.el*.

As a quickstart, you can add the following code to your Emacs init
file.

```elisp
;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

;; Download aiern
(unless (package-installed-p 'aiern)
  (package-install 'aiern))

;; Enable aiern
(require 'aiern)
(aiern-mode 1)
```

## Dependencies

* aiern requires Emacs 24.1 or later.

* aiern requires any of the following for `C-r`:
  * `undo-redo` from Emacs 28
  * The [undo-tree](http://www.emacswiki.org/emacs/UndoTree) package
  * The [undo-fu](https://gitlab.com/ideasman42/emacs-undo-fu) package

* For the motions `g;` `g,` and for the last-change-register `.`, aiern requires the
[goto-chg.el](https://github.com/emacs-aiern/goto-chg) package,
which provides the functions `goto-last-change` and `goto-last-change-reverse`.

* For Emacs 24.1 and 24.2 aiern also requires
  [cl-lib](https://elpa.gnu.org/packages/cl-lib.html).

# Documentation

The latest version of the documentation is readable online
[here](https://aiern.readthedocs.io/en/latest/index.html). It is also
available as
[PDF](https://readthedocs.org/projects/aiern/downloads/pdf/latest/) and
as [EPUB](https://readthedocs.org/projects/aiern/downloads/epub/latest/).

# Mailing list

aiern is discussed at the
[gmane.emacs.vim-emulation](http://lists.ourproject.org/cgi-bin/mailman/listinfo/implementations-list)
mailing list.

# IRC

Visit us on `irc.libera.chat #aiern-mode`.

# Contribution

See
[CONTRIBUTING.md](https://github.com/emacs-aiern/aiern/blob/master/CONTRIBUTING.md)
for guidelines for issues and pull requests.
