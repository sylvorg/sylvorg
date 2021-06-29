Settings
========

aiern's behaviour can be adjusted by setting some variables.  The list
of all available variables and their current values can be inspected
by doing::

  M-x customize-group RET aiern RET

To change the value of a variable, you can use this interface, or add
a ``setq`` form to your Emacs init file, preferably before aiern is
loaded. [#order]_

.. code-block:: elisp

   (setq aiern-shift-width 0)
   ;; Load aiern
   (require 'aiern)

What follows is a non-exhaustive list of the most relevant
customization options.


The initial state
-----------------

The initial state of a buffer is determined by its major mode.  aiern
maintains an association between major modes and their corresponding
states, which is most easily modified using the function
:elisp:ref:`aiern-set-initial-state`.

.. elisp:autofunction:: aiern-set-initial-state

If no state can be found, aiern uses the default initial state.

.. elisp:autovariable:: aiern-default-state

Alternatively, it is possible to select the initial state based on the
buffer *name* rather than its major mode.  This is checked first, so
it takes precedence over the other methods for setting the state.

.. elisp:autovariable:: aiern-buffer-regexps


Keybindings and other behaviour
-------------------------------

aiern comes with a rich system for modifying its key bindings
:ref:`chapter-keymaps`.  For the most common tweaks, the following
variables are available.

.. elisp:autovariable:: aiern-toggle-key

.. elisp:autovariable:: aiern-want-C-i-jump

.. elisp:autovariable:: aiern-want-C-u-delete

.. elisp:autovariable:: aiern-want-C-u-scroll

.. elisp:autovariable:: aiern-want-C-d-scroll

.. elisp:autovariable:: aiern-want-C-w-delete

.. elisp:autovariable:: aiern-want-C-w-in-emacs-state

.. elisp:autovariable:: aiern-want-Y-yank-to-eol

.. elisp:autovariable:: aiern-disable-insert-state-bindings


Search
------

.. elisp:autovariable:: aiern-search-module

.. elisp:autovariable:: aiern-regexp-search

.. elisp:autovariable:: aiern-search-wrap

.. elisp:autovariable:: aiern-flash-delay

.. elisp:autovariable:: aiern-ex-hl-update-delay


Indentation
-----------

.. elisp:autovariable:: aiern-auto-indent

.. elisp:autovariable:: aiern-shift-width

.. elisp:autovariable:: aiern-shift-round

.. elisp:autovariable:: aiern-indent-convert-tabs


Cursor movement
---------------

In standard Emacs terms, the cursor is generally understood to be
located between two characters.  In Vim, and therefore also aiern, this
is the case in insert state, but in other states the cursor is
understood to be *on* a character, and that this character is not a
newline.

Forcing this behaviour in Emacs is the source of some potentially
surprising results (especially for traditional Emacs users---users
used to Vim may find the default behavior to their satisfaction). Many
of them can be tweaked using the following variables.

.. elisp:autovariable:: aiern-repeat-move-cursor

.. elisp:autovariable:: aiern-move-cursor-back

.. elisp:autovariable:: aiern-move-beyond-eol

.. elisp:autovariable:: aiern-cross-lines

.. elisp:autovariable:: aiern-respect-visual-line-mode

.. elisp:autovariable:: aiern-track-eol


Cursor display
--------------

A state may change the appearance of the cursor.  Use the variable
:elisp:ref:`aiern-default-cursor` to set the default cursor, and the
variables ``aiern-normal-state-cursor``, ``aiern-insert-state-cursor``
etc. to set the cursors for specific states.  The acceptable values
for all of them are the same.

.. elisp:autovariable:: aiern-default-cursor


Window management
-----------------

.. elisp:autovariable:: aiern-auto-balance-windows

.. elisp:autovariable:: aiern-split-window-below

.. elisp:autovariable:: aiern-vsplit-window-right


Parenthesis highlighting
------------------------

These settings concern the integration between aiern and
``show-paren-mode``.  They take no effect if this mode is not enabled.

.. elisp:autovariable:: aiern-show-paren-range

.. elisp:autovariable:: aiern-highlight-closing-paren-at-point-states


Miscellaneous
-------------

.. elisp:autovariable:: aiern-want-fine-undo

.. elisp:autovariable:: aiern-undo-system

.. elisp:autovariable:: aiern-backspace-join-lines

.. elisp:autovariable:: aiern-kbd-macro-suppress-motion-error

.. elisp:autovariable:: aiern-mode-line-format

.. elisp:autovariable:: aiern-mouse-word

.. elisp:autovariable:: aiern-bigword

.. elisp:autovariable:: aiern-esc-delay

.. elisp:autovariable:: aiern-intercept-esc

.. elisp:autovariable:: aiern-kill-on-visual-paste

.. elisp:autovariable:: aiern-echo-state

.. elisp:autovariable:: aiern-complete-all-buffers


.. rubric:: Footnotes

.. [#order] Strictly speaking, the order only matters if the variable
   affects the way aiern is loaded.  This is the case with some
   variables.
