Internals
=========

Command properties
------------------

aiern defines *command properties* to store information about commands
[#command]_, such as whether they should be repeated.  A command
property is a ``:keyword`` with an associated value, e.g.
``:repeat nil``.

.. elisp:autofunction:: aiern-add-command-properties

.. elisp:autofunction:: aiern-set-command-properties

.. elisp:autofunction:: aiern-get-command-properties

.. elisp:autofunction:: aiern-get-command-property

.. elisp:autofunction:: aiern-define-command


For setting repeat properties, use the following functions:

.. elisp:autofunction:: aiern-declare-repeat

.. elisp:autofunction:: aiern-declare-not-repeat

.. elisp:autofunction:: aiern-declare-change-repeat


.. rubric:: Footnotes

.. [#command] In this context, a *command* may mean any aiern motion,
   text object, operator or indeed other Emacs commands, which have
   not been defined through the aiern machinery.
