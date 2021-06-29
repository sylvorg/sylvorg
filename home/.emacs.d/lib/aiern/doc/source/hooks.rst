Hooks
=====

A *hook* is a list of functions that are executed when certain events
happen.  Hooks are modified with the Emacs function ``add-hook``.
aiern provides entry and exit hooks for all its states.  For example,
when switching from normal state to insert state, all functions in
``aiern-normal-state-exit-hook`` and ``aiern-insert-state-entry-hook``
are executed.

It is guaranteed that the exit hook will be executed before the entry
hook on all state switches.

During the hook execution, the variables ``aiern-next-state`` and
``aiern-previous-state`` contain information about the states being
switched to and from, respectively.
