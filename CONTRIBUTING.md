# Contributing

A change is a series of commits that is read one commit at a time.
One commit does one thing, passes the check on its own, and says in
its message why it was made.  Three things are particular to this
repository.

## The check

    make check

It compiles `syringe.el` with warnings treated as errors and fails if
Emacs would indent a line differently.  Every commit of a series must
pass it, not only the last one:

    git rebase -x 'make check' <base>

There are no automated tests.

## Areas

The subject of a commit starts with the part that it changes.  For
`syringe.el` that is one of its sections:

    diff      the diff buffer and the commands that start a review
    dots      the dots, the conversations behind them, finding a line again
    chat      the conversation buffer
    robot     what the robot is told and what is done with its answers
    engine    the three functions that run the robot
    reviews   saving reviews and opening them again
    keys      the key bindings at the end of the file

The other files are named by themselves: `build` for the Makefile and
`tools`, `doc`, `gitignore`.

## Merging

History is linear.  A series is rebased onto `master` and lands as its
commits, without a merge commit, so the commit messages are the only
record and each has to stand on its own.
