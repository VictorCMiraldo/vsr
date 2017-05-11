Victor's System Resources
=========================

This is a repo organizing my system configuration and
scripts.

Calling a script from the '''scripts/''' folder is done with the
'''vsr''' script.

For instance,

'''
vsr apt pkg-is-installed -v vim
'''

will run '''scripts/apt/pkg-is-installed -v vim'''


The completion script, '''vsr_complete''' should be sourced by
bash on startup to provide TAB completions.