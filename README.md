Victor's System Resources
=========================

This is a repo organizing my system configuration and
scripts.

Calling a script from the `scripts/` folder is done with the
`vsr` script.

For instance,

`vsr apt pkg-is-installed -v vim`
 
will run `scripts/apt/pkg-is-installed -v vim`


The completion script, `vsr_complete` should be sourced by
bash on startup to provide TAB completions.


Instalation
===========

"Installing" vsr is simple. We mainly need to export a variable `VSR_ROOT`
pointing to the root of the repo on login.

To do that, append the following lines code to `$HOME/.profile`

```bash
export VSR_ROOT=/path/to/repo/root
. $VSR_ROOT/config/bash/profile
```

and

```bash
. $VSR_ROOT/config/bash/bashrc
```

to `$HOME/.bashrc`

vsr will be up and running on the next login.