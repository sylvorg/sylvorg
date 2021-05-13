---
author: Jeet Ray
---

\#!/usr/bin/env mdsh

# current projects

``` example
00321739e2bcd1e57750b641564afebe
```

``` text
shadowrylander/shadowrylander home/shadowrylander
```

# makefile

``` example
cd5dbf3629c558b0104ba8f0937d6816
```

``` makefile
init:
    -git clone --recurse-submodule https://github.com/shadowrylander/shadowrylander home/shadowrylander
.DEFAULT_GOAL := init

rebuild:
    chmod +x ./wheee
    ./wheee --use-hash ${HASH} -H make

switch:
    chmod +x ./wheee
    ./wheee --use-hash ${HMASH} -H make --home-manager
    ./wheee --use-hash ${RMASH} -H make --home-manager
```

# previous projects

## bakery

My pride and joy; based off of [Andrew
Moffat's](https://github.com/amoffat)
[sh](https://amoffat.github.io/sh/), this python module allows you to
import shell commands as modules as well. For example:

``` python
from baker.y import git
git.clone(
    b = "master",
    recurse_submodule = True,
    "https://github.com/shadowrylander/shadowrylander",
    "~/shadowrylander",
    _run = True,
)
print(git(C = "~/shadowrylander").status())
```

You might be thinking to yourself, "Cool!" And also, "…Wait what?"

### breaking it down

------------------------------------------------------------------------

``` example
55cd2acb7ad806fd92712b939e778c43
```

``` python
from baker.y import git
```

This will import the `git` utility from your path as a `bakery` object;
note, however, that while the statement is importing `git` from
`baker.y`, there are actually two submodules in play here: `baker` with
a `y`, and `baker` with an `i`.

To facilitate the use of `git(C = [path]).status()`, the latter
submodule must be used. This also prevents the use of something like
`git()`, but, since that just shows the help text, one can simply use
`git.help()` instead.

------------------------------------------------------------------------

``` example
063c7e217d5d2bc6b70a3fd3633eca26
```

``` python
git.clone(
    b = "master",
    recurse_submodule = True,
    "https://github.com/shadowrylander/shadowrylander",
    "~/shadowrylander",
    _run = True,
)
```

------------------------------------------------------------------------

``` example
c67b0687e4684ea7b068fc3e90e3d2b6
```

``` python
print(git(C = "~/shadowrylander").status())
```

### want more information?

Get it here! <https://gitlab.com/picotech/bakery>

Also: eh heh heh… yeah… I like `gitlab's` grouping system… ***sheepish
grin***
