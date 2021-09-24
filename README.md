# Table of Contents

1.  [Copyright (C) 2018 Entropy](#org17a7a02)
2.  [Commentary:](#org376ddf7)
3.  [Configuration:](#org8de3668)

<a id="org17a7a02"></a>

# Copyright (C) 2018 Entropy

    Author:        Entropy <bmsac0001@gmail.com>
    Maintainer:    Entropy <bmsac001@gmail.com>
    URL:           https://github.com/c0001/entropy-emacs-extensions/blob/master/entropy-emacs-extensions-load.el
    Package-Version: 1.1.0
    Compatibility: GNU Emacs emacs-version;
    Package-Requires: ((emacs "25.3") (cl-lib "0.5"))

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.


<a id="org376ddf7"></a>

# Commentary:

**This project was collection of emacs extensions used for
[entropy-emacs](https://github.com/c0001/entropy-emacs).**

Submodules under this project are collected from the corresonding
package repo of melpa or elpa extensions, version respected by
[entropy-emacs](https://github.com/c0001/entropy-emacs) for the config tieing, not up-to-date with upstream, but
will update in term of the updates of `entropy-emacs`.

Project can be used individually in your own wish but without any
warranty for the reason mentioned above. Load the loader
`entropy-emacs-extensions-load.el` in case for that.

For each `entropy-emacs` user, before using this repo whenever be
after each updating or at the initialization, run the `make all`
for finishing both of the updating or initialization, this was
required at top level.

This is the way for `entropy-emacs` ecosystem complements, the
local-melpa \`package-archives' post way. The main purpose for
maintain all `entropy-emacs` extensions as the commit remained
submodules is to guarantee the extensions compatibility with
`entropy-emacs`. i.e. The designation for
`entropy-emacs-extensions` is to make it as the \`package-archives\`
as what did as [melpa](https://melpa.org) do to install all `entropy-emacs` depended
commit specific extensions froms this project, as that for what,
`entropy-emacs` forked `melpa` to built the `entropy-emacs-melpa`
using for [package.el](https://melpa.org/#/getting-started), on this way, this package will set the
`package-archives` to as form as `("entropy-emacs"
. "path-to-local-malpa")`.

Rely on this usage you selected, the customized variable
`entropy/emacs-ext-elpkg-get-type` of `entropy-emacs` was what you
needed to set to enable this project efficiently, the valid value
of thus is a symbol: `entropy-emacs-extenisons-project`

This variable is pre-defined as 'origin' in `entropy-emacs` so
that you should specified it in your `custom.el` before start
`entropy-emacs`.

The last needed notice is to set `entropy-emacs` customized
variable `entropy/emacs-ext-eemacs-elpkg-archive-project-dir`
correctly which is the path of this project let `entropy-emacs`
locates this project correctly, of cource write it in your
`custom.el` also.


<a id="org8de3668"></a>

# Configuration:

Although this package are originally and designed for
entropy-emacs, but the functional parts are independently beside
it, thus the common usage method are proper for this package too,
this means that you can use this project for your own emacs
configuration too.

    (add-to-list 'load-path "path-of-this")
    (require 'entropy-emacs-extensions-load)


<a id="orge9efe3a"></a>
