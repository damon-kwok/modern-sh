[![GitHub license](https://img.shields.io/github/license/damon-kwok/modern-sh?logo=gnu&.svg)](https://github.com/damon-kwok/modern-sh/blob/master/COPYING)
[![Sponsor](https://img.shields.io/badge/Support%20Me-%F0%9F%92%97-ff69b4.svg)](https://www.patreon.com/DamonKwok)
<!-- [![MELPA](http://melpa.org/packages/modern-sh-badge.svg)](http://melpa.org/#/modern-sh) -->
<!-- [![MELPA Stable](http://stable.melpa.org/packages/modern-sh-badge.svg)](http://stable.melpa.org/#/modern-sh) -->

# Modern sh

An Emacs minor mode for editing shell script.

<!-- - Screenshot -->

<!-- ![screenshot](https://github.com/damon-kwok/modern-sh/blob/master/screenshot.png) -->

## Features

- [X] Modern syntax highlighting
- [x] Auto format on save
- [x] Code navigation (using `imenu`)
- [x] Go to definition (using `ctags`)
<!-- - [ ] Code completion (using `company-mode`) -->
<!-- - [ ] Indentation -->
<!-- - [x] TODO highlighting -->
<!-- - [x] Rainbow delimiters -->
<!-- - [x] Whitespace character dsiplay -->
<!-- - [x] Fill column indicator -->
<!-- - [x] Workspace support -->
<!-- - [x] Code folding -->
<!-- - [x] Compilation integration -->

## Installation

### ~~Using MELPA~~ ([COMING SOON](https://github.com/melpa/melpa/pull/7056))
This package can be obtain from
[MELPA](http://melpa.org/#/modern-sh) or
[MELPA Stable](http://stable.melpa.org/#/modern-sh). The `master`
branch is continuously deployed to `MELPA`, and released versions are
deployed to `MELPA Stable`.

<kbd>M-x package-install [RET] modern-sh [RET]</kbd>

```elisp
(require 'modern-sh)
(add-hook 'sh-mode-hook #'modern-sh-mode)
```
