[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/jcs-poptip.svg)](https://jcs-emacs.github.io/jcs-elpa/#/jcs-poptip)
<a href="https://jcs-emacs.github.io/"><img align="right" src="https://raw.githubusercontent.com/jcs-emacs/badges/master/others/built-with/dark.svg" alt="Built with"></a>

<picture>
  <source media="(prefers-color-scheme: light)" srcset="https://raw.githubusercontent.com/jcs-emacs/jcs-elpa/master/docs/etc/logo/light/sink.png">
  <source media="(prefers-color-scheme: dark)" srcset="https://raw.githubusercontent.com/jcs-emacs/jcs-elpa/master/docs/etc/logo/dark/sink.png">
  <img width="20%" align="right" src="">
</picture>

# jcs-poptip
> Generic popup tip

[![CI](https://github.com/jcs-emacs/jcs-poptip/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-emacs/jcs-poptip/actions/workflows/test.yml)

This package tries to show the information as hard as possible. Think of this as
an all-in-one package. It uses these packages to grab the information:

- [lsp-ui-doc][] - LSP is smart so don't often need to try other solutions.
- `describe-thing` - Built-in describe thing at point
- [company][] - Show company documentation.
- [company-dict][] - Like built-in `describe-thing` but try to show information
from completion dictionary.
- [preview-it][] - Preview `file`, `image`, `url`, and color at the point.
- [define-it][] - Define, translate wiki the word.

## 🔨 Usage

```
M-x jcs-poptip
```

## 🛠️ Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either 
clone and make pull requests to this repository. Or you can 
clone the project and establish your own branch of this tool. 
Any methods are welcome!

### 🔬 Development

To run the test locally, you will need the following tools:

- [Eask](https://emacs-eask.github.io/)
- [Make](https://www.gnu.org/software/make/) (optional)

Install all dependencies and development dependencies:

```sh
$ eask install-deps --dev
```

To test package's installation:

```sh
$ eask package
$ eask install
```

To test compilation:

```sh
$ eask compile
```

**🪧 The following steps are optional, but we recommend you follow these lint results!**

The built-in `checkdoc` linter:

```sh
$ eask lint checkdoc
```

The standard `package` linter:

```sh
$ eask lint package
```

*📝 P.S. For more information, find the Eask manual at https://emacs-eask.github.io/.*

## ⚜️ License

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

See [`LICENSE`](./LICENSE.txt) for details.


[lsp-ui-doc]: https://github.com/emacs-lsp/lsp-ui#lsp-ui-doc
[preview-it]: https://github.com/jcs-elpa/preview-it
[company]: https://github.com/company-mode/company-mode
[company-dict]: https://github.com/hlissner/emacs-company-dict
[define-it]: https://github.com/jcs-elpa/define-it
