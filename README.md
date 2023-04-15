[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/jcs-poptip.svg)](https://jcs-emacs.github.io/jcs-elpa/#/jcs-poptip)
<a href="https://jcs-emacs.github.io/"><img align="right" src="https://raw.githubusercontent.com/jcs-emacs/badges/master/others/built-with/dark.svg" alt="Built with"></a>

<picture>
  <source media="(prefers-color-scheme: light)" srcset="https://raw.githubusercontent.com/jcs-emacs/jcs-elpa/master/docs/etc/logo/light/sink.png">
  <source media="(prefers-color-scheme: dark)" srcset="https://raw.githubusercontent.com/jcs-emacs/jcs-elpa/master/docs/etc/logo/dark/sink.png">
  <img width="25%" align="right" src="">
</picture>

# jcs-poptip
> Generic popup tip

[![CI](https://github.com/jcs-emacs/jcs-poptip/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-emacs/jcs-poptip/actions/workflows/test.yml)

This package tries to show the information as hard as possible. Think of this as
an all-in-one package. It uses these packages to grab the information:

- [lsp-ui-doc][] - LSP is smart so don't often need to try other solutions.
- `describe-thing` - Built-in describe thing at point
- [company-dict][] - Like built-in `describe-thing` but try to show information
from completion dictionary.
- [preview-it][] - Preview `file`, `image`, `url`, and color at the point.
- [define-it][] - Define, translate wiki the word.

## 🔨 Usage

```
M-x jcs-poptip
```

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either 
clone and make pull requests to this repository. Or you can 
clone the project and establish your own branch of this tool. 
Any methods are welcome!

[lsp-ui-doc]: https://github.com/emacs-lsp/lsp-ui#lsp-ui-doc
[preview-it]: https://github.com/jcs-elpa/preview-it
[company-dict]: https://github.com/hlissner/emacs-company-dict
[define-it]: https://github.com/jcs-elpa/define-it
