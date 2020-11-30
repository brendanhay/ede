# ED-E

[![MPL2][license-badge]][license]
[![Build][build-badge]][build]
[![Hackage][hackage-badge]][hackage]
[![Nix][nix-badge]][nix]
[![Cachix][cachix-badge]][cachix]

[license]: https://opensource.org/licenses/MPL-2.0
[license-badge]: https://img.shields.io/badge/License-MPL%202.0-blue.svg
[build]: https://github.com/brendanhay/ede/actions
[build-badge]: https://github.com/brendanhay/ede/workflows/Build/badge.svg
[hackage]: http://hackage.haskell.org/package/ede
[hackage-badge]: https://img.shields.io/hackage/v/ede.svg
[nix]: https://nixos.org
[nix-badge]: https://img.shields.io/badge/builtwith-nix-purple.svg
[cachix]: https://amazonka.cachix.org
[cachix-badge]: https://img.shields.io/badge/cachix-amazonka-purple.svg

* [Introduction](#introduction)
* [Syntax](#syntax)
* [Contribute](#contribute)
* [Licence](#licence)


## Introduction

ED-E is a templating language written in Haskell with a specific set of features:

* **Logicless (within reason):** A small set of consistent predicates and expressions for formatting and presentational logic are provided.
* **Secure:** No arbitrary code evaluation, with input data required to be fully specified at render time.
* **Stateless:** Parsing and rendering are separate steps so that loading, parsing, include resolution, and embedding of the compiled template can optionally be done ahead of time, amortising cost.
* **Markup agnostic:** ED-E is used to write out everything from configuration files for system services, to HTML and formatted emails.
* **Control over purity:** Users can choose pure or IO-based resolution of `include` expressions.
* **No surprises:** All parsing, type assurances, and rendering steps report helpful error messages with line/column metadata. Variable shadowing, unprintable expressions, implicit type coercion, and unbound variable access are all treated as errors errors.

Please see the [documentation](http://brendanhay.github.io/ede/Text-EDE.html)
for more information.


## Syntax

A set of syntatic/semantic fragments for all supported expressions can be
found in the [tests](test/resources) or the [documentation](http://brendanhay.github.io/ede/Text-EDE.html#syntax).


## Contribute

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/ede/issues).


## Licence

ED-E is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
