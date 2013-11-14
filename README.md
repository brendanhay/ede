# ED-E

* [Introduction](#introduction)
* [Usage](#usage)
* [Contribute](#contribute)
* [Licence](#licence)


## Introduction

ED-E is a templating language written in Haskell with a specific set of features:

* Logicless within reason. A small set of consistent predicates
and expressions for formatting and presentational logic are provided.
* Secure. No arbitrary code evaluation, with input data required to be fully specified
at render time.
* Stateless. Parsing and rendering are separate steps so that loading/parsing/embedding
of the template can optionally be done ahead of time.
* Markup agnostic. ED-E is used to write out everything from configuration files for
system services to HTML, or formatted emails.
* Easy to debug. All parsing, type checking, and rendering steps report helpful
error messages with line/column metadata. Variable shadowing, unprintable expressions,
type coercion, and unbound variable access are all considered errors.


## Usage

**template.ede**:

```HTML+Django
Hello, {{ name }}!
```

**Main.hs**:

```haskell
import           Control.Applicative ((<$>))
import           Control.Monad       ((>=>))
import qualified Data.Text.Lazy.IO   as LText
import           Text.EDE.Aeson      (parse, render, toObject, (.=))

main :: IO ()
main = (parse >=> flip render env) <$> LText.readFile "template.ede" >>= print
  where
    name = "World" :: String
    env  = toObject ["name" .= name]
```

A set of syntatic/semnatic fragments for all expressions can be found in the [tests](test/resources).


## Contribute

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/ed-e/issues).


## Licence

ED-E is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
