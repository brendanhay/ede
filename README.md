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

Here's a contrived example demonstrating some of the expressions ED-E supports:

**template.ede**:

```HTML+Django
{#
  Conditional expressions test the result of the predicate.

  In this case, because name is a variable it's presence (or absence) is tested.

  The only exception to this is if name resolved to a boolean value in the
  environment it's value would used as the predicate.
#}
{% if name %}
  Hello, {{ name }}!
{% else %}
  :(
{% endif %}

{% for var in list %}
  {#
     The binding <name>.loop.* contains loop metadata which can be found
     on line 153 of Compiler.hs
  #}
  {% if var.loop.first %}
  first!
  {% endif %}

  {% if var.loop.last %}
  last.
  {% endif %}

  {# The value of a loop binding is access using <name>.value (or <name>.key for a hash key) #}
  {{ var.loop.index0 }} : {{ var.value }}
{#
  Specifying an else block within a for loop will cause the block to be evaluated
  if the variable target of the for loop is absent or empty.
#}
{% else %}
  Empty list!
{% endfor %}
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
    env = toObject
        [ "name" .= "World"
        , "list" .= [1, 2, 3]
        , ""
        ]
```

**Result**:

```
  Hello, World!

  first!
  0 : 1
  1 : 2
  last.
  2 : 3
```

A set of syntatic/semnatic fragments for all supported expressions can be found in the [tests](test/resources).


## Contribute

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/ed-e/issues).


## Licence

ED-E is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
