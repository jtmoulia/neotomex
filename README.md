# Neotomex

**Obligatory Alpha Quality Disclaimer**

A [PEG](http://bford.info/packrat/) implementation with an Elixir
interface.

While inspired by [neotoma](https://github.com/seancribbs/neotoma),
Neotomex doesn't use functional composition. Instead, it creates
a data structure representing a grammar, and then applies the
data structure to an input.

NB: For now, Neotomex is a recursive rather than packrat parser.


## Usage

By taking advantage of pattern matching and Elixir's macros,
Neotomex provides a fresh DSL for specifying grammars.

Here's a simple grammar for parsing a number:

```elixir
defmodule Number do
  use Neotomex.ExGrammar

  @root true
  define :number, "digit+" do
    digits -> digits |> Enum.join |> String.to_integer
  end

  define :digit, "[0-9]"
end

42 = Number.parse! "42"
```


## Learning More

Check out the module docs to learn more about how Neotomex
specifies grammars, and how to write your own:

```elixir
iex> h Neotomex.Grammar
iex> h Neotomex.ExGrammar
```

Look in `examples/` for existing usage.


Copyright (c) Thomas Moulia, 2014
