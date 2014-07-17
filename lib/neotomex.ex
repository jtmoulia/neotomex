defmodule Neotomex do
  @non_terminals []

  # `use` callback
  @doc false
  defmacro __using__(_opts) do
    quote do
      import Neotomex
    end
  end

  @doc """
  This macro defines a new PEG definition, with the scope of the grammar
  limited to the provided module.

  The identifier names the definition's non-terminal node, the
  expression is matched against the provided input, and the body
  is evaluated upon a correct match.
  """
  defmacro pegdef(identifier, expression, do: body) do
    quote do
      unquote(IO.inspect(identifier))
      unquote(IO.inspect(expression))
      unquote(IO.inspect(body))
    end
  end
end
