defmodule Neotomex.ExGrammar do
  ## Interface Macros [TODO]

  @doc false
  defmacro __using__(_opts) do
    quote do
      import Neotomex.ExGrammar, only: :macros
      @root false

      @_root_def nil
      @_neotomex_definitions %{}

      @before_compile Neotomex.ExGrammar
    end
  end

  @doc false
  defmacro __before_compile__(_env) do
    quote unquote: false do
      if @_root_def == nil do
        throw "you must set a root defintion"
      end

      @_neotomex_grammar Neotomex.Grammar.new(@_root_def, @_neotomex_definitions)

      IO.inspect @_neotomex_definitions
      IO.inspect Macro.escape(@_neotomex_grammar)
      def parse(input) do
        Neotomex.Grammar.parse(unquote(Macro.escape(@_neotomex_grammar)), input)
      end
    end
  end

  defmacro define(identifier, expr, body) do
    neo_expr = case Neotomex.PEG.parse_expression(expr) do
      :mismatch ->
        throw "invalid expression"
      {:ok, expr} ->
        expr
    end

    quote bind_quoted: [identifier: identifier,
                        def_name: :"transform_#{Atom.to_string(identifier)}",
                        neo_expr: Macro.escape(neo_expr),
                        body: Macro.escape(body)] do
      IO.inspect neo_expr
      if @root do
        if @_root_def == nil do
          @_root_def identifier
        else
          throw "only one root can be set"
        end
      end

      @_neotomex_definitions Dict.put(@_neotomex_definitions, identifier, neo_expr)

      for {:->, _context, [[branch_head], branch_body]} <- body[:do] do
        def unquote(def_name)(unquote(branch_head)), do: unquote(branch_body)
      end
    end
  end
end