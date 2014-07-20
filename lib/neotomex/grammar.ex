defmodule Neotomex.Grammar do
  @moduledoc """
  # Neotomex.Grammar

  A Neotomex PEG grammar specifies a directed graph of definitons.
  It consists of:

  - A set of definitions, where each definition consists of an
    `identifier` and an `expression`.
    e.g. `Definition <- Identifier '<-' Expression`
  - The root definition's identifier. This root definition will be
    used as the entry point for matching.
  - A transform function can be associated with a Neotomex expression.
    They are applied after the parse

  The definition data types (see the definition/0 typespec below) are
  verbose and machine readable as opposed to human readable. Typical
  usage will be to compile a grammar from a more machine readable
  format, bootstrapping with Neotomex grammars as necessary.

  Parsing consists of first using the grammar to `match` a valid
  parse tree, and then applying a `transform` node by node to
  this parse tree.

  ## Match

  A grammar consists of definitions which are labeled by an identifier
  and consist of an expression. An expression can be a

  - `terminal`: references a char, string, or regex. It successfully
    matches when it's reference appears at the beginning of the input.
  - `nonterminal`: the identifier of a definition. It matches if the
    identified definition matches.
  - `sequence`: an ordered list of expressions. It succesfully matches
    by folding the expressions against the input.
  - `priority`: an ordered list of expressions. It matches using the
    first successful match of the list.
  - `zero_or_more`: matches when its subexpression greedily matches zero or
    more times.
  - `one_or_more`: matches when its subexpression greedily matches one or
    more times.
  - `zero_or_one`: matches when its subexpression greedily matches zero or
    one times.
  - `and`: matches when its subexpression matches. Consumes no input.
  - `not`: matches when its subexpression does not match. Consumes no input.

  A grammar's definitions come together to form a conditional directed graph.
  Matching starts at the root node and performs a depth first search for the
  first successful route through the tree. This represents the valid parse
  tree for the grammar and input.

  ## Transform

  A transform is a function that modifies the match for an expression. This
  can be used to evaluate the parse tree.

  The default transform is the identity function.

  Transforms must return {:ok, transformed} when successful, where transformed
  is the transformed match.
  """

  # Specification of PEG, in PEG:

  # Hierarchical syntax
  # Grammar    <- Spacing Definition+ EndOfFile
  # Definition <- Identifier LEFTARROW Expression
  # Expression <- Sequence (SLASH Sequence)*
  # Sequence   <- Prefix*
  # Prefix     <- (AND / NOT)? Suffix
  # Suffix     <- Primary (QUESTION / STAR / PLUS)?
  # Primary    <- Identifier !LEFTARROW
  #             / OPEN Expression CLOSE
  #             / Literal / Class / DOT

  # Lexical syntax
  # Identifier <- IdentStart IdentCont* Spacing
  # IdentStart <- [a-zA-Z_]
  # IdentCont  <- IdentStart / [0-9]
  # Literal    <- ['] (!['] Char)* ['] Spacing
  #             / ["] (!["] Char)* ["] Spacing
  # Class      <- '[' (!']' Range)* ']' Spacing
  # Range      <- Char '-' Char / Char
  # Char       <- '\\' [nrt'"\[\]\\]
  #             / '\\' [0-2][0-7][0-7]
  #             / '\\' [0-7][0-7]?
  #             / !'\\' .

  # LEFTARROW  <- '<-' Spacing
  # SLASH      <- '/' Spacing
  # AND        <- '&' Spacing
  # NOT        <- '!' Spacing
  # QUESTION   <- '?' Spacing
  # STAR       <- '*' Spacing
  # PLUS       <- '+' Spacing
  # OPEN       <- '(' Spacing
  # CLOSE      <- ')' Spacing
  # DOT        <- '.' Spacing
  # Spacing    <- (Space / Comment)*
  # Comment    <- '#' (!EndOfLine .)* EndOfLine
  # Space      <- ' ' / '\t' / EndOfLine
  # EndOfLine  <- '\r\n' / '\n' / '\r'
  # EndOfFile  <- !.

  # PEG parser grammar defined in Neotomex internal PEG format
  @peg_root        :grammar
  # @peg_definitions
  # %{:grammar =>
  #   # Hierarchical syntax
  #   {:sequence, [{:nonterminal, :spacing},
  #                {:one_or_more, {:nonterminal, :definition}},
  #                {:nonterminal, :EOF}]},
  #   :definition =>
  #   {:sequence, [{:nonterminal, :identifier},
  #                :LEFTARROW, :expression]},
  #   :expression =>
  #   {:sequence, [{:non_terminal, :seq},
  #                {:zero_or_more, {:sequence,
  #                                 [{:nonterminal, :SLASH},
  #                                  {:nonterminal, :seq}]}}]},
  #   :sequence => {:zero_or_more, {:nonterminal, :prefix}},
  #   :prefix => {:sequence, [{:priority, [:AND, :NOT]}, {:nonterminal, :suffix}]},
  #   :suffix =>
  #   {:sequence, [{:nonterminal, :primary},
  #                {:zero_or_one, {:priority, [{:nonterminal, :QUESTION},
  #                                            {:nonterminal, :STAR},
  #                                            {:nonterminal, :PLUS}]}}]},
  #   :primary =>
  #   {:priority, [{:sequence, [{:nonterminal, :identifier},
  #                             {:not, {:nonterminal, :LEFTARROW}}]},
  #                {:sequence, [{:nonterminal, :OPEN},
  #                             {:nonterminal, :expression},
  #                             {:nonterminal, :CLOSE}]},
  #                {:nonterminal, :literal},
  #                {:nonterminal, :class},
  #                {:nonterminal, :DOT}]},

  #   # Lexical syntax
  #   :identifier =>
  #   {:sequence, [{:nonterminal, :ident_start},
  #                {:zero_or_more, {:nonterminal, :ident_cont}},
  #                {:nonterminal, :SPACING}]},
  #   :ident_start => {:terminal, ~r/^[a-zA-Z_]/},
  #   :ident_cont => {:priority, [{:nonterminal, :ident_start},
  #                               {:terminal, ~r/^[0-9]/}]},
  #   :literal =>
  #   {:priority, [{:sequence, [{:terminal, 39},
  #                             {:zero_or_more,
  #                              {:sequence, [{:not, {:terminal, 39}},
  #                                           {:nonterminal, :char}]}},
  #                             {:terminal, 39},
  #                             {:nonterminal, :spacing}]},
  #                {:sequence, [{:terminal, 34},
  #                             {:zero_or_more,
  #                              {:sequence, [{:not, {:terminal, 34}},
  #                                           {:nonterminal, :char}]}},
  #                             {:terminal, 34},
  #                             {:nonterminal, :spacing}]}]},
  #   :class => {:sequence, [{:terminal, 91},
  #                          {:zero_or_more, {:sequence, [:TODO]}}]},

  #   :LEFTARROW => {:sequence, [{:terminal, "<-"}, {:nonterminal, :spacing}]},
  #   :SLASH => {:sequence, [{:terminal, }, {:nonterminal, :spacing}]}
  #   }


  # Neotomex parses PEG expressions into this internal representation
  @type terminal :: binary | char | Regex.t | nil
  @type nonterminal :: atom

  @type expression :: :empty
                    | {:terminal, terminal}
                    | {:nonterminal, nonterminal}
                    | {:sequence [expression]}
                    | {:priority, [expression]}
                    | {:zero_or_more, expression}
                    | {:one_or_more, expression}
                    | {:zero_or_one, expression}
                    | {:and, expression}
                    | {:not, expression}

  @type definition :: {nonterminal, expression}
  @type transform :: ((term) -> {:ok, term})
  @type expr_trans :: {expression, transform}


  # A match is the result of the Neotomex PEG grammar matching an expression
  @typep match :: {expr_trans, [match] | match | String.t}

  # A grammar contains a root label, and a map of rules keyed by nonterminal label
  @type grammar :: %{root: nonterminal | false,
                     definitions: %{nonterminal => expression},
                     memos: pid() | false}

  @doc """
  Returns an empty grammar.
  """
  @spec new :: grammar
  def new(root \\ false, definitions \\ %{}, memos \\ false) do
    %{root: root, definitions: definitions, memos: memos}
  end

  # @doc """
  # Returns a grammar for parsing PEGs.
  # """
  # @spec peg_grammar :: grammar
  # def peg_grammar do
  #   new(@peg_root, @peg_definitions)
  # end

  @doc """
  Add a rule to the grammar.

  Set the definition as the root of the grammar if not yet set.
  """
  @spec add_rule(grammar, definition) :: grammar
  def add_rule(%{:root => false} = grammar, {ident, _} = definition) do
    add_rule(%{grammar | :root => ident}, definition)
  end
  def add_rule(%{:definitions => definitions} = grammar, definition) do
    %{grammar | :definitions => [definitions | definition]}
  end


  @doc """
  Parse the `input` using the `grammar` by matching a parse tree and
  then applying all transforms.

  ## Examples

      iex> grammar = new(:root, %{root: {{:terminal, ~r/^[0-9]+/},
      ...>                               &String.to_integer/1}})
      iex> parse(grammar, "1")
      {:ok, 1, ""}
      iex> parse(grammar, "100")
      {:ok, 100, ""}

  """
  @spec parse(grammar, binary) :: {:ok, any, binary} | :mismatch | {:error, term}
  def parse(grammar, input) do
    case match(grammar, input) do
      {:ok, match, rest} ->
        {:ok, transform_match(match), rest}
      otherwise ->
        otherwise
    end
  end


  @doc """
  Match the `input` using the grammar.

  NB: Not tail call optimized. Possible?
  """
  @spec match(grammar, binary) :: {:ok, {expression, transform}, binary}
                                | :mismatch | {:error, term}
  def match(%{:root => root, :definitions => definitions} = grammar, input)
      when is_binary(input) do
    match(definitions[root], grammar, input)
  end


  @doc """
  Transform the parse tree returned by match by applying the the
  expressions' transform functions via depth first recursion.

  NB: Not tail call optimized. Possible? Pack rat?

  ## Examples

      iex> transform_match({{nil, fn x -> String.to_integer(x) end},
      ...>                  {{nil, nil}, "1"}})
      1
      iex> transform_match({{nil, fn [x, y] -> x + y end},
      ...>                  [{{nil, nil}, 1}, {{nil, nil}, 1}]})
      2
  """
  @spec transform_match(match) :: any
  def transform_match({{_, nil}, match}) do
    match
  end
  def transform_match({{_, transform_fn}, matches}) when is_list(matches) do
    transform_fn.(for match <- matches, do: transform_match(match))
  end
  def transform_match({{_, transform_fn}, match}) when is_binary(match) do
    transform_fn.(match)
  end
  def transform_match({{_, transform_fn}, match}) do
    transform_fn.(transform_match(match))
  end


  ## Private Functions

  defp match({identifier, _} = expr, grammar, input) when is_atom(identifier) do
    # If no transform is provided, default it to `nil`
    match({expr, nil}, grammar, input)
  end

  defp match(nil, _, _),        do: {:error, :no_root}
  defp match(:empty, _, input), do: {:ok, nil, input}

  # Terminal nodes can be characters [integer], strings, or regexs
  defp match({{:terminal, char}, _} = expr_trans, _, <<char, rest :: utf8>>)
      when is_integer(char) do
    {:ok, {expr_trans, char}, rest}
  end
  defp match({{:terminal, char}, _}, _, _) when is_integer(char) do
    :mismatch
  end
  defp match({{:terminal, terminal}, _} = expr_trans, _, input)
      when is_binary(terminal) do
    case String.split_at(input, String.length(terminal)) do
      {^terminal, rest} ->
        {:ok, {expr_trans, terminal}, rest}
      {_, _} ->
        :mismatch
    end
  end
  defp match({{:terminal, terminal}, _} = expr_trans, _, input) do
    case Regex.run(terminal, input) do
      [""] ->
        # Make sure it's not just an empty match
        case Regex.match?(terminal, input) do
          true ->
            {:ok, {expr_trans, ""}, input}
          false ->
            :mismatch
        end
      nil ->
        :mismatch
      [match] ->
        # Two parts are necessary since the first is being trimmed away
        {^match, rest} = String.split_at(input, String.length(match))
        {:ok, {expr_trans, match}, rest}
    end
  end

  defp match({{:nonterminal, nonterminal}, _} = expr_trans,
             %{:definitions => definitions} = grammar, input) do
    case match(definitions[nonterminal], grammar, input) do
      {:ok, match, rest} ->
        {:ok, {expr_trans, match}, rest}
      otherwise ->
        otherwise
    end
  end

  defp match({{:sequence, _}, _} = expr_trans, grammar, input) do
    match_sequence(expr_trans, grammar, input)
  end

  defp match({{:priority, _}, _} = expr_trans, grammar, input) do
    match_priorities(expr_trans, grammar, input)
  end

  defp match({{:zero_or_more, _}, _} = expr_trans, grammar, input) do
   match_zero_or_more(expr_trans, grammar, input)
  end

  defp match({{:one_or_more, expression}, _} = expr_trans, grammar, input) do
    case match(expression, grammar, input) do
      {:ok, match, input} ->
        match_zero_or_more(expr_trans, grammar, input, [match])
      otherwise ->
        otherwise
    end
  end

  defp match({{:zero_or_one, expression}, _} = expr_trans, grammar, input) do
    case match(expression, grammar, input) do
      :mismatch ->
        {:ok, {expr_trans, nil}, input}
      otherwise ->
        otherwise
    end
  end

  defp match({{:and, expression}, _} = expr_trans, grammar, input) do
    case match(expression, grammar, input) do
      {:ok, _, _} ->
        {:ok, {expr_trans, nil}, input}
      otherwise ->
        otherwise
    end
  end

  defp match({{:not, expression}, _} = expr_trans, grammar, input) do
    case match(expression, grammar, input) do
      {:ok, _, _} ->
        :mismatch
      :mismatch ->
        {:ok, {expr_trans, nil}, input}
      {:error, reason} ->
        {:error, reason}
    end
  end

  # Helper for parsing a sequence of expressions
  defp match_sequence({{:sequence, expressions}, _} = expr_trans, grammar, input) do
    match_sequence(expr_trans, grammar, input, expressions, [])
  end

  defp match_sequence({{:sequence, _}, _} = expr_trans, _, input, [], acc) do
    {:ok, {expr_trans, Enum.reverse(acc)}, input}
  end
  defp match_sequence(expr_trans, grammar, input, [expression | expressions], acc) do
    case match(expression, grammar, input) do
      {:ok, match, input} ->
        match_sequence(expr_trans, grammar, input, expressions, [match | acc])
      otherwise ->
        otherwise
    end
  end


  # Helper for parsing a priority list of expressions
  defp match_priorities({{:priority, expressions}, _} = expr_trans, grammar, input) do
    match_priorities(expr_trans, grammar, input, expressions)
  end

  defp match_priorities(_, _, _, []), do: :mismatch
  defp match_priorities(expr_trans, grammar, input, [expression | expressions]) do
    case match(expression, grammar, input) do
      {:ok, match, input} ->
        {:ok, {expr_trans, match}, input}
      :mismatch ->
        match_priorities(expr_trans, grammar, input, expressions)
      {:error, reason} ->
        {:error, reason}
    end
  end


  # Helper for zero or more (* suffix). Also used for one or more.
  defp match_zero_or_more(expr_trans, grammar, input, acc \\ [])
  defp match_zero_or_more({{_, expression}, _} = expr_trans, grammar, input, acc) do
    case match(expression, grammar, input) do
      {:ok, match, input} ->
        match_zero_or_more(expr_trans, grammar, input, [match | acc])
      :mismatch ->
        {:ok, {expr_trans, Enum.reverse(acc)}, input}
      {:error, reason} ->
        {:error, reason}
    end
  end

end