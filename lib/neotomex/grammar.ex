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


  @typep match :: [match]   # sequence or priority
                | match     # the rest
                | String.t  # terminal

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
  Parse the provided input using the grammar.
  """
  @spec parse(binary, grammar) :: {:ok, {expression, transform}, binary}
                                | :mismatch | {:error, term}
  def parse(input, %{:root => root, :definitions => definitions} = grammar)
      when is_binary(input) do
    parse(input, grammar, definitions[root])
  end

  defp parse(input, grammar, {identifier, _} = expr) when is_atom(identifier) do
    # If no transform is provided, default it to `nil`
    parse(input, grammar, {expr, nil})
  end

  defp parse(_, _, nil),        do: {:error, :no_root}
  # defp parse("", _, _),         do: {:ok, [], ""}
  defp parse(input, _, :empty), do: {:ok, nil, input}

  # Terminal nodes can be characters [integer], strings, or regexs
  defp parse(<<char, rest :: utf8>>, _, {{:terminal, char}, _} = expr_trans)
      when is_integer(char) do
    return(rest, expr_trans, char)
  end
  defp parse(_, _, {{:terminal, char}, _}) when is_integer(char) do
    :mismatch
  end
  defp parse(input, _, {{:terminal, terminal}, _} = expr_trans)
      when is_binary(terminal) do
    case String.split_at(input, String.length(terminal)) do
      {^terminal, rest} ->
        return(rest, expr_trans, terminal)
      {_, _} ->
        :mismatch
    end
  end
  defp parse(input, _, {{:terminal, terminal}, _} = expr_trans) do
    case Regex.run(terminal, input) do
      [""] ->
        # Make sure it's not just an empty match
        case Regex.match?(terminal, input) do
          true ->
            return(input, expr_trans, "")
          false ->
            :mismatch
        end
      nil ->
        :mismatch
      [match] ->
        # Two parts are necessary since the first is being trimmed away
        {^match, rest} = String.split_at(input, String.length(match))
        return(rest, expr_trans, match)
    end
  end

  defp parse(input, %{:definitions => definitions} = grammar,
             {{:nonterminal, nonterminal}, _} = expr_trans) do
    transform_parse(case parse(input, grammar, definitions[nonterminal]) do
                      {:ok, match, input} ->
                        return(input, expr_trans, match)
                      otherwise ->
                        otherwise
                    end, expr_trans)
  end

  defp parse(input, grammar, {{:sequence, expressions}, _} = expr_trans) do
    IO.puts "whee"
    IO.inspect parse_sequence(input, grammar, expressions)
    IO.inspect expr_trans
    transform_parse(parse_sequence(input, grammar, expressions), expr_trans)
  end

  defp parse(input, grammar, {{:priority, expressions}, _} = expr_trans) do
    transform_parse(parse_priorities(input, grammar, expressions), expr_trans)
  end

  defp parse(input, grammar, {{:zero_or_more, expression}, _} = expr_trans) do
   transform_parse(parse_zero_or_more(input, grammar, expression), expr_trans)
  end

  defp parse(input, grammar, {{:one_or_more, expression}, _} = expr_trans) do
    transform_parse(case parse(input, grammar, expression) do
                      {:ok, match, input} ->
                        parse_zero_or_more(input, grammar, expression, [match])
                      otherwise ->
                        otherwise
                    end, expr_trans)
  end

  defp parse(input, grammar, {{:zero_or_one, expression}, _} = expr_trans) do
    case parse(input, grammar, expression) do
      :mismatch ->
        return(input, expr_trans, nil)
      otherwise ->
        otherwise
    end
  end

  defp parse(input, grammar, {{:and, expression}, _} = expr_trans) do
    transform_parse(case parse(input, grammar, expression) do
                      {:ok, _, _} ->
                        return(input, expr_trans, expression)
                      otherwise ->
                        otherwise
                    end, expr_trans)
  end

  defp parse(input, grammar, {{:not, expression}, _} = expr_trans) do
    transform_parse(case parse(input, grammar, expression) do
                      {:ok, _, _} ->
                        :mismatch
                      :mismatch ->
                        return(input, expr_trans, expression)
                      {:error, reason} ->
                        {:error, reason}
                    end, expr_trans)
  end

  # Helper for parsing a sequence of expressions
  defp parse_sequence(input, grammar, expressions, acc \\ [])
  defp parse_sequence(input, _, [], acc) do
    {:ok, Enum.reverse(acc), input}
  end
  defp parse_sequence(input, grammar, [expression | expressions], acc) do
    case parse(input, grammar, expression) do
      {:ok, matches, input} ->
        parse_sequence(input, grammar, expressions, [matches | acc])
      otherwise ->
        otherwise
    end
  end

  # Helper for parsing a priority list of expressions
  defp parse_priorities(_, _, []) do
    :mismatch
  end

  defp parse_priorities(input, grammar, [expression | expressions]) do
    case parse(input, grammar, expression) do
      {:ok, matches, input} ->
        {:ok, matches, input}
      :mismatch ->
        parse_priorities(input, grammar, expressions)
      {:error, reason} ->
        {:error, reason}
    end
  end

  # Helper for zero or more (* suffix)
  defp parse_zero_or_more(input, grammar, expression, acc \\ [])
  defp parse_zero_or_more(input, grammar, expression, acc) do
    case parse(input, grammar, expression) do
      {:ok, match, input} ->
        parse_zero_or_more(input, grammar, expression, [match | acc])
      :mismatch ->
        {:ok, {expression, Enum.reverse(acc)}, input}
      {:error, reason} ->
        {:error, reason}
    end
  end

  # Compose an `ok` return by running the transform function on the match -- kills
  # tail recursion :/
  @spec return(binary, expr_trans, match) :: {:ok, {expression, match}, binary}
  defp return(rest, {expression, nil}, match) do
    {:ok, {expression, match}, rest}
  end
  defp return(rest, {expression, transform}, match) do
    # If there were multiple matches, apply them to the transform as args
    case (if is_list(match) do
            apply(transform, match)
          else
            transform.(match)
          end) do
      {:ok, transformed} ->
        {:ok, {expression, transformed}, rest}
    end
  end

  # Helper function for applying transforms to parse return values.
  defp transform_parse({:ok, {_, match}, rest}, expr_trans) do
    #IO.puts "calling"
    #IO.inspect match
    #IO.inspect expr_trans

    return(rest, expr_trans, match)
  end
  defp transform_parse(otherwise, _) do
    IO.puts "otherwise"
    IO.inspect otherwise
    otherwise
  end
end