defmodule Neotomex.Grammar do
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

  # @peg_grammar = %{:root => :grammar,
  #                  :rules =>
  #                  %{:grammar =>
  #                    {:sequence, [:spacing, {:+, definition}, :end_of_file]},
  #                    :definition =>
  #                    {:sequence, [:identifier, :LEFT_ARROW, :expression]},
  #                    :expression =>
  #                    {:sequence, [{:non_terminal, :sequence},
  #                                 {:*, {:sequence, [{:non_terminal, :SLASH},
  #                                                   :sequence]}}]},
  #                    :prefix => {:sequence, [{:priority, [:AND, :NOT]},
  #                                            {:non_terminal, :suffix}}}]}}}


  # Neotomex parses PEG expressions into this internal representation
  @type nonterminal :: atom
  @type terminal :: binary

  @type expression :: :empty
                    | {:terminal, binary}
                    | {:nonterminal, atom}
                    | {:sequence [expression]}
                    | {:priority, [expression]}
                    | {:zero_or_more, expression}
                    | {:one_or_more, expression}
                    | {:zero_or_one, expression}
                    | {:and, expression}
                    | {:not, expression}

  @type definition :: {nonterminal, expression}


  @typep match :: {atom, [match]}

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
  @spec parse(binary, grammar) :: {:ok, [term], binary} | :mismatch | {:error, term}
  def parse(input, %{:root => root, :definitions => definitions} = grammar)
      when is_binary(input) do
    parse(input, grammar, definitions[root])
  end

  defp parse(_, _, nil),        do: {:error, :no_root}
  # defp parse("", _, _),         do: {:ok, [], ""}
  defp parse(input, _, :empty), do: {:ok, nil, input}

  defp parse(input, _, {:terminal, terminal}) do
    case Regex.run(terminal, input) do
      [""] ->
        # Make sure it's not just an empty match
        case Regex.match?(terminal, input) do
          true ->
            {:ok, "", input}
          false ->
            :mismatch
        end
      nil ->
        :mismatch
      [match] ->
        # Two parts are necessary since the first is being trimmed away
        {^match, rest} = String.split_at(input, String.length(match))
        {:ok, match, rest}
    end
  end

  defp parse(input, %{:definitions => definitions} = grammar,
             {:nonterminal, nonterminal}) do
    parse(input, grammar, definitions[nonterminal])
  end

  defp parse(input, grammar, {:sequence, expressions}) do
    parse_sequence(input, grammar, expressions)
  end

  defp parse(input, grammar, {:priority, expressions}) do
    parse_priorities(input, grammar, expressions)
  end

  defp parse(input, grammar, {:zero_or_more, expression}) do
    parse_zero_or_more(input, grammar, expression)
  end

  defp parse(input, grammar, {:one_or_more, expression}) do
    parse_one_or_more(input, grammar, expression)
  end

  defp parse(input, grammar, {:zero_or_one, expression}) do
    case parse(input, grammar, expression) do
      :mismatch ->
        {:ok, nil, input}
      otherwise ->
        otherwise
    end
  end

  defp parse(input, grammar, {:and, expression}) do
    case parse(input, grammar, expression) do
      {:ok, _, _} ->
        {:ok, nil, input}
      otherwise ->
        otherwise
    end
  end

  defp parse(input, grammar, {:not, expression}) do
    case parse(input, grammar, expression) do
      {:ok, _, _} ->
        :mismatch
      :mismatch ->
        {:ok, nil, input}
      {:error, reason} ->
        {:error, reason}
    end
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
        {:ok, Enum.reverse(acc), input}
      {:error, reason} ->
        {:error, reason}
    end
  end

  # Helper for one or more (+ suffix)
  defp parse_one_or_more(input, grammar, expression, acc \\ [])
  defp parse_one_or_more(input, grammar, expression, []) do
    case parse(input, grammar, expression) do
      {:ok, match, input} ->
        parse_zero_or_more(input, grammar, expression, [match])
      otherwise ->
        otherwise
    end
  end
end