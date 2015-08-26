defmodule Neotomex.PEG do
  @moduledoc """
  # Neotomex.PEG

  Implements a PEG specification parser using the internal PEG
  specification, and functions for parsing PEG grammars. There
  are separate functions for parsing entire grammars or
  only expressions.

  Neotomex's expressions add onto Bryan Ford's original PEG
  grammar specification with:

  - **Expression Pruning** allows for matching expressions
    which aren't passed into the transform function. They're
    indicated by bracketing an expression with angle brackets,
    i.e. `'<' expression '>'`

"""

  # Specification of PEG, in PEG:

  # Hierarchical syntax
  # Grammar    <- Spacing Definition+ EndOfFile
  # Definition <- Identifier LEFTARROW Expression
  # Expression <- Sequence (SLASH Sequence)*
  # Sequence   <- (('<' Prefix '>') / Prefix)*
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
  # OPENANGLE  <- '<' Spacing
  # CLOSEANGLE <- '>' Spacing
  # DOT        <- '.' Spacing
  # Spacing    <- (Space / Comment)*
  # Comment    <- '#' (!EndOfLine .)* EndOfLine
  # Space      <- ' ' / '\t' / EndOfLine
  # EndOfLine  <- '\r\n' / '\n' / '\r'
  # EndOfFile  <- !.
  # Parse a PEG expression for the Neotoma grammar interface.

  @doc """
  Parse the input using the `peg_grammar`.
  """
  @spec parse(binary()) :: {:ok, Neotomex.Grammar.grammar()}
  def parse(input) do
    unwrap_parse(grammar, input)
  end


  @doc """
  Match against the input using the `peg_grammar`.

  This could be useful for basic validation of grammars.
  """
  @spec match(binary()) :: {:ok, Neotomex.Grammar.match(), binary()}
  def match(input) do
    Neotomex.Grammar.match(grammar, input)
  end


  @doc """
  Parse the input as a PEG expression rather than a full PEG grammar.
  """
  def parse_expression(input) do
    unwrap_parse(expression_grammar, input)
  end


  @doc """
  PEG parser grammar defined in Neotomex internal PEG format
  """
  @spec grammar :: Neotomex.Grammar.grammar()
  def grammar do
    Neotomex.Grammar.new(:grammar, grammar_definitions)
  end

  def expression_grammar do
    Neotomex.Grammar.new(:expression, expression_definitions)
  end


  ## Private functions

  ## Helper for simplifying the parse function return
  @doc false
  defp unwrap_parse(grammar, input) do
    case Neotomex.Grammar.parse(grammar, input) do
      {:ok, grammar, ""} ->
        {:ok, grammar}
      otherwise ->
        otherwise
    end
  end

  # Definitions for parsing a PEG grammar
  @doc false
  defp grammar_definitions do
    grammar_definitions =
    %{:grammar =>
        {{:sequence, [{:nonterminal, :spacing},
                      {:one_or_more, {:nonterminal, :definition}},
                      {:nonterminal, :EOF}]},
         {:transform,
          fn [:spacing, [{root, _}] = definitions, :EOF] ->
            Neotomex.Grammar.new(root, Enum.into(definitions, %{}))
             [:spacing, [{root, _} | _] = definitions, :EOF] ->
            Neotomex.Grammar.new(root, Enum.into(definitions, %{}))
          end}},
      :definition =>
        {{:sequence, [{:nonterminal, :identifier},
                      {:nonterminal, :LEFTARROW},
                      {:nonterminal, :expression}]},
         {:transform, fn [id, _, expr] -> {id, expr} end}}}
    Dict.merge(grammar_definitions, expression_definitions)
  end

  # Definitions for parsing a PEG expression
  @doc false
  defp expression_definitions do
    %{:expression =>
        {{:sequence, [{:nonterminal, :sequence},
                      {:zero_or_more, {:sequence,
                                       [{:nonterminal, :SLASH},
                                        {:nonterminal, :sequence}]}}]},
         {:transform,
          fn [seq, []] -> seq
             [seq, rest] -> {:priority, [seq | for [:SLASH, p] <- rest, do: p]}
          end}},

      :sequence =>
      {{:zero_or_more, {:priority, [{:sequence,
                                     [{:prune, {:nonterminal, :OPENANGLE}},
                                      {:nonterminal, :prefix},
                                      {:prune, {:nonterminal, :CLOSEANGLE}}]},
                                    {:nonterminal, :prefix}]}},
         {:transform,
          # TODO - this is pretty ugly and could use some refactoring
          fn [[sub_expr]]                      -> {:prune, sub_expr}
             [sub_expr]                        -> sub_expr
             sub_exprs when is_list(sub_exprs) ->
            {:sequence, (for e <- sub_exprs do
                           (case e do [e] -> {:prune, e}
                                      e   -> e
                            end) end)}
          end}},

      :prefix =>
        {{:sequence, [{:zero_or_one, {:priority, [{:nonterminal, :AND},
                                                  {:nonterminal, :NOT}]}},
                      {:nonterminal, :suffix}]},
         {:transform,
          fn [nil,  suffix] -> suffix
             [:NOT, suffix] -> {:not, suffix}
             [:AND, suffix] -> {:and, suffix}
          end}},

      :suffix =>
        {{:sequence, [{:nonterminal, :primary},
                      {:zero_or_one, {:priority, [{:nonterminal, :QUESTION},
                                                  {:nonterminal, :STAR},
                                                  {:nonterminal, :PLUS}]}}]},
         {:transform,
          fn [primary, nil]       -> primary
             [primary, :QUESTION] -> {:zero_or_one,  primary}
             [primary, :STAR]     -> {:zero_or_more, primary}
             [primary, :PLUS]     -> {:one_or_more,  primary}
          end}},

      :primary =>
        {{:priority, [{:sequence, [{:nonterminal, :identifier},
                                   {:not, {:nonterminal, :LEFTARROW}}]},
                      {:sequence, [{:nonterminal, :OPEN},
                                   {:nonterminal, :expression},
                                   {:nonterminal, :CLOSE}]},
                      {:nonterminal, :literal},
                      {:nonterminal, :class},
                      {:nonterminal, :DOT}]},
         {:transform,
          fn [id, _]               -> {:nonterminal, id}
             [:OPEN, expr, :CLOSE] -> expr
             :DOT                  -> {:terminal, ~r/^./u}
             x                     -> x
          end}},

      # Lexical syntax
      :identifier =>
        {{:sequence, [{:nonterminal, :ident_start},
                      {:zero_or_more, {:nonterminal, :ident_cont}},
                      {:nonterminal, :spacing}]},
         {:transform,
          fn [ident_start, ident_cont, :spacing] ->
            Enum.join([ident_start | ident_cont]) |> String.to_atom
          end}},
      :ident_start => {:terminal, ~r/^[a-zA-Z_]/u},
      :ident_cont => {:priority, [{:nonterminal, :ident_start},
                                  {:terminal, ~r/^[0-9]/}]},
      :literal =>
        {{:priority, [{:sequence, [{:terminal, ?'},
                                   {:zero_or_more,
                                    {:sequence, [{:not, {:terminal, 39}},
                                                 {:nonterminal, :char}]}},
                                   {:terminal, ?'},
                                   {:nonterminal, :spacing}]},
                      {:sequence, [{:terminal, ?"},
                                   {:zero_or_more,
                                    {:sequence, [{:not, {:terminal, 34}},
                                                 {:nonterminal, :char}]}},
                                   {:terminal, ?"},
                                   {:nonterminal, :spacing}]}]},
         {:transform,
          fn [quot, chars, quot, :spacing] ->
            {:terminal, Enum.join(for [nil, char] <- chars, do: char)}
          end}},

      :class =>
        {{:sequence, [{:terminal, ?[},
                      {:zero_or_more, {:sequence, [{:not, {:terminal, 93}},
                                                   {:nonterminal, :range}]}},
                                    {:terminal, ?]},
                      {:nonterminal, :spacing}]},
         {:transform,
          fn [?[, ranges, ?], :spacing] ->
                 {:ok, regex} = Enum.join(["^[" | for [nil, r] <- ranges, do: r]
                                          ++ ["]"])
                   |> Regex.compile
                 {:terminal, regex}
               end}},

      :range =>
        {{:priority, [{:sequence, [{:nonterminal, :char},
                                   {:terminal, ?-},
                                   {:nonterminal, :char}]},
                      {:nonterminal, :char}]},
         {:transform,
          fn [start, ?-, stop] -> Enum.join([start, "-", stop])
             char              -> char
          end}},

      # TODO: Fix single character match
      :char =>
        {{:priority, [{:sequence, [{:terminal, "\\"},
                                   {:terminal, ~r/^[nrts\[\]\\'"]/u}]},
                      {:sequence, [{:not, {:terminal, "\\"}},
                                   {:terminal, ~r/^./u}]}]},
         {:transform,
          fn [nil, char] -> char
             ["\\", "n"] -> "\n"
             ["\\", "r"] -> "\r"
             ["\\", "t"] -> "\t"
             ["\\", "s"] -> "\t"
             ["\\", "["] -> "["
             ["\\", "]"] -> "]"
             ["\\", "\\"] -> "\\"
             ["\\", "\""] -> "\""
          end
        }},

      :LEFTARROW => {{:sequence, [{:terminal, "<-"}, {:nonterminal, :spacing}]},
                     {:transform, fn _ -> :LEFTARROW end}},
      :SLASH     => {{:sequence, [{:terminal, 47}, {:nonterminal, :spacing}]},
                     {:transform, fn _ -> :SLASH end}},
      :AND       => {{:sequence, [{:terminal, ?&}, {:nonterminal, :spacing}]},
                     {:transform, fn _ -> :AND end}},
      :NOT       => {{:sequence, [{:terminal, ?!}, {:nonterminal, :spacing}]},
                     {:transform, fn _ -> :NOT end}},
      :QUESTION  => {{:sequence, [{:terminal, ??}, {:nonterminal, :spacing}]},
                     {:transform, fn _ -> :QUESTION end}},
      :STAR      => {{:sequence, [{:terminal, ?*}, {:nonterminal, :spacing}]},
                     {:transform, fn _ -> :STAR end}},
      :PLUS      => {{:sequence, [{:terminal, ?+}, {:nonterminal, :spacing}]},
                     {:transform, fn _ -> :PLUS end}},
      :OPEN      => {{:sequence, [{:terminal, ?(}, {:nonterminal, :spacing}]},
                     {:transform, fn _ -> :OPEN end}},
      :CLOSE     => {{:sequence, [{:terminal, ?)}, {:nonterminal, :spacing}]},
                     {:transform, fn _ -> :CLOSE end}},
      :OPENANGLE => {{:sequence, [{:terminal, ?<}, {:nonterminal, :spacing}]},
                     {:transform, fn _ -> :OPENANGLE end}},
      :CLOSEANGLE => {{:sequence, [{:terminal, ?>}, {:nonterminal, :spacing}]},
                      {:transform, fn _ -> :CLOSEANGLE end}},
      :DOT       => {{:sequence, [{:terminal, ?.}, {:nonterminal, :spacing}]},
                     {:transform, fn _ -> :DOT end}},
      :spacing   => {{:zero_or_more, {:priority, [{:nonterminal, :space},
                                                  {:nonterminal, :comment}]}},
                     {:transform, fn _ -> :spacing end}},
      :comment => {:sequence, [{:terminal, ?#},
                               {:zero_or_more,
                                {:sequence, [{:not, {:nonterminal, :EOL}},
                                             {:terminal, ~r/./u}]}},
                               {:nonterminal, :EOL}]},
      :space => {{:priority, [{:terminal, " "},
                              {:terminal, "\t"},
                              {:nonterminal, :EOL}]},
                 {:transform, fn _ -> :space end}},
      :EOL => {{:priority, [{:terminal, "\r\n"},
                            {:terminal, "\n"},
                            {:terminal, "\r"}]},
               {:transform, fn _ -> :EOL end}},
      :EOF => {{:not, {:terminal, ~r/./u}},
               {:transform, fn _ -> :EOF end}}
     }
  end
end
