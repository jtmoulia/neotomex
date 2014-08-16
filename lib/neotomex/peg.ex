defmodule Neotomex.PEG do
  @moduledoc """
  # Neotomex.PEG

  Implements a PEG specification parser using the internal PEG
  specification, and functions for parsing PEG specifications.
  """
  require Logger

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
  # Parse a PEG expression for the Neotoma grammar interface.

  @doc """
  Parse the input using the `peg_grammar`.
  """
  @spec parse(binary()) :: {:ok, Neotomex.Grammar.grammar()}
  def parse(input) do
    case Neotomex.Grammar.parse(grammar, input) do
      {:ok, grammar, ""} ->
        {:ok, grammar}
      otherwise ->
        otherwise
    end
  end


  @doc """
  Match against the input using the `peg_grammar`.

  This could be useful for basic validation of grammars.
  """
  def match(input) do
    Neotomex.Grammar.match(grammar, input)
  end


  @doc """
  PEG parser grammar defined in Neotomex internal PEG format
  """
  def grammar do
    definitions =
    %{:grammar =>
        {{:sequence, [{:nonterminal, :spacing},
                      {:one_or_more, {:nonterminal, :definition}},
                      {:nonterminal, :EOF}]},
         fn [:spacing, [{root, _}] = definitions, :EOF] ->
           Neotomex.Grammar.new(root, Enum.into(definitions, %{}))
            [:spacing, [{root, _} | _] = definitions, :EOF] ->
           Neotomex.Grammar.new(root, Enum.into(definitions, %{}))
         end},
      :definition =>
        {{:sequence, [{:nonterminal, :identifier},
                      {:nonterminal, :LEFTARROW},
                      {:nonterminal, :expression}]},
         fn [id, _, expr] -> {id, expr} end},

      :expression =>
        {{:sequence, [{:nonterminal, :sequence},
                      {:zero_or_more, {:sequence,
                                       [{:nonterminal, :SLASH},
                                        {:nonterminal, :sequence}]}}]},
         fn [seq, []] -> seq
            [seq, rest] -> {:priority, [seq | for [:SLASH, p] <- rest, do: p]}
         end},

      :sequence =>
        {{:zero_or_more, {:nonterminal, :prefix}},
         fn [sub_expr]                        -> sub_expr
            sub_exprs when is_list(sub_exprs) -> {:sequence, sub_exprs}
         end},

      :prefix =>
        {{:sequence, [{:zero_or_one, {:priority, [{:nonterminal, :AND},
                                                  {:nonterminal, :NOT}]}},
                      {:nonterminal, :suffix}]},
         fn [nil,  suffix] -> suffix
            [:NOT, suffix] -> {:not, suffix}
            [:AND, suffix] -> {:and, suffix}
         end},

      :suffix =>
        {{:sequence, [{:nonterminal, :primary},
                      {:zero_or_one, {:priority, [{:nonterminal, :QUESTION},
                                                  {:nonterminal, :STAR},
                                                  {:nonterminal, :PLUS}]}}]},
         fn [primary, nil]       -> primary
            [primary, :QUESTION] -> {:zero_or_one,  primary}
            [primary, :STAR]     -> {:zero_or_more, primary}
            [primary, :PLUS]     -> {:one_or_more,  primary}
         end},

      :primary =>
        {{:priority, [{:sequence, [{:nonterminal, :identifier},
                                   {:not, {:nonterminal, :LEFTARROW}}]},
                      {:sequence, [{:nonterminal, :OPEN},
                                   {:nonterminal, :expression},
                                   {:nonterminal, :CLOSE}]},
                      {:nonterminal, :literal},
                      {:nonterminal, :class},
                      {:nonterminal, :DOT}]},
         fn [id, _]               -> {:nonterminal, id}
            [:OPEN, expr, :CLOSE] -> expr
            x                     -> x
         end},

      # Lexical syntax
      :identifier =>
        {{:sequence, [{:nonterminal, :ident_start},
                      {:zero_or_more, {:nonterminal, :ident_cont}},
                      {:nonterminal, :spacing}]},
         fn [ident_start, ident_cont, :spacing] ->
           Enum.join([ident_start | ident_cont]) |> String.to_atom
         end},
      :ident_start => {:terminal, ~r/^[a-zA-Z_]/},
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
         fn [quot, chars, quot, :spacing] ->
           {:terminal, Enum.join(for [nil, char] <- chars, do: char)}
         end},

      :class =>
        {{:sequence, [{:terminal, ?[},
                      {:zero_or_more, {:sequence, [{:not, {:terminal, 93}},
                                                   {:nonterminal, :range}]}},
                                    {:terminal, ?]},
                      {:nonterminal, :spacing}]},
         fn [?[, ranges, ?], :spacing] ->
           Enum.join(["^[" | for [nil, r] <- ranges, do: r] ++ ["]"]) |> Regex.compile
         end},

      :range =>
        {{:priority, [{:sequence, [{:nonterminal, :char},
                                   {:terminal, ?-},
                                   {:nonterminal, :char}]},
                      {:nonterminal, :char}]},
         fn [start, ?-, stop] -> Enum.join([start, "-", stop]) end},

      :char => {:terminal, ~r/^./}, # TODO: Fix single character match

      :LEFTARROW => {{:sequence, [{:terminal, "<-"}, {:nonterminal, :spacing}]},
                     fn _ -> :LEFTARROW end},
      :SLASH     => {{:sequence, [{:terminal, 47}, {:nonterminal, :spacing}]},
                     fn _ -> :SLASH end},
      :AND       => {{:sequence, [{:terminal, ?&}, {:nonterminal, :spacing}]},
                     fn _ -> :AND end},
      :NOT       => {{:sequence, [{:terminal, ?!}, {:nonterminal, :spacing}]},
                     fn _ -> :NOT end},
      :QUESTION  => {{:sequence, [{:terminal, ??}, {:nonterminal, :spacing}]},
                     fn _ -> :QUESTION end},
      :STAR      => {{:sequence, [{:terminal, ?*}, {:nonterminal, :spacing}]},
                     fn _ -> :STAR end},
      :PLUS      => {{:sequence, [{:terminal, ?+}, {:nonterminal, :spacing}]},
                     fn _ -> :PLUS end},
      :OPEN      => {{:sequence, [{:terminal, ?(}, {:nonterminal, :spacing}]},
                     fn _ -> :OPEN end},
      :CLOSE     => {{:sequence, [{:terminal, ?)}, {:nonterminal, :spacing}]},
                     fn _ -> :CLOSE end},
      :DOT       => {{:sequence, [{:terminal, ?.}, {:nonterminal, :spacing}]},
                     fn _ -> :DOT end},
      :spacing   => {{:zero_or_more, {:priority, [{:nonterminal, :space},
                                                  {:nonterminal, :comment}]}},
                     fn _ -> :spacing end},
      :comment => {:sequence, [{:terminal, ?#},
                               {:zero_or_more,
                                {:sequence, [{:not, {:nonterminal, :EOL}},
                                             {:terminal, ~r/./}]}},
                               {:nonterminal, :EOL}]},
      :space => {{:priority, [{:terminal, " "},
                              {:terminal, "\t"},
                              {:nonterminal, :EOL}]},
                 fn _ -> :space end},
      :EOL => {{:priority, [{:terminal, "\r\n"},
                            {:terminal, "\n"},
                            {:terminal, "\r"}]},
               fn _ -> :EOL end},
      :EOF => {{:not, {:terminal, ~r/./}},
               fn _ -> :EOF end}
     }

    Neotomex.Grammar.new(:grammar, definitions)
  end

end