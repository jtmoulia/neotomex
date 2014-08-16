defmodule Neotomex.PEG do
  @moduledoc """
  # Neotomex.PEG

  Implements a PEG specification parser using the internal PEG
  specification, and functions for parsing PEG specifications.
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
  # Parse a PEG expression for the Neotoma grammar interface.

  @doc """
  Parse the input using the `peg_grammar`.
  """
  def parse(input) do
    Neotomex.Grammar.parse(grammar, input)
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
        # Hierarchical syntax
        {:sequence, [{:nonterminal, :spacing},
                     {:one_or_more, {:nonterminal, :definition}},
                     {:nonterminal, :EOF}]},
      :definition =>
        {:sequence, [{:nonterminal, :identifier},
                     {:nonterminal, :LEFTARROW},
                     {:nonterminal, :expression}]},
      :expression =>
        {:sequence, [{:nonterminal, :sequence},
                     {:zero_or_more, {:sequence,
                                      [{:nonterminal, :SLASH},
                                       {:nonterminal, :sequence}]}}]},
      :sequence => {:zero_or_more, {:nonterminal, :prefix}},
      :prefix => {:sequence, [{:zero_or_one, {:priority, [{:nonterminal, :AND},
                                                          {:nonterminal, :NOT}]}},
                              {:nonterminal, :suffix}]},
      :suffix =>
        {:sequence, [{:nonterminal, :primary},
                     {:zero_or_one, {:priority, [{:nonterminal, :QUESTION},
                                                 {:nonterminal, :STAR},
                                                 {:nonterminal, :PLUS}]}}]},
      :primary =>
        {:priority, [{:sequence, [{:nonterminal, :identifier},
                                  {:not, {:nonterminal, :LEFTARROW}}]},
                     {:sequence, [{:nonterminal, :OPEN},
                                  {:nonterminal, :expression},
                                  {:nonterminal, :CLOSE}]},
                     {:nonterminal, :literal},
                     {:nonterminal, :class},
                     {:nonterminal, :DOT}]},

      # Lexical syntax
      :identifier =>
        {:sequence, [{:nonterminal, :ident_start},
                     {:zero_or_more, {:nonterminal, :ident_cont}},
                     {:nonterminal, :spacing}]},
      :ident_start => {:terminal, ~r/^[a-zA-Z_]/},
      :ident_cont => {:priority, [{:nonterminal, :ident_start},
                                  {:terminal, ~r/^[0-9]/}]},
      :literal =>
        {:priority, [{:sequence, [{:terminal, ?'},
                                  {:zero_or_more,
                                   {:sequence, [{:not, {:terminal, 39}},
                                                {:nonterminal, :char}]}},
                                  {:terminal, ?'},
                                  {:nonterminal, :spacing}]},
                     {:sequence, [{:terminal, 34},
                                  {:zero_or_more,
                                   {:sequence, [{:not, {:terminal, 34}},
                                                {:nonterminal, :char}]}},
                                  {:terminal, 34},
                                  {:nonterminal, :spacing}]}]},
      :class => {:sequence, [{:terminal, ?[},
                                           {:zero_or_more, {:sequence, [{:not, {:terminal, 93}},
                                                                        {:nonterminal, :range}]}},
                                           {:terminal, ?]},
                             {:nonterminal, :spacing}]},
      :range => {:priority, [{:sequence, [{:nonterminal, :char},
                                          {:terminal, ?-},
                                          {:nonterminal, :char}]},
                             {:nonterminal, :char}]},
      :char => {:terminal, ~r/./},  # TODO -- this is wrong

        :LEFTARROW => {:sequence, [{:terminal, "<-"}, {:nonterminal, :spacing}]},
        :SLASH     => {:sequence, [{:terminal, 47}, {:nonterminal, :spacing}]},
        :AND       => {:sequence, [{:terminal, ?&}, {:nonterminal, :spacing}]},
        :NOT       => {:sequence, [{:terminal, ?!}, {:nonterminal, :spacing}]},
        :QUESTION  => {:sequence, [{:terminal, ??}, {:nonterminal, :spacing}]},
        :STAR      => {:sequence, [{:terminal, ?*}, {:nonterminal, :spacing}]},
        :PLUS      => {:sequence, [{:terminal, ?+}, {:nonterminal, :spacing}]},
        :OPEN      => {:sequence, [{:terminal, ?(}, {:nonterminal, :spacing}]},
        :CLOSE     => {:sequence, [{:terminal, ?)}, {:nonterminal, :spacing}]},
        :DOT       => {:sequence, [{:terminal, ?.}, {:nonterminal, :spacing}]},
        :spacing   => {:zero_or_more, {:priority, [{:nonterminal, :space},
                                                   {:nonterminal, :comment}]}},
        :comment => {:sequence, [{:terminal, ?#},
                                 {:zero_or_more,
                                  {:sequence, [{:not, {:nonterminal, :EOL}},
                                               {:terminal, ~r/./}]}},
                                 {:nonterminal, :EOL}]},
        :space => {:priority, [{:terminal, " "},
                               {:terminal, "\t"},
                               {:nonterminal, :EOL}]},
        :EOL => {:priority, [{:terminal, "\r\n"},
                             {:terminal, "\n"},
                             {:terminal, "\r"}]},
        :EOF => {:not, {:terminal, ~r/./}}  # TODO this isn't matching...
     }

    Neotomex.Grammar.new(:grammar, definitions)
  end

end