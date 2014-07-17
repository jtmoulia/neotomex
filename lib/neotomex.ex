defmodule Neotomex do
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
      unquote IO.inspect(Macro.expand("string", __CALLER__))
    end
  end


  @type parse_expression_state :: :none
  def tokenize_expression(<<??, rest :: binary>>, :none) do 
  end

  def parse_expression(expression, state \\ {:identifier, ""}) do

  end

end
