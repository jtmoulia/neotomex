# Run this app from Neotomex root with
#
#     mix run -r examples/json.exs

defmodule JSON do
  use Neotomex.ExGrammar

  @root true
  define :json_value,
      "space? (object / array / number / string / true / false / null) space?" do
    [_, json_value, _] -> json_value
  end

  define :object,
      "'{' space? pair (space? ',' space? pair)* space? '}' / '{' space? '}'" do
    [_, _, _]                  -> HashDict.new()
    [_, _, head, others, _, _] ->
      Enum.into([head | for [_, _, _, pair] <- others, do: pair], HashDict.new)
  end

  define :pair, "space? string space? ':' space? json_value space?" do
    [_, string, _, _, _, val, _] -> {String.to_atom(string), val}
  end

  # TODO - not properly matching escape seqs
  define :string, "'\"' (!'\"' ('\\\\' / '\\\"' / .))* '\"'" do
    [_, chars, _] -> Enum.join(for [nil, c] <- chars, do: c)
  end

  define :array, "'[' space? json_value (space? ',' space? json_value)* space? ']' / '[' space? ']'" do
    [_, _, _] -> []
    [_, _, head, rest, _, _] -> [head | for [_, _, _, val] <- rest, do: val]
  end


  define :number, "int frac? exp?" do
    [int, nil,  nil] -> iolist_to_integer(int)
    [int, frac, exp] ->
      base = if frac do
               iolist_to_float([int | frac])
             else
               iolist_to_integer(int)
             end
      if exp, do: :math.pow(base, exp), else: base
  end

  define :int, "'-'? (non_zero_digit digit+) / digit" do
    [nil, [head, rest]]         -> [head | rest]
    digit when is_binary(digit) -> [digit]
  end

  # Produce the exponent as an integer
  define :exp, "e digit+" do
    [suffix, digits] -> iolist_to_integer([suffix | digits])
  end

  define :e, "[eE] ('+' / '-')?" do
    [_, nil]    -> "+"
    [_, suffix] -> suffix
  end

  define :frac,           "'.' digit+",  do: ([head, rest] -> [head | rest])
  define :non_zero_digit, "[1-9]"
  define :digit,          "[0-9]"
  define :true,           "'true'",      do: (_ -> true)
  define :false,          "'false'",     do: (_ -> false)
  define :null,           "'null'",      do: (_ -> nil)
  define :space,          "[ \\r\\n\\s\\t]*"


  defp iolist_to_integer(digits) do
    digits |> :erlang.iolist_to_binary |> String.to_integer
  end

  defp iolist_to_float(digits) do
    digits |> :erlang.iolist_to_binary |> String.to_float
  end


  def repl do
    input = IO.gets "Enter a valid JSON expression: "
    case input |> String.strip |> parse do
      {:ok, result} ->
        IO.inspect result
      :mismatch ->
        IO.puts "You sure you got that right?"
    end
    repl
  end
end

IO.puts "A JSON Parser. Because all the cool kids are doing it."
IO.puts "======================================================\n"
JSON.repl
