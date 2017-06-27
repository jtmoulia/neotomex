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
      "<'{'> <space?> pair (<space?> <','> <space?> pair)* <space?> <'}'> / <'{'> <space?> <'}'>" do
    [] -> Map.new()
    [head, others] ->
      Enum.into([head | (for [pair] <- others, do: pair)], Map.new())
  end

  define :pair, "<space?> string <space?> <':'> <space?> json_value <space?>" do
    [string, val] -> {String.to_atom(string), val}
  end

  # TODO - not properly matching escape seqs
  define :string, "<'\"'> (<!'\"'> ('\\\\' / '\\\"' / .))* <'\"'>" do
    [chars] -> Enum.join(for [c] <- chars, do: c)
  end

  define :array, "<'['> <space?> json_value (<space?> <','> <space?> json_value)* <space?> <']'> / <'['> space? <']'>" do
    [] -> []
    [head, rest] -> [head | (for [val] <- rest, do: val)]
  end


  define :number, "int frac? exp?" do
    [int, nil,  nil] -> iolist_to_integer(int)
    [int, frac, exp] ->
      base = if frac do
               iolist_to_float([int | frac])
             else
               iolist_to_integer(int)
             end
      base = if exp, do: base * :math.pow(10, exp), else: base
      if frac, do: base, else: round(base)
  end

  define :int, "'-'? (non_zero_digit digit+) / digit" do
    [nil, [head, rest]]         -> [head | rest]
    digit when is_binary(digit) -> [digit]
  end

  # Produce the exponent as an integer
  define :exp, "e digit+" do
    [suffix, digits] -> iolist_to_integer([suffix | digits])
  end

  define :e, "<[eE]> ('+' / '-')?" do
    [nil]    -> "+"
    [suffix] -> suffix
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


  @doc """
  JSON parsing REPL.
  """
  def repl do
    input = IO.gets "Enter a valid JSON expression: "
    case input |> String.strip |> parse do
      {:ok, result} ->
        IO.inspect result
      {:ok, result, remainder} ->
        IO.inspect result
        IO.puts "remainder: #{remainder}"
      :mismatch ->
        IO.puts "You sure you got that right?"
    end
    repl()
  end

  @doc """
  Basic JSON parsing tests.
  """
  def test do
    {:ok, 1} = parse("1")
    {:ok, 3000} = parse("3e3")
    {:ok, 3.0e3} = parse("3.0e3")
    {:ok, 3.3} = parse("3.3")

    {:ok, [1]} = parse("[1]")
    dict = ["a": 1] |> Enum.into(Map.new)
    {:ok, ^dict} = parse("{\"a\": 1}")
  end
end

JSON.test

IO.puts "A JSON Parser. See this file for the implementation."
IO.puts "====================================================\n"
JSON.repl
