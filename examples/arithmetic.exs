# Run this app from Neotomex root with
#
#     mix run -r examples/arithmetic.exs

defmodule Arithmetic do
  @moduledoc """
  Adapted from
  [neotoma](https://github.com/seancribbs/neotoma/blob/master/extra/arithmetic.peg).
  """
  use Neotomex.ExGrammar

  @root true
  define :additive, "multitive '+' additive / multitive" do
    int when is_integer(int) -> int
    [x, "+", y]              -> x + y
  end

  define :multitive, "primary '*' multitive / primary" do
    int when is_integer(int) -> int
    [x, "*", y]              -> x * y
  end

  define :primary, "('(' additive ')') / decimal" do
    int when is_integer(int) -> int
    [_, additive, _]         -> additive
  end

  define :decimal, "[0-9]+" do
    digits -> Enum.join(digits) |> String.to_integer
  end


  def repl do
    input = IO.gets "Enter an equation: "
    case input |> String.strip |> parse do
      {:ok, result} ->
        IO.puts "Result: #{result}"
      :mismatch ->
        IO.puts "You sure you got that right?"
    end
    repl
  end
end


IO.puts "Neotomex Arithmetic Parser!"
IO.puts "no spaces, no division, no subtraction"
IO.puts "C-c to quit"
Arithmetic.repl