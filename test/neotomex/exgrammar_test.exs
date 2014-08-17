defmodule Neotomex.ExGrammarTest do
  use ExUnit.Case

  defmodule Number do
    use Neotomex.ExGrammar

    # IO.inspect((testin string, do: string).(6))
    # IO.inspect quote do: fn(string = 5) -> string end

    #testin do: (a -> a)

    @root true
    define :a, "[0-9]+" do
      x -> x
    end
  end

  # test "macro interface (Number)" do
  #   # assert Number.parse("1") == 1
  #   1
  # end

end