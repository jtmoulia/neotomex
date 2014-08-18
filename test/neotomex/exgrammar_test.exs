defmodule Neotomex.ExGrammarTest do
  use ExUnit.Case

  defmodule Number do
    use Neotomex.ExGrammar

    @root true
    define :number, "digit+" do
      digits -> digits |> Enum.join |> String.to_integer
    end

    define :digit, "[0-9]"
  end

  test "the macro interface using `Number`" do
    assert Number.parse("1") == {:ok, 1}
    assert Number.parse!("1") == 1
    assert Number.parse("123") == {:ok, 123}
    assert Number.parse!("123") == 123
    assert Number.parse("nope") == :mismatch
  end


  defmodule Options do
    use Neotomex.ExGrammar

    @root true
    define :options, "'a' / 'b'" do
      "a" -> :a
      "b" -> :b
    end
  end

  test "parsing with options" do
    assert Options.parse("a") == {:ok, :a}
    assert Options.parse("c") == :mismatch
  end
end