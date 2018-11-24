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


  defmodule Invalid do
    use Neotomex.ExGrammar
    @validate false
    @root true
    define :number, "non_rule"
  end


  test "validation on an invalid grammar" do
    # assert_raise Neotomex.Grammar.ValidationError,
    #   "validation error: {error,{bad_definition,{number,{missing,{definition,non_rule}}}}}",
    #   Invalid.validate!
    assert {:error, _} = Invalid.validate
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


  defmodule Stripper do
    @moduledoc """
    For testing transforms that match against `_`.
    """
    use Neotomex.ExGrammar

    @root true
    define :options, "'a' / 'b'" do
      "a" -> :a
      _   -> false
    end
  end

  test "test throwaway matches in transform" do
    assert Stripper.parse("a") == {:ok, :a}
    assert Stripper.parse("b") == {:ok, false}
  end


  defmodule FunCaller do
    use Neotomex.ExGrammar

    @root true
    define :number, "digit+" do
      digits -> list_to_number(digits)
    end

    define :digit, "[0-9]"

    defp list_to_number(digits) do
      digits |> Enum.join |> String.to_integer
    end
  end

  test "calling a module function from transform" do
    assert FunCaller.parse("42") == {:ok, 42}
  end


  defmodule OneLiner do
    @moduledoc """
    For testing one liner define.
    """
    use Neotomex.ExGrammar

    @root true
    define :char, ".", do: (char -> char)
  end

  test "one liner single character match" do
    assert OneLiner.parse("a") == {:ok, "a"}
    assert OneLiner.parse("b") == {:ok, "b"}
    assert OneLiner.parse("!") == {:ok, "!"}
  end


  defmodule UncalledPruner do
    @moduledoc """
    For testing expression pruning of uncalled expressions.
    """
    use Neotomex.ExGrammar

    @root true
    define :char, "<.>", do: ([:wont, :be] -> :called)
  end

  test "a single, non-sequence match won't be called." do
    assert UncalledPruner.parse("a") == {:ok, nil}
  end


  defmodule Pruner do
    @moduledoc """
    For testing list pruning
    """
    use Neotomex.ExGrammar

    @root true
    define :char, "<.> . <.>", do: ([middle] -> middle)
  end

  test "pruning out expressions" do
    assert Pruner.parse("abc") == {:ok, "b"}
  end

  defmodule InsensitiveMatcher do
    @moduledoc """
    For testing list pruning
    """
    use Neotomex.ExGrammar

    @root true
    define :char, "( foo )~ <' '> bar"
    define :foo, "'FOO'"
    define :bar, "'BAR'"
  end

  test "case insensitive expressions" do
    assert InsensitiveMatcher.parse("FoO BAR") == {:ok, ["FoO", "BAR"]}
    assert InsensitiveMatcher.parse("fOo bar") == :mismatch
  end


  defmodule UnicodeEscaper do
    @moduledoc """
    For testing escaped unicode expressions. Simply having a unicode
    definition checks if the neotomex PEG grammar is matching correctly.

    See [#11](https://github.com/jtmoulia/neotomex/issues/11)
    """
    use Neotomex.ExGrammar

    @root true
    define :unicode, "[\u000c\u0020\u200a]"
  end

end
