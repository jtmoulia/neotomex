defmodule Neotomex.GrammarTest do
  use ExUnit.Case
  import Neotomex.Grammar  # using new/1,2,3 and parse/2,3

  test "parse terminal" do
    first_char_grammar = new(:root, %{root: {:terminal, ~r/^.*/}})
    assert parse("", first_char_grammar) == {:ok, "", ""}
    assert parse("test", first_char_grammar) == {:ok, "test", ""}

    a_grammar = new(:root, %{root: {:terminal, ~r/^a/}})
    assert parse("", a_grammar) == :mismatch
    assert parse("abc", a_grammar) == {:ok, "a", "bc"}

    non_term_grammar = new(:root, %{root: {:nonterminal, :non},
                                    non: {:terminal, ~r/^.*/}})
    assert parse("", non_term_grammar) == {:ok, "", ""}
    assert parse("test", non_term_grammar) == {:ok, "test", ""}
  end

  test "parse sequence" do
    grammar = new(:root, %{root: {:sequence, [{:terminal, ~r/^a/},
                                              {:terminal, ~r/^b/}]}})
    assert parse("", grammar) == :mismatch
    assert parse("a", grammar) == :mismatch
    assert parse("ab", grammar) == {:ok, ["a", "b"], ""}
    assert parse("aab", grammar) == :mismatch
  end

  test "parse priority" do
    grammar = new(:root, %{root: {:priority, [{:terminal, ~r/^a/},
                                              {:terminal, ~r/^b/}]}})
    assert parse("", grammar) == :mismatch
    assert parse("a", grammar) == {:ok, "a", ""}
    assert parse("ab", grammar) == {:ok, "a", "b"}
    assert parse("ba", grammar) == {:ok, "b", "a"}
  end

  test "parse zero or more" do
    grammar = new(:root, %{root: {:zero_or_more, {:terminal, ~r/^a/}}})
    assert parse("", grammar) == {:ok, [], ""}
    assert parse("ab", grammar) == {:ok, ["a"], "b"}
    assert parse("aab", grammar) == {:ok, ["a", "a"], "b"}
  end

  test "parse one or more" do
    grammar = new(:root, %{root: {:one_or_more, {:terminal, ~r/a/}}})
    assert parse("", grammar) == :mismatch
    assert parse("abc", grammar) == {:ok, ["a"], "bc"}
    assert parse("aabc", grammar) == {:ok, ["a", "a"], "bc"}
  end

  test "parse zero or one" do
    grammar = new(:root, %{root: {:zero_or_one, {:terminal, ~r/a/}}})
    assert parse("", grammar) == {:ok, nil, ""}
    assert parse("abc", grammar) == {:ok, "a", "bc"}
    assert parse("aabc", grammar) == {:ok, "a", "abc"}
  end

  test "parse and" do
    grammar = new(:root, %{root: {:and, {:terminal, ~r/a/}}})
    assert parse("a", grammar) == {:ok, nil, "a"}
    assert parse("b", grammar) == :mismatch
  end

  test "parse not" do
    grammar = new(:root, %{root: {:not, {:terminal, ~r/a/}}})
    assert parse("b", grammar) == {:ok, nil, "b"}
    assert parse("a", grammar) == :mismatch
  end
end