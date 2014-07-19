defmodule Neotomex.GrammarTest do
  use ExUnit.Case
  import Neotomex.Grammar  # using new/1,2,3 and parse/2,3

  test "parse terminal" do
    expression = {:terminal, ~r/^.*/}
    grammar = new(:root, %{root: {expression, nil}})
    assert parse("", grammar) == {:ok, {expression, ""}, ""}
    assert parse("test", grammar) == {:ok, {expression, "test"}, ""}

    expression = {:terminal, ~r/^a/}
    grammar = new(:root, %{root: {expression, nil}})
    assert parse("", grammar) == :mismatch
    assert parse("abc", grammar) == {:ok, {expression, "a"}, "bc"}

    expression1 = {:nonterminal, :non}
    expression2 = {:terminal, ~r/^.*/}
    grammar = new(:root, %{root: {expression1, nil},
                           non: {expression2, nil}})
    assert parse("", grammar) == {:ok, {expression1, {expression2, ""}}, ""}
    assert parse("test", grammar) == {:ok, {expression1, {expression2, "test"}}, ""}
  end

  test "parse sequence" do
    expression = {{:sequence, [{{:terminal, ~r/^a/}, nil},
                               {{:terminal, ~r/^b/}, nil}]}, nil}
    grammar = new(:root, %{root: expression})
    assert parse("", grammar) == :mismatch
    assert parse("a", grammar) == :mismatch
    assert parse("ab", grammar) == {:ok, [{{:terminal, ~r/^a/}, "a"},
                                          {{:terminal, ~r/^b/}, "b"}], ""}
    assert parse("aab", grammar) == :mismatch
  end

  test "parse priority" do
    expression = {{:priority, [{{:terminal, ~r/^a/}, nil},
                               {{:terminal, ~r/^b/}, nil}]}, nil}
    grammar = new(:root, %{root: expression})
    assert parse("", grammar) == :mismatch
    assert parse("a", grammar) ==
      {:ok, {{:priority, [{{:terminal, ~r/^a/}, nil},
                          {{:terminal, ~r/^b/}, nil}]}, "a"}, ""}
    assert parse("ab", grammar) ==
      {:ok, {{:priority, [{{:terminal, ~r/^a/}, nil},
                          {{:terminal, ~r/^b/}, nil}]}, "a"}, "b"}
    assert parse("ba", grammar) ==
      {:ok, {{:priority, [{{:terminal, ~r/^a/}, nil},
                          {{:terminal, ~r/^b/}, nil}]}, "b"}, "a"}
  end

  test "parse zero or more" do
    grammar = new(:root, %{root: {{:zero_or_more,
                                   {{:terminal, ~r/^a/}, nil}}, nil}})
    assert parse("", grammar) == {:ok, {{:zero_or_more,
                                         {{:terminal, ~r/^a/}, nil}}, []}, ""}
    assert parse("ab", grammar) == {:ok, {{:zero_or_more,
                                           {{:terminal, ~r/^a/}, nil}},
                                          [{{:terminal, ~r/^a/}, "a"}]}, "b"}
    assert parse("aab", grammar) == {:ok,
                                     {{:zero_or_more,
                                       {{:terminal, ~r/^a/}, nil}},
                                      [{{:terminal, ~r/^a/}, "a"},
                                       {{:terminal, ~r/^a/}, "a"}]}, "b"}
  end

  test "parse one or more" do
    grammar = new(:root, %{root: {{:one_or_more,
                                   {{:terminal, ~r/^a/}, nil}}, nil}})
    assert parse("", grammar) == :mismatch
    assert parse("abc", grammar) == {:ok, {{:one_or_more, {{:terminal, ~r/^a/}, nil}},
                                           [{{:terminal, ~r/^a/}, "a"}]}, "bc"}
    assert parse("aabc", grammar) == {:ok, {{:one_or_more, {{:terminal, ~r/^a/}, nil}},
                                            [{{:terminal, ~r/^a/}, "a"},
                                             {{:terminal, ~r/^a/}, "a"}]}, "bc"}
  end

  test "parse zero or one" do
    grammar = new(:root, %{root: {{:zero_or_more,
                                   {{:terminal, ~r/^a/}, nil}}, nil}})
    assert parse("", grammar) == {:ok, {{:zero_or_more,
                                         {{:terminal, ~r/^a/}, nil}}, []}, ""}
    assert parse("abc", grammar) == {:ok, {{:zero_or_more, {{:terminal, ~r/^a/}, nil}},
                                           [{{:terminal, ~r/^a/}, "a"}]}, "bc"}
    assert parse("aabc", grammar) == {:ok, {{:zero_or_more, {{:terminal, ~r/^a/}, nil}},
                                            [{{:terminal, ~r/^a/}, "a"},
                                             {{:terminal, ~r/^a/}, "a"}]}, "bc"}
  end

  test "parse and" do
    grammar = new(:root, %{root: {{:and, {{:terminal, ~r/^a/}, nil}}, nil}})
    assert parse("a", grammar) == {:ok, {{:and, {{:terminal, ~r/^a/}, nil}},
                                         {{:terminal, ~r/^a/}, nil}}, "a"}
    assert parse("b", grammar) == :mismatch
  end

  test "parse not" do
    grammar = new(:root, %{root: {{:not, {{:terminal, ~r/^a/}, nil}}, nil}})
    assert parse("b", grammar) == {:ok, {{:not, {{:terminal, ~r/^a/}, nil}},
                                         {{:terminal, ~r/^a/}, nil}}, "b"}
    assert parse("a", grammar) == :mismatch
  end

  test "parse transforms" do
    # Parse a number, and transform it from a string to an integer
    int_transform = fn(number) -> {:ok, String.to_integer(number)} end
    int_expr = {:terminal, ~r/[0-9]+/}
    grammar = new(:root, %{root: {int_expr, int_transform}})
    assert parse("1", grammar) == {:ok, {{:terminal, ~r/[0-9]+/}, 1}, ""}

    # Parse a simple addition
    add_transform = fn(x, _, y) -> {:ok, x + y} end
    grammar = new(:root, %{root: {{:sequence, [{:nonterminal, :integer},
                                               {:terminal, "+"},
                                               {:nonterminal, :integer}]},
                                   add_transform},
                           integer: {int_expr, int_transform}})
    assert parse("1+1", grammar) == :ok
  end
end