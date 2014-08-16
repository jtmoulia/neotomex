defmodule Neotomex.GrammarTest do
  use ExUnit.Case
  import Neotomex.Grammar, only: [new: 2, match: 2, parse: 2,
                                  transform_match: 1, validate: 1]
  doctest Neotomex.Grammar

  # defmodule Number do
  #   use Neotomex.Grammar

  #   # IO.inspect((testin string, do: string).(6))
  #   # IO.inspect quote do: fn(string = 5) -> string end

  #   testin do: (a -> a)
  #   definition :root, "[0-9]+",
  #     transform: fn(x) -> String.to_integer(x) end

  # end

  # test "macro interface (Number)" do
  #   assert Number.parse("1") == 1
  # end


  test "parse" do
      grammar = new(:root, %{root: {:terminal, ~r/^[0-9]+/}})
      assert parse(grammar, "1") == {:ok, "1", ""}

      grammar = new(:root, %{root: {{:terminal, ~r/^[0-9]+/}, &String.to_integer/1}})
      assert parse(grammar, "1") == {:ok, 1, ""}
      assert parse(grammar, "100") == {:ok, 100, ""}
  end

  test "match terminal" do
    expression = {:terminal, ~r/^.*/}
    grammar = new(:root, %{root: expression})
    assert match(grammar, "") == {:ok, {{expression, nil}, ""}, ""}
    assert match(grammar, "test") == {:ok, {{expression, nil}, "test"}, ""}

    expression = {:terminal, ~r/^a/}
    grammar = new(:root, %{root: expression})
    assert match(grammar, "") == :mismatch
    assert match(grammar, "abc") == {:ok, {{expression, nil}, "a"}, "bc"}

    expression1 = {:nonterminal, :non}
    expression2 = {:terminal, ~r/^.*/}
    grammar = new(:root, %{root: expression1, non: expression2})
    assert match(grammar, "") == {:ok, {{{:nonterminal, :non}, nil},
                                        {{{:terminal, ~r/^.*/}, nil}, ""}}, ""}
    assert match(grammar, "test") == {:ok, {{{:nonterminal, :non}, nil},
                                            {{{:terminal, ~r/^.*/}, nil}, "test"}}, ""}

    expression = {:terminal, ?a}
    grammar = new(:root, %{root: expression})
    assert match(grammar, "") == :mismatch
    assert match(grammar, "b") == :mismatch
    assert match(grammar, "a") == {:ok, {{{:terminal, 97}, nil}, 97}, ""}
  end

  test "match sequence" do
    expression = {:sequence, [{:terminal, ~r/^a/}, {:terminal, ~r/^b/}]}
    grammar = new(:root, %{root: expression})
    assert match(grammar, "") == :mismatch
    assert match(grammar, "a") == :mismatch
    assert match(grammar, "ab") ==
      {:ok, {{{:sequence, [terminal: ~r/^a/, terminal: ~r/^b/]}, nil},
             [{{{:terminal, ~r/^a/}, nil}, "a"},
              {{{:terminal, ~r/^b/}, nil}, "b"}]}, ""}
    assert match(grammar, "aab") == :mismatch
  end

  test "match priority" do
    expression = {:priority, [{:terminal, ~r/^a/}, {:terminal, ~r/^b/}]}
    grammar = new(:root, %{root: expression})
    assert match(grammar, "") == :mismatch
    assert match(grammar, "a") ==
      {:ok, {{{:priority, [terminal: ~r/^a/, terminal: ~r/^b/]}, nil},
             {{{:terminal, ~r/^a/}, nil}, "a"}}, ""}
    assert match(grammar, "ab") ==
      {:ok, {{{:priority, [terminal: ~r/^a/, terminal: ~r/^b/]}, nil},
             {{{:terminal, ~r/^a/}, nil}, "a"}}, "b"}
    assert match(grammar, "ba") ==
      {:ok, {{{:priority, [terminal: ~r/^a/, terminal: ~r/^b/]}, nil},
             {{{:terminal, ~r/^b/}, nil}, "b"}}, "a"}
  end

  test "match zero or more" do
    grammar = new(:root, %{root: {:zero_or_more, {:terminal, ~r/^a/}}})
    assert match(grammar, "") ==
      {:ok, {{{:zero_or_more, {:terminal, ~r/^a/}}, nil}, []}, ""}
    assert match(grammar, "ab") ==
      {:ok, {{{:zero_or_more, {:terminal, ~r/^a/}}, nil},
             [{{{:terminal, ~r/^a/}, nil}, "a"}]}, "b"}
    assert match(grammar, "aab") ==
      {:ok, {{{:zero_or_more, {:terminal, ~r/^a/}}, nil},
             [{{{:terminal, ~r/^a/}, nil}, "a"},
              {{{:terminal, ~r/^a/}, nil}, "a"}]}, "b"}
  end

  test "match one or more" do
    grammar = new(:root, %{root: {:one_or_more, {:terminal, ~r/^a/}}})
    assert match(grammar, "") == :mismatch
    assert match(grammar, "abc") == {:ok, {{{:one_or_more, {:terminal, ~r/^a/}}, nil},
                                           [{{{:terminal, ~r/^a/}, nil}, "a"}]}, "bc"}
    assert match(grammar, "aabc") == {:ok, {{{:one_or_more, {:terminal, ~r/^a/}}, nil},
                                            [{{{:terminal, ~r/^a/}, nil}, "a"},
                                             {{{:terminal, ~r/^a/}, nil}, "a"}]}, "bc"}
  end

  test "match zero or one" do
    grammar = new(:root, %{root: {:zero_or_more, {:terminal, ~r/^a/}}})
    assert match(grammar, "") == {:ok, {{{:zero_or_more,
                                          {:terminal, ~r/^a/}}, nil}, []}, ""}
    assert match(grammar, "abc") == {:ok, {{{:zero_or_more, {:terminal, ~r/^a/}}, nil},
                                           [{{{:terminal, ~r/^a/}, nil}, "a"}]}, "bc"}
    assert match(grammar, "aabc") == {:ok, {{{:zero_or_more, {:terminal, ~r/^a/}}, nil},
                                            [{{{:terminal, ~r/^a/}, nil}, "a"},
                                             {{{:terminal, ~r/^a/}, nil}, "a"}]}, "bc"}
  end

  test "match and" do
    grammar = new(:root, %{root: {:and, {:terminal, ~r/^a/}}})
    assert match(grammar, "a") == {:ok, {{{:and, {:terminal, ~r/^a/}}, nil},
                                         nil}, "a"}
    assert match(grammar, "b") == :mismatch
  end

  test "match not" do
    grammar = new(:root, %{root: {:not, {:terminal, ~r/^a/}}})
    assert match(grammar, "b") == {:ok, {{{:not, {:terminal, ~r/^a/}}, nil}, nil}, "b"}
    assert match(grammar, "a") == :mismatch
  end

  test "transform" do
    assert transform_match({{nil, fn x -> String.to_integer(x) end},
                            {{nil, nil}, "1"}}) == 1

    match = {{nil, fn [x, y] -> x + y end}, [{{nil, nil}, 1}, {{nil, nil}, 1}]}
    assert transform_match(match) == 2
  end

end