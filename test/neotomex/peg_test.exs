defmodule Neotomex.PEGTest do
  use ExUnit.Case
  require Logger

  test "validate the PEG grammar" do
    assert Neotomex.Grammar.validate(Neotomex.PEG.grammar) == :ok
  end

  test "match PEG grammar using neotomex PEG metagrammar" do
    assert {:ok, _, ""} = Neotomex.PEG.match("A <- a")
    assert {:ok, _, ""} = Neotomex.PEG.match("A1 <- abra")
    assert {:ok, _, ""} = Neotomex.PEG.match("A <- 'a'")
    assert {:ok, _, ""} = Neotomex.PEG.match("A <- B 'a'\rB <- 'b'")
    assert {:ok, _, ""} = Neotomex.PEG.match("A <- [a-zA-Z]")
    assert {:ok, _, ""} = Neotomex.PEG.match("A <- [a-zA-Z0-9]")
  end
end