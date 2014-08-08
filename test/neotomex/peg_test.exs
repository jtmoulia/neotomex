defmodule Neotomex.PEGTest do
  use ExUnit.Case

  test "validate the PEG grammar" do
    assert Neotomex.Grammar.validate(Neotomex.PEG.grammar) == :ok
  end

  # test "parse PEG format using PEG grammar" do
  #   assert Neotomex.PEG.parse("a -> a") == true
  # end
end