defmodule Neotomex.Mixfile do
  use Mix.Project
  use Mix.Config

  def project do
    [app: :neotomex,
     version: "0.1.2",
     elixir: "~> 0.15.0",
     deps: deps,
     description: description,
     package: package]
  end

  def application do
    [applications: []]
  end

  defp deps do
    []
  end

  defp description do
    """
    A PEG parser/transformer with a pleasant Elixir DSL.
    """
  end

  defp package do
    [contributors: ["Thomas Moulia"],
     licenses: ["BSD 3-Clause License"],
     links: %{github: "https://github.com/jtmoulia/neotomex"}]
  end
end
