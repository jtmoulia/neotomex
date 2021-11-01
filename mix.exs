defmodule Neotomex.Mixfile do
  use Mix.Project
  use Mix.Config

  @github "https://github.com/jtmoulia/neotomex"

  def project do
    [app: :neotomex,
     version: "0.1.7",
     elixir: ">= 1.3.0",
     name: "Neotomex",
     source_url: @github,
     deps: deps(),
     description: description(),
     package: package()]
  end

  def application do
    [applications: applications(Mix.env)]
  end

  defp applications(:dev) do
    [:dbg]
  end
  defp applications(_) do
    []
  end

  defp deps do
    [{:dbg, "~> 1.0", only: [:dev, :test]},
     {:earmark, "~> 1.2", only: :dev},
     {:ex_doc, "~> 0.16", only: :dev}]
  end

  defp description do
    """
    A PEG parser/transformer with a pleasant Elixir DSL.
    """
  end

  defp package do
    [maintainers: ["Thomas Moulia"],
     licenses: ["BSD 3-Clause License"],
     links: %{github: @github}]
  end
end
