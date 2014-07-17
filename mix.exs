defmodule Neotomex.Mixfile do
  use Mix.Project

  def project do
    [app: :neotomex,
     version: "0.0.1",
     elixir: "~> 0.14.3-dev",
     deps: deps,
     description: description]
  end

  def application do
    [applications: []]
  end

  defp deps do
    []
  end

  defp description do
    """
    A PEG parser.
    """
  end
end
