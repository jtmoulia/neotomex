defmodule Neotomex.Mixfile do
  use Mix.Project
  use Mix.Config

  def project do
    [app: :neotomex,
     version: "0.0.1",
     elixir: "~> 0.15.0",
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
    A PEG implementation.
    """
  end
end
