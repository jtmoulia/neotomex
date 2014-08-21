defmodule Neotomex do
  @moduledoc """
  - `Neotomex.Grammar` provides functions for manipulating grammars.
  - `Neotomex.ExGrammar` provides an interface for specifying grammars.
  """

  defmodule Error do
    @moduledoc """
    General `Neotomex` exceptions.
    """
    defexception [message: "neotomex error"]

    def message(exception) do
      exception.message
    end
  end
end
