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

  @doc """
  Trace the Neotomex.Grammar match calls using `dbg`.
  """
  @spec trace :: :ok
  def trace do
    Dbg.trace(self, :call)
    for fun <- [&Neotomex.Grammar.match/2,
                &Neotomex.Grammar.match/3] do
      Dbg.local_call(fun)
    end
    :ok
  end

  @doc """
  Stop any active traces.
  """
  def clear_trace do
    Dbg.clear
  end
end
