defmodule Neotomex.Grammar do
  @moduledoc """
  # Neotomex.Grammar

  A Neotomex PEG grammar specifies a directed graph of definitons.
  It consists of:

  - A set of definitions, where each definition consists of an
    `identifier` and an `expression`.
    e.g. `Definition <- Identifier '<-' Expression`
  - The root definition's identifier. This root definition will be
    used as the entry point for matching.
  - A transform function can be associated with a Neotomex expression.
    They are applied after the parse

  The definition data types (see the definition/0 typespec below) are
  verbose and machine readable as opposed to human readable. Typical
  usage will be to compile a grammar from a more machine readable
  format, bootstrapping with Neotomex grammars as necessary.

  Parsing consists of first using the grammar to `match` a valid
  parse tree, and then applying a `transform` node by node to
  this parse tree.

  ## Match

  A grammar consists of definitions which are labeled by an identifier
  and consist of an expression. An expression can be a

  - `terminal`: references a char, string, or regex. It successfully
    matches when it's reference appears at the beginning of the input.
  - `nonterminal`: the identifier of a definition. It matches if the
    identified definition matches.
  - `sequence`: an ordered list of expressions. It succesfully matches
    by folding the expressions against the input.
  - `priority`: an ordered list of expressions. It matches using the
    first successful match of the list.
  - `zero_or_more`: matches when its subexpression greedily matches zero or
    more times.
  - `one_or_more`: matches when its subexpression greedily matches one or
    more times.
  - `zero_or_one`: matches when its subexpression greedily matches zero or
    one times.
  - `and`: matches when its subexpression matches. Consumes no input.
  - `not`: matches when its subexpression does not match. Consumes no input.
  - `prune`: matches when it's subexpression matches, however the match
    is pruned from the result.

  A grammar's definitions come together to form a conditional directed graph.
  Matching starts at the root node and performs a depth first search for the
  first successful route through the tree. This represents the valid parse
  tree for the grammar and input.

  ## Transform

  A transform is a function that modifies the match for an expression. This
  can be used to evaluate the parse tree. When applying transforms to
  the matched tree, they are applied depth first.

  The default transform is the identity function.

  Transforms must return `transformed` when successful, where
  `transformed` is the transformed match.
  """

  # Neotomex parses PEG expressions into this internal representation
  @type terminal :: binary | char | Regex.t | nil
  @type nonterminal :: atom

  @type expression :: :empty
                    | {:terminal, terminal}
                    | {:nonterminal, nonterminal}
                    | {:sequence [expression]}
                    | {:priority, [expression]}
                    | {:zero_or_more, expression}
                    | {:one_or_more, expression}
                    | {:zero_or_one, expression}
                    | {:and, expression}
                    | {:not, expression}
                    | {:prune, expression}

  @type definition :: {nonterminal, expression}
  @type transform :: {:transform, ((term) -> {:ok, term})}
                   | {:transform, {atom(), atom()}}
                   | nil
  @type expr_trans :: {expression, transform}


  # A match is the result of the Neotomex PEG grammar matching an expression
  @typep match :: {expr_trans, [match] | match | String.t}

  # A grammar contains a root label, and a map of rules keyed by nonterminal label
  @type grammar :: %{root: nonterminal | false,
                     definitions: %{nonterminal => expression},
                     cache: pid() | false}

  @doc """
  Returns an empty grammar.
  """
  @spec new :: grammar
  def new(root \\ false, definitions \\ %{}, cache \\ %{}) do
    %{root: root, definitions: definitions, cache: cache}
  end


  @doc """
  Add a rule to the grammar.

  Set the definition as the root of the grammar if not yet set.
  """
  @spec add_rule(grammar, definition) :: grammar
  def add_rule(%{:root => false} = grammar, {ident, _} = definition) do
    add_rule(%{grammar | :root => ident}, definition)
  end
  def add_rule(%{:definitions => definitions} = grammar, {identity, expr_trans}) do
    %{grammar | :definitions => Map.put(definitions, identity, expr_trans)}
  end


  @doc """
  Parse the `input` using the `grammar` by matching a parse tree and
  then applying all transforms.

  ## Examples

      iex> grammar = new(:root, %{root: {{:terminal, ~r/^[0-9]+/},
      ...>                               {:transform, &String.to_integer/1}}})
      iex> parse(grammar, "1")
      {:ok, 1, ""}
      iex> parse(grammar, "100")
      {:ok, 100, ""}

  """
  @spec parse(grammar, binary) :: {:ok, any, binary} | :mismatch | {:error, term}
  def parse(grammar, input) when is_binary(input) do
    case match(grammar, input) do
      {:ok, match, rest} ->
        {:ok, transform_match(match), rest}
      otherwise ->
        otherwise
    end
  end


  @doc """
  Match the `input` using the grammar.

  NB: Not tail call optimized. Possible?
  """
  @spec match(grammar, binary) :: {:ok, {expression, transform}, binary}
                                | :mismatch | {:error, term}
  def match(%{:root => root, :definitions => definitions} = grammar, input)
      when is_binary(input) do
    match(definitions[root], grammar, input)
  end


  @doc """
  Transform the parse tree returned by match by applying the the
  expressions' transform functions via depth first recursion.

  NB: Not tail call optimized. Possible? Pack rat?

  ## Examples

      iex> transform_match({{nil, {:transform, fn x -> String.to_integer(x) end}},
      ...>                  {{nil, nil}, "1"}})
      1
      iex> transform_match({{nil, {:transform, fn [x, y] -> x + y end}},
      ...>                  [{{nil, nil}, 1}, {{nil, nil}, 1}]})
      2
  """
  @spec transform_match(match) :: any
  def transform_match(nil),              do: nil
  def transform_match({_, {:prune, _}}), do: nil
  def transform_match({{_, nil}, terminal})
      when is_binary(terminal) or is_integer(terminal) do
    terminal
  end
  def transform_match({{_, nil}, matches}) when is_list(matches) do
    transform_prune(matches)
  end
  def transform_match({{_, nil}, match}) do
    transform_match(match)
  end
  def transform_match({{_, transform}, matches}) when is_list(matches) do
    apply_transform(transform, transform_prune(matches))
  end
  def transform_match({{_, transform}, match}) when is_binary(match) do
    apply_transform(transform, match)
  end
  def transform_match({{_, transform}, match}) do
    apply_transform(transform, transform_match(match))
  end


  @doc """
  Validate the grammar. This is especially useful for
  debugging a grammar since it is exhaustive and provides richer error
  reporting than a failed match.

  Notes:

  - Dialyzer is your friend -- `validate/1` augments it
  - Grammar's are not validated by default due to the performance overhead.
  - The validation will return the result of the first failure. There may
    be more issues with the grammar.

  Validation checks that:

  - The grammar has a `:root` fields.
  - The grammar has a `:definitions` fields.
  - The root references a definition
  - All nonterminals reference a definition.
  - There are no unreferenced definitions [TODO]

  ## Examples

  More complex examples can be found in `test/neotomex/grammar_test.exs` [todo]

      iex> validate(%{})
      {:error, {:missing, :root}}
      iex> validate(%{:root => :root})
      {:error, {:missing, :definitions}}
      iex> validate(%{:root => :root, :definitions => %{}})
      {:error, {:missing, :root_definition}}
      iex> validate(%{:root => :root,
      ...>            :definitions => %{:root => {:nonterminal, :reference}}})
      {:error, {:bad_definition, {:root, {:missing, {:definition, :reference}}}}}
      iex> validate(%{:root => :root,
      ...>            :definitions => %{:root => {:bad_type, :nonsense}}})
      {:error, {:bad_definition, {:root, {:bad_expr_type, :bad_type}}}}
      iex> validate(%{:root => :root, :definitions => %{:root => :the_bad_expr}})
      {:error, {:bad_definition, {:root, {:bad_expr, :the_bad_expr}}}}
      iex> validate(%{:root => :root,
      ...>            :definitions => %{:root => {:terminal, ?a}}})
      :ok
  """
  @spec validate(grammar) :: :ok |
    {:error, {:missing, :root
                      | :definitions
                      | :root_definitions
                      | {:definition, atom}}}
  def validate(%{:root => root, :definitions => definitions} = grammar) when
      root != nil and definitions != nil do
    case Dict.has_key?(definitions, root) do
      true ->
        validate(grammar, Dict.to_list(definitions))
      false ->
        {:error, {:missing, :root_definition}}
    end
  end
  def validate(grammar) do
    # Catch if root or defintions aren't present
    case {Dict.has_key?(grammar, :root), Dict.has_key?(grammar, :definitions)} do
      {false, _} ->
        {:error, {:missing, :root}}
      {_, false} ->
        {:error, {:missing, :definitions}}
    end
  end


  ## Private Functions

  defp match({identifier, _} = expr, grammar, input) when is_atom(identifier) do
    # If no transform is provided, default it to `nil`
    match({expr, nil}, grammar, input)
  end

  defp match(nil, _, _),        do: {:error, :no_node_ref}
  defp match(:empty, _, input), do: {:ok, nil, input}

  # Terminal nodes can be a char, string, or regex
  defp match({{:terminal, char}, _} = expr_trans, _, <<char, rest :: binary>>)
      when is_integer(char) do
    {:ok, {expr_trans, char}, rest}
  end
  defp match({{:terminal, char}, _}, _, _) when is_integer(char) do
    :mismatch
  end
  defp match({{:terminal, terminal}, _} = expr_trans, _, input)
      when is_binary(terminal) do
    case String.split_at(input, String.length(terminal)) do
      {^terminal, rest} ->
        {:ok, {expr_trans, terminal}, rest}
      {_, _} ->
        :mismatch
    end
  end
  defp match({{:terminal, terminal}, _} = expr_trans, _, input) do
    case Regex.run(terminal, input) do
      [""] ->
        # Make sure it's not just an empty match
        case Regex.match?(terminal, input) do
          true ->
            {:ok, {expr_trans, ""}, input}
          false ->
            :mismatch
        end
      nil ->
        :mismatch
      [match] ->
        # Two parts are necessary since the first is being trimmed away
        {^match, rest} = String.split_at(input, String.length(match))
        {:ok, {expr_trans, match}, rest}
    end
  end

  defp match({{:nonterminal, nonterminal}, _} = expr_trans,
             %{:definitions => definitions} = grammar, input) do
    case match(definitions[nonterminal], grammar, input) do
      {:ok, match, rest} ->
        {:ok, {expr_trans, match}, rest}
      otherwise ->
        otherwise
    end
  end

  defp match({{:sequence, _}, _} = expr_trans, grammar, input) do
    match_sequence(expr_trans, grammar, input)
  end

  defp match({{:priority, _}, _} = expr_trans, grammar, input) do
    match_priorities(expr_trans, grammar, input)
  end

  defp match({{:zero_or_more, _}, _} = expr_trans, grammar, input) do
   match_zero_or_more(expr_trans, grammar, input)
  end

  defp match({{:one_or_more, expression}, _} = expr_trans, grammar, input) do
    case match(expression, grammar, input) do
      {:ok, match, input} ->
        match_zero_or_more(expr_trans, grammar, input, [match])
      otherwise ->
        otherwise
    end
  end

  defp match({{:zero_or_one, expression}, _} = expr_trans, grammar, input) do
    case match(expression, grammar, input) do
      :mismatch ->
        {:ok, {expr_trans, nil}, input}
      otherwise ->
        otherwise
    end
  end

  defp match({{:and, expression}, _} = expr_trans, grammar, input) do
    case match(expression, grammar, input) do
      {:ok, _, _} ->
        {:ok, {expr_trans, nil}, input}
      otherwise ->
        otherwise
    end
  end

  defp match({{:not, expression}, _} = expr_trans, grammar, input) do
    case match(expression, grammar, input) do
      {:ok, _, _} ->
        :mismatch
      :mismatch ->
        {:ok, {expr_trans, nil}, input}
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp match({{:prune, expression}, _} = expr_trans, grammar, input) do
    case match(expression, grammar, input) do
      {:ok, match, input} ->
        {:ok, {expr_trans, {:prune, match}}, input}
      otherwise ->
        otherwise
    end
  end


  # Helper for parsing a sequence of expressions
  defp match_sequence({{:sequence, expressions}, _} = expr_trans, grammar, input) do
    match_sequence(expr_trans, grammar, input, expressions, [])
  end

  defp match_sequence({{:sequence, _}, _} = expr_trans, _, input, [], acc) do
    {:ok, {expr_trans, Enum.reverse(acc)}, input}
  end
  defp match_sequence(expr_trans, grammar, input, [expression | expressions], acc) do
    case match(expression, grammar, input) do
      {:ok, match, input} ->
        match_sequence(expr_trans, grammar, input, expressions, [match | acc])
      otherwise ->
        otherwise
    end
  end


  # Helper for parsing a priority list of expressions
  @doc false
  defp match_priorities({{:priority, expressions}, _} = expr_trans, grammar, input) do
    match_priorities(expr_trans, grammar, input, expressions)
  end

  @doc false
  defp match_priorities(_, _, _, []), do: :mismatch
  defp match_priorities(expr_trans, grammar, input, [expression | expressions]) do
    case match(expression, grammar, input) do
      {:ok, match, input} ->
        {:ok, {expr_trans, match}, input}
      :mismatch ->
        match_priorities(expr_trans, grammar, input, expressions)
      {:error, reason} ->
        {:error, reason}
    end
  end


  # Helper for zero or more (* suffix). Also used for one or more.
  @doc false
  defp match_zero_or_more(expr_trans, grammar, input, acc \\ [])
  defp match_zero_or_more({{_, expression}, _} = expr_trans, grammar, input, acc) do
    case match(expression, grammar, input) do
      {:ok, match, input} ->
        match_zero_or_more(expr_trans, grammar, input, [match | acc])
      :mismatch ->
        {:ok, {expr_trans, Enum.reverse(acc)}, input}
      {:error, reason} ->
        {:error, reason}
    end
  end


  # Apply the transform to the provided arg
  @doc false
  defp apply_transform({:transform, {module, method}}, arg)
      when is_atom(module) and is_atom(method) do
    apply(module, method, [arg])
  end
  defp apply_transform({:transform, transform_fn}, arg)
      when is_function(transform_fn) do
    transform_fn.(arg)
  end

  # Helper for applying transform match while pruning
  @doc false
  defp transform_prune(matches) do
    for match <- matches,
        (case match do
           {_, {:prune, _}} -> false
           _                -> true
         end),
        do: transform_match(match)
  end


  @doc false
  defp validate(_grammar, []), do: :ok
  # Strip out transforms, which should be either nil or {:transform, _}
  defp validate(grammar, [{id, {expr, {:transform, _}}} | rest]) do
    validate(grammar, [{id, expr} | rest])
  end
  defp validate(grammar, [{id, {expr, nil}} | rest]) do
    validate(grammar, [{id, expr} | rest])
  end
  defp validate(grammar, [{id, expr} | rest]) do
    case validate_expr(grammar, expr) do
      :ok ->
        validate(grammar, rest)
      {:error, reason} ->
        {:error, {:bad_definition, {id, reason}}}
    end
  end

  @doc false
  defp validate_expr(%{:definitions => definitions}, {:nonterminal, id}) do
    # check that the referenced terminal exists in the grammar
    case Dict.has_key?(definitions, id) do
      true ->
        :ok
      false ->
        {:error, {:missing, {:definition, id}}}
    end
  end
  defp validate_expr(grammar, {list_expr_type, exprs})
      when list_expr_type == :sequence or list_expr_type == :priority do
    # handle a rule which wraps a list of rules
    if is_list(exprs) do
      validate_expr(grammar, exprs)
    else
      {:error, {:bad_rule, {list_expr_type, exprs}}}
    end
  end
  defp validate_expr(grammar, {wrap_expr_type, expr})
      when wrap_expr_type == :zero_or_more
        or wrap_expr_type == :zero_or_one
        or wrap_expr_type == :one_or_more
        or wrap_expr_type == :not
        or wrap_expr_type == :and
        or wrap_expr_type == :prune do
    # handle a expr which is wraps a single expression
    if is_tuple(expr) do
      validate_expr(grammar, expr)
    else
      {:error, {:bad_expr, {wrap_expr_type, expr}}}
    end
  end

  defp validate_expr(_grammar, []), do: :ok
  defp validate_expr(grammar, [expr | rest]) do
    # handle lists of exprs
    case validate_expr(grammar, expr) do
      :ok ->
        validate_expr(grammar, rest)
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp validate_expr(_grammar, {:terminal, terminal}) do
    if Regex.regex?(terminal) or is_binary(terminal) or is_integer(terminal) do
      :ok
    else
      {:bad_terminal, terminal}
    end
  end
  defp validate_expr(_grammar, {expr_type, _}) do
    {:error, {:bad_expr_type, expr_type}}
  end
  defp validate_expr(_grammar, expr),           do: {:error, {:bad_expr, expr}}
end
