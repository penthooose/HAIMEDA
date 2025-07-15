defmodule PreProcessing.Tableaux do
  alias PreProcessing.UniverseState

  def to_cond(id) do
    {:cond, id}
  end

  def to_set(name, set) when is_map(set) do
    {:set, String.to_atom(name), set}
  end

  def neg({:set, name, set}) when is_map(set) do
    # Get universe set (all conditions) to ensure proper negation
    universe = UniverseState.get_set("c_all")

    # Create complement using universe set
    complement = MapSet.difference(universe || MapSet.new(), set)

    # Create new set with negated name
    neg_name = String.to_atom("¬#{name}")

    {:set, neg_name, complement}
  end

  def neg({:cond, id}) do
    {:neg, {:cond, id}}
  end

  # Double negation elimination
  def neg({:neg, x}), do: x

  def neg(x), do: {:neg, x}

  # Update conjunction to automatically apply distributive law
  def conj(x, y) do
    result =
      case {x, y} do
        # If either operand is a bracket, preserve it but apply distributive law to contents
        {{:bracket, inner}, other} ->
          {:conj, {:bracket, apply_distributive(inner)}, other}

        {other, {:bracket, inner}} ->
          {:conj, other, {:bracket, apply_distributive(inner)}}

        # Set operations handled directly
        {{:set, name1, set1}, {:set, name2, set2}} when is_map(set1) and is_map(set2) ->
          new_name = combine_names(:conj, name1, name2)
          {:set, new_name, MapSet.intersection(set1, set2)}

        # Default case - create conjunction then distribute
        _ ->
          {:conj, x, y}
      end

    # Always apply distributive law to result
    apply_distributive(result)
  end

  # Update disjunction similarly
  def disj(x, y) do
    result =
      case {x, y} do
        # If either operand is a bracket, preserve it but apply distributive law to contents
        {{:bracket, inner}, other} ->
          {:disj, {:bracket, apply_distributive(inner)}, other}

        {other, {:bracket, inner}} ->
          {:disj, other, {:bracket, apply_distributive(inner)}}

        # Set operations handled directly
        {{:set, name1, set1}, {:set, name2, set2}} when is_map(set1) and is_map(set2) ->
          new_name = combine_names(:disj, name1, name2)
          {:set, new_name, MapSet.union(set1, set2)}

        # Default case - create disjunction then distribute
        _ ->
          {:disj, x, y}
      end

    # Always apply distributive law to result
    apply_distributive(result)
  end

  def impl(x, y) do
    disj(neg(x), y)
  end

  def iff(x, y) do
    conj(impl(x, y), impl(y, x))
  end

  def bot(x) do
    {:bot, x}
  end

  def top(x) do
    {:top, x}
  end

  def for_all(var, domain, formula) do
    {:for_all, var, domain, formula}
  end

  def exists(var, domain, formula) do
    {:exists, var, domain, formula}
  end

  # New functions for handling brackets and distributive law
  def bracket(x) do
    {:bracket, x}
  end

  def distribute({:conj, x, {:disj, y, z}}) do
    {:disj, {:conj, x, y}, {:conj, x, z}}
  end

  def distribute({:conj, {:disj, x, y}, z}) do
    {:disj, {:conj, x, z}, {:conj, y, z}}
  end

  #### Functions

  # Comparison functions
  def lt(x, y), do: {:fun, :lt, [x, y]}
  def gt(x, y), do: {:fun, :gt, [x, y]}
  def leq(x, y), do: {:fun, :leq, [x, y]}
  def geq(x, y), do: {:fun, :geq, [x, y]}
  def eq(x, y), do: {:fun, :eq, [x, y]}
  def neq(x, y), do: {:fun, :neq, [x, y]}

  # Custom function application
  def apply_fun(name, args), do: {:fun, name, args}

  # Basic set operations
  def element(x, set) do
    case {x, set} do
      # Direct element check
      {{:cond, id}, {:set, _name, set_content}} when is_map(set_content) ->
        if MapSet.member?(set_content, id) do
          {:set, :empty, MapSet.new([id])}
        else
          {:set, :empty, MapSet.new()}
        end

      # Element in conjunction of sets: x ∈ (A ∧ B) ⟺ (x ∈ A) ∧ (x ∈ B)
      {x, {:conj, set1, set2}} ->
        conj(element(x, set1), element(x, set2))

      # Element in disjunction of sets: x ∈ (A ∨ B) ⟺ (x ∈ A) ∨ (x ∈ B)
      {x, {:disj, set1, set2}} ->
        disj(element(x, set1), element(x, set2))

      # Element in negated set: x ∈ ¬A ⟺ ¬(x ∈ A)
      {x, {:neg, set}} ->
        neg(element(x, set))

      # Default case: return empty set (false)
      _ ->
        {:set, :empty, MapSet.new()}
    end
  end

  # Update subset to return function term instead of evaluating
  def subset(set1, set2) do
    {:fun, :subset, [set1, set2]}
  end

  # Add helper to evaluate set expressions
  defp evaluate_set(expr) do
    case expr do
      {:set, _name, set} when is_map(set) ->
        {:ok, set}

      {:neg, {:set, _name, set}} when is_map(set) ->
        universe = UniverseState.get_set("c_all")
        {:ok, MapSet.difference(universe || MapSet.new(), set)}

      _ ->
        :error
    end
  end

  # Helper to extract element from term
  defp elem_from_term({:cond, id}), do: id
  defp elem_from_term(_), do: nil

  # Add new functions for condition handling
  def cond_in_set(id, set) do
    {:fun, :subset, [{:cond, id}, set]}
  end

  # Helper to extract elements from term
  defp extract_elements(term) do
    case term do
      {:set, _name, set} when is_map(set) -> MapSet.to_list(set)
      {:cond, id} -> [id]
      _ -> []
    end
  end

  #### Helper functions

  # Enhanced set name handling
  defp combine_names(op, name1, name2) do
    # Convert inputs to strings
    n1 = if is_atom(name1), do: Atom.to_string(name1), else: to_string(name1)
    n2 = if is_atom(name2), do: Atom.to_string(name2), else: to_string(name2)

    # Clean up existing brackets
    n1 = String.trim(n1, "()")
    n2 = String.trim(n2, "()")

    # Determine if we need brackets around operands
    n1_needs_brackets = String.contains?(n1, ["∨", "∧"])
    n2_needs_brackets = String.contains?(n2, ["∨", "∧"])

    # Add brackets if needed
    n1 = if n1_needs_brackets, do: "(#{n1})", else: n1
    n2 = if n2_needs_brackets, do: "(#{n2})", else: n2

    # Choose operator symbol
    operator =
      case op do
        :conj -> "∧"
        :disj -> "∨"
        _ -> "•"
      end

    # Create combined name with proper spacing
    result = "#{n1} #{operator} #{n2}"

    # Convert to atom, preserving UTF-8 characters
    String.to_atom(result)
  end

  # Helper for domain substitution
  def substitute(formula, var, value) do
    case formula do
      {:cond, a} -> {:cond, a}
      {:neg, f} -> {:neg, substitute(f, var, value)}
      {:conj, f1, f2} -> {:conj, substitute(f1, var, value), substitute(f2, var, value)}
      {:disj, f1, f2} -> {:disj, substitute(f1, var, value), substitute(f2, var, value)}
      # {:var, ^var} -> {:atom, value}
      other -> other
    end
  end

  # Update apply_distributive to handle set combinations better
  def apply_distributive(formula) do
    case formula do
      # Special case for combined dimension sets
      {:disj, {:set, name1, set1}, {:set, name2, set2}} when is_map(set1) and is_map(set2) ->
        {:set, combine_names(:disj, name1, name2), MapSet.union(set1, set2)}

      {:conj, {:set, name1, set1}, {:set, name2, set2}} when is_map(set1) and is_map(set2) ->
        {:set, combine_names(:conj, name1, name2), MapSet.intersection(set1, set2)}

      {:conj, x, y} = term ->
        case {apply_distributive(x), apply_distributive(y)} do
          # Handle sets with universe awareness
          {{:set, name1, set1}, {:set, name2, set2}} when is_map(set1) and is_map(set2) ->
            universe1 = UniverseState.get_set(Atom.to_string(name1))
            universe2 = UniverseState.get_set(Atom.to_string(name2))
            intersection = MapSet.intersection(set1, set2)

            # Ensure intersection is within universe bounds
            valid_intersection =
              if universe1 && universe2 do
                MapSet.intersection(intersection, MapSet.intersection(universe1, universe2))
              else
                intersection
              end

            {:set, name1, valid_intersection}

          # Handle nested disjunctions
          {{:disj, x1, x2}, {:disj, y1, y2}} ->
            {:disj,
             {:disj, apply_distributive({:conj, x1, y1}), apply_distributive({:conj, x1, y2})},
             {:disj, apply_distributive({:conj, x2, y1}), apply_distributive({:conj, x2, y2})}}

          # Handle disjunction in left operand
          {{:disj, x1, x2}, y_dist} ->
            {:disj, apply_distributive({:conj, x1, y_dist}),
             apply_distributive({:conj, x2, y_dist})}

          # Handle disjunction in right operand
          {x_dist, {:disj, y1, y2}} ->
            {:disj, apply_distributive({:conj, x_dist, y1}),
             apply_distributive({:conj, x_dist, y2})}

          # No disjunctions to distribute
          {x_dist, y_dist} ->
            {:conj, x_dist, y_dist}
        end

      {:disj, x, y} ->
        {:disj, apply_distributive(x), apply_distributive(y)}

      {:neg, x} ->
        {:neg, apply_distributive(x)}

      {:bracket, x} ->
        apply_distributive(x)

      {:fun, :subset, [{:cond, id}, set]} ->
        # Keep subset with condition ID intact
        {:fun, :subset, [{:cond, id}, apply_distributive(set)]}

      other ->
        other
    end
  end

  # Update string conversion to be cleaner
  def to_hd(term) do
    case term do
      {:set, name, _set} when is_atom(name) ->
        # Already formatted by combine_names
        "#{name}"

      {:set, {:neg, name}, _set} ->
        "¬#{to_hd(name)}"

      # Already formatted string
      {:set, name, _set} when is_binary(name) ->
        name

      {:set, {:conj, n1, n2}, _set} ->
        combine_names(:conj, n1, n2)

      {:set, {:disj, n1, n2}, _set} ->
        combine_names(:disj, n1, n2)

      {:cond, id} ->
        "#{id}"

      {:neg, x} ->
        "¬#{to_hd(x)}"

      {:conj, x, y} ->
        combine_names(:conj, x, y)

      {:disj, x, y} ->
        combine_names(:disj, x, y)

      {:bracket, x} ->
        "(#{to_hd(x)})"

      {:compare, op, x, y} ->
        symbol =
          case op do
            :lt -> "<"
            :gt -> ">"
            :leq -> "≤"
            :geq -> "≥"
            :eq -> "="
            :neq -> "≠"
          end

        "#{to_hd(x)} #{symbol} #{to_hd(y)}"

      {:fun, name, args} when is_list(args) ->
        args_str = Enum.map_join(args, ", ", &to_hd/1)
        "#{name}(#{args_str})"

      {:fun, op, [x, y]} when op in [:lt, :gt, :leq, :geq, :eq, :neq] ->
        symbol =
          case op do
            :lt -> "<"
            :gt -> ">"
            :leq -> "≤"
            :geq -> "≥"
            :eq -> "="
            :neq -> "≠"
          end

        "#{to_hd(x)} #{symbol} #{to_hd(y)}"

      {:fun, :subset, [x, y]} ->
        "#{to_hd(x)} ⊆ #{to_hd(y)}"

      x when is_atom(x) ->
        "#{x}"

      # Already formatted string
      x when is_binary(x) ->
        x

      _ ->
        "#{inspect(term)}"
    end
  end

  #### Macros for ease of use

  defmacro ~~~x do
    quote do
      {:neg, unquote(x)}
    end
  end

  defmacro left &&& right do
    quote do
      conj(unquote(left), unquote(right))
    end
  end

  defmacro left ||| right do
    quote do
      disj(unquote(left), unquote(right))
    end
  end

  defmacro left ~> right do
    quote do
      impl(unquote(left), unquote(right))
    end
  end

  defmacro left <~> right do
    quote do
      iff(unquote(left), unquote(right))
    end
  end

  # Add macros for nicer syntax
  defmacro all(var, domain, do: body) do
    quote do
      for_all(unquote(var), unquote(domain), unquote(body))
    end
  end

  defmacro exist(var, domain, do: body) do
    quote do
      exists(unquote(var), unquote(domain), unquote(body))
    end
  end

  # defmacro <<<(body)>>> do
  #   quote do
  #     bracket(unquote(body))
  #   end
  # end
end
