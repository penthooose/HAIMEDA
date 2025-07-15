defmodule HaimedaCoreWeb.ReportsEditor.TipTapSnippets do
  use HaimedaCoreWeb, :html
  import Phoenix.HTML
  require Logger

  @doc """
  Formats an entity for TipTap with replacements.
  Takes the input entity, its replacements, color, and category.
  Returns a map representing the entity with proper TipTap marks.
  """
  def format_entity_for_tiptap(
        input_text,
        replacements,
        color \\ "#d8b5ff",
        category \\ "entity",
        entity_id \\ nil
      ) do
    # Ensure replacements is properly formatted as a list
    replacements_list = format_replacements(replacements)
    entity_id = entity_id || "entity_#{:erlang.monotonic_time()}_#{:rand.uniform(1000)}"

    # Create the entity with appropriate marks
    %{
      "type" => "text",
      "text" => input_text,
      "marks" => [
        %{
          "type" => "coloredEntity",
          "attrs" => %{
            "entityId" => entity_id,
            "entityType" => "replacement",
            "entityColor" => color,
            "entityCategory" => category,
            "originalText" => input_text,
            "currentText" => input_text,
            "replacements" => replacements_list
          }
        }
      ]
    }
  end

  # Helper to ensure replacements are properly formatted
  defp format_replacements(replacements) do
    case replacements do
      list when is_list(list) -> list
      bin when is_binary(bin) -> String.split(bin, ",") |> Enum.map(&String.trim/1)
      _ -> []
    end
  end

  @doc """
  Creates a TipTap document with an entity.
  Takes a text, position to insert entity, entity text, and replacements.
  Returns a complete TipTap document with the entity integrated.
  """
  def create_tiptap_document_with_entity(text, position, entity_text, replacements) do
    # Split the text at the position
    {before_text, after_text} = String.split_at(text, position)

    # Create document content with proper handling of entities at paragraph boundaries
    paragraph_content =
      cond do
        # Case 1: Entity at start of paragraph
        before_text == "" ->
          [
            # Add an invisible zero-width space before the entity for proper rendering
            # %{"type" => "text", "text" => "\u200B"},

            # The entity with marks
            format_entity_for_tiptap(entity_text, replacements),

            # Text after the entity
            %{"type" => "text", "text" => after_text}
          ]

        # Case 2: Entity at end of paragraph
        after_text == "" ->
          [
            # Text before the entity
            %{"type" => "text", "text" => before_text},

            # The entity with marks
            format_entity_for_tiptap(entity_text, replacements)

            # Add an invisible space after the entity for proper rendering
            # %{"type" => "text", "text" => "\u200B"}
          ]

        # Case 3: Entity in middle of paragraph (normal case)
        true ->
          [
            # Text before the entity
            %{"type" => "text", "text" => before_text},

            # The entity with marks
            format_entity_for_tiptap(entity_text, replacements),

            # Text after the entity
            %{"type" => "text", "text" => after_text}
          ]
      end

    # Create a document with the paragraph content
    %{
      "type" => "doc",
      "content" => [
        %{
          "type" => "paragraph",
          "content" => paragraph_content
        }
      ]
    }
  end

  # Helper function to split text into paragraphs with proper hardBreaks and whitespace
  def split_text_into_paragraphs(text) do
    text
    |> String.split("\n")
    |> Enum.with_index()
    |> Enum.flat_map(fn {segment, idx} ->
      nodes = [%{"type" => "text", "text" => segment}]

      # If not the last segment, add a hardBreak followed by a whitespace
      if idx < length(String.split(text, "\n")) - 1 do
        # ++
        nodes
        # [
        #   # %{"type" => "hardBreak"},
        #   # Add a whitespace text node after hardBreak for better rendering
        #   %{"type" => "text", "text" => "\n "}
        # ]
      else
        nodes
      end
    end)
  end

  @doc """
  Gets all entities from TipTap formatted content.
  Returns a list of entity maps with their positions and attributes.
  """
  def get_entities_from_formatted_content(formatted_content) when is_map(formatted_content) do
    try do
      extract_entities(formatted_content)
    rescue
      e ->
        Logger.error("Error extracting entities: #{inspect(e)}")
        []
    end
  end

  def get_entities_from_formatted_content(_), do: []

  # Helper to extract entities from content
  defp extract_entities(%{"content" => content}) when is_list(content) do
    content
    |> Enum.with_index()
    |> Enum.flat_map(fn {block, block_idx} ->
      case block do
        %{"content" => block_content} when is_list(block_content) ->
          extract_entities_from_block(block_content, block_idx)

        _ ->
          []
      end
    end)
  end

  defp extract_entities(%{}), do: []

  # Extract entities from a content block
  defp extract_entities_from_block(content, block_idx) do
    content
    |> Enum.with_index()
    |> Enum.flat_map(fn {node, node_idx} ->
      case node do
        %{"marks" => marks, "text" => text} when is_list(marks) ->
          entity_mark = Enum.find(marks, &(&1["type"] == "coloredEntity"))

          if entity_mark do
            attrs = Map.get(entity_mark, "attrs", %{})

            [
              %{
                position: {block_idx, node_idx},
                text: text,
                attrs: attrs
              }
            ]
          else
            []
          end

        _ ->
          []
      end
    end)
  end
end
