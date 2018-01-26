defmodule ExCernan.Avro.Schema do
  @moduledoc """
    Constructors for avro types.

    The following example illustrates usage of this module
    to define and construct a User record:

      ```
        schema = ExCernan.Avro.Schema.record(
          "User",
          [ExCernan.Avro.Schema.field("name", :string),
           ExCernan.Avro.Schema.field("favorite_number", :int),
           ExCernan.Avro.Schema.field("favorite_color", :string)],
          [{:namespace, "example.avro"}])

        user1 = [{"name", "Jane Doe"}, {"favorite_number", 10}, {"favorite_color", "maroon"}]
      ```
  """

  defdelegate record(name, fields), to: :cerlnan_avro_schema

  defdelegate record(name, fields, options), to: :cerlnan_avro_schema

  defdelegate field(name, type_or_name), to: :cerlnan_avro_schema

  defdelegate field(name, type_or_name, options), to: :cerlnan_avro_schema

  defdelegate array(type), to: :cerlnan_avro_schema

  defdelegate array(type, props), to: :cerlnan_avro_schema

  defdelegate map(type), to: :cerlnan_avro_schema

  defdelegate map(type, props), to: :cerlnan_avro_schema

  defdelegate union(types_or_names), to: :cerlnan_avro_schema

  defdelegate enum(name, symbols), to: :cerlnan_avro_schema
end
