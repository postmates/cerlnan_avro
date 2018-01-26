ExUnit.start()

defmodule ExCernanAvroTest do

  use ExUnit.Case, async: true

  test "Check Pool-backed Publish Blob for Basic Errors" do
    ExCernan.Avro.publish_blob(<<>>)
  end

  test "Check Pool-backed Publish for Basic Errors" do
      schema = ExCernan.Avro.Schema.record(
        "User",
        [ExCernan.Avro.Schema.field("name", :string),
         ExCernan.Avro.Schema.field("favorite_number", :int),
         ExCernan.Avro.Schema.field("favorite_color", :string)],
       [{:namespace, "example.avro"}])

      user1 = [{"name", "Foo Bar"}, {"favorite_number", 10}, {"favorite_color", "maroon"}]
      user2 = [{"name", "Alice Bob"}, {"favorite_number", 32}, {"favorite_color", "greenish-gold"}]

      ExCernan.Avro.publish("example.avro.User", schema, [user1, user2])
  end
end
