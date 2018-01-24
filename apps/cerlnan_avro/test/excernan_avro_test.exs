ExUnit.start()


defmodule ExCernanAvroTest do

  use ExUnit.Case, async: true

  test "Check Pool-backed Publish Blob for Basic Errors" do
    ExCernan.Avro.publish_blob(<<>>)
  end



end
