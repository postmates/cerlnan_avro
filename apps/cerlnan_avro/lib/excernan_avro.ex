defmodule ExCernan.Avro do
  @moduledoc """
    Client / connection pool for Cernan's Avro source.

    This module is a pass-through to the cerlnan_avro application.
  """

  @doc """
    Publishes the given record (or batch of records)  of the given `type` and
    `schema` on the default pool.

    Note - This function will error when users specify their own pools.

    Returns `:ok`.
  """
  defdelegate publish(type, schema, record_or_records), to: :cerlnan_avro

  @doc """
    Publishes the given record (or batch of records) of the given `type` and
    `schema` on the given `pool`.

    Returns `:ok`.
  """
  defdelegate publish(pool, type, schema, record_or_records), to: :cerlnan_avro

  @doc """
    Publishes the given record (or batch of records) of the given `type` and
    `schema` on the given `pool`.

    Backend specific `args` may be given to control how the blob is published.

    Returns `:ok`.
  """
  defdelegate publish(pool, type, schema, record_or_records, args), to: :cerlnan_avro

  @doc """
    Publishes the given blob on the default pool.

    Note - This function will result in error when users specify their own
    named pools.

    Returns `:ok`.
  """
  defdelegate publish_blob(blob), to: :cerlnan_avro

  @doc """
    Publishes the given binary `blob` via connection the given `pool`.

    Returns `:ok`.
  """
  defdelegate publish_blob(pool, blob), to: :cerlnan_avro

  @doc """
    Publishes the given `blob` on `pool` with the given `args`.

    Returns `:ok`.
  """
  defdelegate publish_blob(socket, blob, args), to: :cerlnan_avro
end
