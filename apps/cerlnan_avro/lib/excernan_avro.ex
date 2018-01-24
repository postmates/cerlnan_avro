defmodule ExCernan.Avro do
  @moduledoc """
    Client / connection pool for Cernan's Avro source.

    This module is a pass-through to the cerlnan_avro application.  On
    startup, a poolboy backed connection pool is created.  This pool and
    is backing socket semantics are configurable via Application.
    The following is a listing of the options available to the user:
        * backend - Backend socket semantics.  Default = :cerlnan_avro_socket_v1.
        * pool_size - Size of the connection pool to Cernan.  Default = 10.
        * pool_overflow - Number of overflow workers allowed.  Default = 20.

    For example, to configure the pool size to 50 one would execute:
        Application.put_env(:cerlnan_avro, pool_size, 50)

  """

  @doc """
    Creates an :cerlnan_avro socket with the default backend.
  """
  defdelegate connect(backend_args), to: :cerlnan_avro

  @doc """
    Creates an :cerlnan_avro socket with the specified `backend` from the given `backend_args`.
  """
  defdelegate connect(backend, backend_args), to: :cerlnan_avro

  @doc """
    Publishes the given binary blob via connection pool.

    Returns `:ok`.
  """
  defdelegate publish_blob(blob), to: :cerlnan_avro

  @doc """
    Publishes either directly via an avro socket or via connection pool.
  """
  defdelegate publish_blob(socket_or_blob, blob_or_args), to: :cerlnan_avro

  @doc """
    Publishes the given `blob` on `socket` with the given `args`.
  """
  defdelegate publish_blob(socket, blob, args), to: :cerlnan_avro
end
