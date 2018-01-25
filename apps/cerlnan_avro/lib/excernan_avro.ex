defmodule ExCernan.Avro do
  @moduledoc """
    Client / connection pool for Cernan's Avro source.

    This module is a pass-through to the cerlnan_avro application.
    On startup, a number of poolbay backed connection pools are created
    based on Application env.  If no pools are configured by the user, a
    default pool equivalent to the following config is created:

        ```
          config :cerlnan_avro,
            pools: [
              {:cerlnan_avro,
               %{}
              }
            ]
        ```

    Pool configs are of the form {pool-id :, pool-args} when pool-id: atom,
    pool-args: map.  pool-args has the following form:

        %{
          :backend => Backend socket semantics.  Default = :cerlnan_avro_socket_v1.
          :backend_args => Map of backend specific arguments.  Default = %{}.
          :pool_size => Size of the connection pool to Cernan.  Default = 10.
          :pool_overflow => Number of overflow workers allowed.  Default = 20.
          ...
        }
  """

  @doc """
    Publishes the given blob on the default pool.

    Note - This function will result in error when users specify their own
    named pools.

    Returns `:ok`.
  """
  defdelegate publish_blob(blob), to: :cerlnan_avro

  @doc """
    Publishes the given binary `blob` via connection the given `pool`..

    Returns `:ok`.
  """
  defdelegate publish_blob(pool, blob), to: :cerlnan_avro

  @doc """
    Publishes the given `blob` on `pool` with the given `args`.

    Returns `:ok`.
  """
  defdelegate publish_blob(socket, blob, args), to: :cerlnan_avro
end
