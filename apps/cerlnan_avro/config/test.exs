use Mix.Config

config :cerlnan_avro,
  pools: [
    {:cerlnan_avro,
     %{:backend => :cerlnan_avro_socket_dummy}
    }
  ]
