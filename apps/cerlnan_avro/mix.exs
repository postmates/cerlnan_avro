defmodule Cerlnan.Avro.Mixfile do
  use Mix.Project

  def project do
    [app: :cerlnan_avro,
     version: "0.0.1",
     elixir: "~> 1.0",
     language: :erlang,
     deps: deps(),

     build_path: "../../_build",
     deps_path: "../../deps",
     lockfile: "../../mix.lock"]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [applications: [:lager], mod: {:cerlnan_avro_app, []}]
  end

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
     {:lager, "~> 3.2.4"},
     {:poolboy, "~> 1.5.1"},
     {:uuid, "~> 1.1"}
    ]
  end

end
