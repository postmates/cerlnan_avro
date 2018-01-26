defmodule Cerlnan.Avro.Mixfile do
  use Mix.Project

  def project do
    base_erlc_opts = [:debug_info, {:parse_transform, :lager_transform}]
    erlc_opts =
      case Mix.env do
          :test ->
            [{:d, :TEST} | base_erlc_opts]
          _ ->
            base_erlc_opts
      end

    [app: :cerlnan_avro,
     version: "0.0.1",
     language: :erlang,
     env: [],
     erlc_options: erlc_opts,
     deps: deps(),

     build_path: "../../_build",
     deps_path: "../../deps",
     lockfile: "../../mix.lock",
   ]
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
     {:erlavro, "~> 2.4.0"},
    ]
  end

end
