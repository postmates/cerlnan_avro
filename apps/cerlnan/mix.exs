defmodule Cerlnan.Mixfile do
  use Mix.Project

  def project do
    [app: :cerlnan,
     version: "0.0.1",
     language: :erlang,
     env: [{:clients, []}],
     deps: deps(),

     build_path: "../../_build",
     deps_path: "../../deps",
     lockfile: "../../mix.lock"]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [applications: [], mod: {:cerlnan_app, []}]
  end

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:cerlnan_avro, in_umbrella: true}
    ]
  end

end
