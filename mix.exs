defmodule CerlnanUmbrella.Mixfile do
  use Mix.Project

  def project do
    [apps_path: "apps",
     deps: deps()]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  #def application do
    #3  [applications: [], mod: {:cerlnan_app, []}]
  #nd

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [{:dialyxir, "~> 0.5.1", only: [:dev], runtime: false}]
  end

end
