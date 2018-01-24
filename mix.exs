defmodule CerlnanUmbrella.Mixfile do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      preferred_cli_env: [eunit: :test],
      eunit: [
        verbose: true,
        cover: true,
      ],
      deps: deps()
    ]
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
    [{:dialyxir, "~> 0.5.1", only: [:dev, :test], runtime: false},
     {:mix_eunit, "~> 0.3.0", only: [:dev, :test], runtime: false}]
  end

end
