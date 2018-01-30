defmodule CerlnanUmbrella.Mixfile do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      preferred_cli_env: [eunit: :test],
      dialyzer: [
        plt_add_apps: [:erlavro, :poolboy],
        flags: [:unmatched_returns,:error_handling,:race_conditions, :no_opaque],
        paths: ["_build/dev/lib/cerlnan_avro/ebin"]
      ],
      deps: deps()
    ]
  end

  defp deps do
    [{:dialyxir, "~> 0.5.1", only: [:dev, :test], runtime: false},
     {:mix_eunit, "~> 0.3.0", only: [:dev, :test], runtime: false}]
  end

end
