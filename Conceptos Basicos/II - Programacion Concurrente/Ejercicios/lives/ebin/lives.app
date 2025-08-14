{application, lives,
[
  {description, "Aplicación de gestión de vidas"},
  {vsn, "1.0"},
  {modules, [lives, lives_server, lives_app]},
  {registered, [vidas_erl]},
  {mod, {lives_app, []}},
  {applications, [kernel, stdlib]},
  {env, [{initial_lives, 15}]}
]}.