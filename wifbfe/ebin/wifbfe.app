{application, wifbfe, [
    {description, "A simple Facebook application that displays your Wii code in your profile."},
    {vsn, "0.1"},
    {modules, [
        wifbfe,
        wifbfe_handler,
        wifbfe_yaws,
        wifbfe_hometmpl
    ]},
    {registered, [wifbfe, wifbfe_handler, wifbfe_yaws]},
    {applications, [kernel, stdlib, sasl, yaws, inets]},
    {mod, {wifbfe, []}},
    {start_phases, [
      {mnesia, []},
      {facebook, []}
    ]}
]}.