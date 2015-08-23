{application, ars,
	      [{vsn, "1.0.0"},
	      {modules, [ars, ars_app, ars_supersup, ars_sup, ars_server, ars_task_sup]},
	      {applications,[stdlib,kernel]},
	      {registered, [ars]},
	      {module, {ars, []}}
]}.
