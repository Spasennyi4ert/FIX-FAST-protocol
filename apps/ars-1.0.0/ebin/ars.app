{application, ars,
	      [{vsn, "1.0.0"},
	      {modules, [ars, ars_app, ars_fsm, ars_handler, ars_msg_sender, ars_mng,
	      ars_supersup, ars_sup, ars_server, ars_task_sup]},
	      {applications,[stdlib,kernel]},
	      {registered, [ars]},
	      {module, {ars, []}}
]}.
