{application, fix,
	      [{vsn, "1.0.0"},
	      {modules, [fix, fix_app, fix_supersup, fix_group,
	        fix_parser, fix_sup, fix_exec_conn, fix_splitter]},
        {applications,[stdlib,kernel]},
	      {registered, [fix_supersup]},
	      {mod, {fix_app, []}}
]}.
