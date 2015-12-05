{application, fix,
	      [{vsn, "1.0.0"},
	      {modules, [fix, fix_app, fix_supersup, fix_group, fix_ets_mng, fix_ets_sup,
	      		fix_parser, fix_exec_sup, fix_exec_conn, fix_splitter]},
              {applications,[stdlib,kernel,ars]},
	      {registered, [fix]},
	      {mod, {fix_app, []}},
	      {env, [
	      	    {fix_read, [
		      {host, "91.208.232.244"},
		      {port, 6001},
		      {password, "12345"},
		      {target, "FG"},
		      {sender, "tgFZcmx200A80"},
		      {heartbeat, 30},
		      {account, 'A80'}
		      ]}
	      ]}
]}.
