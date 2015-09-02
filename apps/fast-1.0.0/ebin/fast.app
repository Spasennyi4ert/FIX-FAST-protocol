{application,fast,
             [{registered,[]},
              {vsn,"1.0.0"},
              {modules,[fast,fast_app,fast_app_SUITE,fast_server,fast_server_sup,
                        fast_supersup, fast_dispatcher, fast_dispatcher_sup]},
              {applications,[stdlib,kernel,ars]},
              {mod,{fast_app,[]}},
	      {registered, [fast]},
              {env,[
		{feed_a,[{source, "172.27.129.90"},{port, "34003"},{group, "239.192.70.3"}]},
		{feed_b,[{source, "172.27.129.90"},{port, "35003"},{group, "239.192.175.3"}]},
		{feed_c,[{source, "172.27.129.90"},{port, "34004"},{group, "239.192.70.4"}]},
		{feed_d,[{source, "172.27.129.90"},{port, "35004"},{group, "239.192.175.4"}]}
	      ]}
]}.
