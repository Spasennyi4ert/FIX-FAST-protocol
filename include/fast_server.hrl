-record(server, {
	buffer, 
	socket,
	pid,
	pid_fsm,
	name,
	seq = 1,
	prev
}).
