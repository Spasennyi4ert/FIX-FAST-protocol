FIX-FAST-protocol
=================
FIX-FAST-protocol подключения к секции срочного рынка moex.com. Образец получаемых данных.



1> application:start(fast).

ok

2> fast:start_task(server, 2, {fast_server, start_link, []}).

{ok,<0.69.0>} 

3> fast:run(server,[feed_a]). 

{ok,<0.73.0>} 

4> 91290.0 
 
application:stop(fast).

ok 

5> 

=INFO REPORT==== 4-Mar-2015::14:39:46 === 
    application: fast 
    exited: stopped 
    type: temporary
