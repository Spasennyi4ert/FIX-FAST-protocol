Робот для торговли на MOEX в секции FORTS, с подключением по FAST и FIX протоколам.
Пробный запуск следующий -
запускаем терминал source:

s5@s5:~/source/source-1.0.0$ erl -pa ebin/ -sname feeda 
Erlang/OTP 18 [erts-7.2] [source] [64-bit] [smp:8:8] [async-threads:10] [kernel-poll:false] 

Eshell V7.2  (abort with ^G) 
(feeda@s5)1> SecID = 91567431.

91567431

Запускаем терминал server:
s5@s5:~/ars/apps$ erl -env ERL_LIBS "." -sname server 
Erlang/OTP 18 [erts-7.2] [source] [64-bit] [smp:8:8] [async-threads:10] [kernel-poll:false] 

Eshell V7.2  (abort with ^G) 

(server@s5)1> net_kernel:connect_node(feeda@s5). 

true 

(server@s5)2> Item = 'F.RIM6'. 

'F.RIH6' 

(server@s5)3> SecID = 91567431. 

91567431

(server@s5)4> application:load(ars).

ok 

(server@s5)5> application:start(ars), application:start(fast), application:start(fix). 

ok 

(server@s5)6> ars:start_service(fsm, {ars_fsm, start_link, []}). 

{ok,<0.64.0>} 

(server@s5)7> ars:start_service(mng, {ars_mng, start_link, []}). 

{ok,<0.68.0>} 

(server@s5)8> ars:run_task(fsm, []), ars:run_task(mng, [book]). 

MGR(<0.78.0>) -> FSM(<0.72.0>) getting TableId: 32790 

{ok,<0.78.0>} 

(server@s5)9> fix:conn(fix_read, Item), fix:table(order). 

fix_exec_conn:63 <0.81.0> 

fix_ets_mng:26 <0.81.0> 

MGR(<0.84.0>) -> FEC(<0.81.0>) getting TableId:36887 

{ok,<0.83.0>} 

(server@s5)10> fix_exec_conn:173 1 

fast:start_dispatcher(dispatcher, SecID). 

{ok,<0.88.0>} 

(server@s5)11> fix_exec_conn:173 2 

fix_exec_conn:173 3 

Далее в терминале source:

(feeda@s5)2> application:start(source). 

ok 

(feeda@s5)3> source:start_server(feed_a, SecID). 

{ok,<0.50.0>}

После чего в терминале появятся цены:

Price: 73570.0 

Price: 73570.0 

Price: 73580.0 

...

Отключение начинаем с терминала server:

(server@s5)12> ars:stop_task(fsm).

ars_fsm:940 {exp_price,72590.0} 

fix_exec_conn:173 354 

{ok,<0.72.0>} 

ars_msg_sender:12 {new_order_single,sell,3} 

(server@s5)15> fix_exec_conn:173 355 

fix_exec_conn:252 {execution_report,pending_cancel,0,6,sell,<0.72.0>} 

Status: pending_cancel 

fix_exec_conn:252 {execution_report,canceled,0,6,sell,<0.72.0>} 

fix_exec_conn:252 {execution_report,new,0,3,sell,<0.72.0>} 

Status: new 

fix_exec_conn:252 {execution_report,partial,1,3,sell,<0.72.0>} 

fix_exec_conn:252 {execution_report,filled,3,3,sell,<0.72.0>} 

Price: 73570.0 

fix_exec_conn:173 356

Price: 73580.0 

application:stop(fix), application:stop(fast), application:stop(ars). 

fix_exec_conn:63 <0.103.0> 

=INFO REPORT==== 20-Feb-2016::15:21:25 === 

    application: fix 
    
    exited: stopped 
    
    type: temporary 
    

=INFO REPORT==== 20-Feb-2016::15:21:25 === 

    application: fast 
    
    exited: stopped 
    
    type: temporary 
    
received down msg 

=INFO REPORT==== 20-Feb-2016::15:21:25 === 

    application: ars 
    
    exited: stopped 
    
    type: temporary 
    
ok 

(server@s5)16> q(). 

ok 

После чего отключаем терминал source:

(feeda@s5)4> application:stop(source).

ok 

(feeda@s5)5> 

=INFO REPORT==== 20-Feb-2016::15:16:23 === 

    application: source 
    
    exited: stopped 
    
    type: temporary 
    
q(). 

ok 
 
