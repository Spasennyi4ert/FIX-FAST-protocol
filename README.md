Робот для торговли на MOEX в секции FORTS, с подключением по FAST и FIX протоколам.
Пробный запуск следующий:
~/ars/apps$ erl -env ERL_LIBS "."

Erlang/OTP 18 [erts-7.0] [source] [64-bit] [smp:8:8] [async-threads:10] [kernel-poll:false]

Eshell V7.0  (abort with ^G)

1> application:load(fast).

ok

2> application:start(ars), application:start(fix).

ok

3> ars:start_service(fsm, {ars_fsm, start_link, []}).

{ok,<0.50.0>}

4> ars:start_service(mng, {ars_mng, start_link, []}).

{ok,<0.54.0>}

5> ars:run_task(fsm, []), ars:run_task(mng, [book]).

MGR(<0.64.0>) -> FSM(<0.58.0>) getting TableId: 20498

{ok,<0.64.0>}

6> Item = 'F.RIZ5'. 

'F.RIZ5'

5> 7> fix:conn(fix_read, Item), fix:table(order). 

MGR(<0.71.0>) -> FEC(<0.68.0>) getting TableId:24595 

{ok,<0.70.0>} 

*SecID = 77205831 - код фъючерса RIU5, при "правильном" декодировании равен 77205830, чтобы привести к нормальному виду добавлена 1.
*Для подключения требуется установить соединение по specific source multicast.
