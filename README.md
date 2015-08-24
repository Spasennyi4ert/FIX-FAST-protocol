Робот для торговли на MOEX в секции FORTS, с подключением по FAST и FIX протоколам.
Пробный запуск следующий:
~/ars/apps/fast-1.0.0$ erl -pa ebin/
Erlang/OTP 18 [erts-7.0] [source] [64-bit] [smp:8:8] [async-threads:10] [kernel-poll:false]

Eshell V7.0  (abort with ^G)
1> application:start(fast).
ok
2> SecID = 77205831.
77205831
3> fast:start_server(feed_a, SecID).
{ok,<0.42.0>}
4> List : 1135 [64,44,1135,20150824044824300,<<>>]
List : 1136 [64,44,1136,20150824044854682,<<>>]
List : 1137 [64,44,1137,20150824044925312,<<>>]
List : 1138 [64,44,1138,20150824044955694,<<>>]
List : 1139 [64,44,1139,20150824045026324,<<>>]
fast:stop_server(feed_a). 
ok 
5> application:stop(fast). 
ok 
6> 
=INFO REPORT==== 23-Aug-2015::14:54:01 === 
    application: fast 
    exited: stopped 
    type: temporary 
q(). 
ok 
*SecID = 77205831 - код фъючерса RIU5, при "правильном" декодировании равен 77205830, чтобы привести к нормальному виду добавлена 1.
*Для подключения требуется установить соединение по specific source multicast.
