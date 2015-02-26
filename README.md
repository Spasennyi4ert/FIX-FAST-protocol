FIX-FAST-protocol
=================
FIX-FAST-protocol подключения к секции срочного рынка moex.com. Образец получаемых данных.



1> application:start(fast).

ok

2> fast_in:start_link().

{ok,<0.41.0>}

3> fast_server:start_link().

{ok,<0.43.0>}

103090.0 
 
103060.0 
 
103120.0 
 
1.032e5 
 
103190.0 
 
103190.0 
 
103120.0 

это и есть цена актива, поступающая от сделок
