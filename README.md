FIX-FAST-protocol
=================
Протокол подключения к Московской бирже на Erlang'е.
Адреса групп транслируемых данных для тестов ftp://ftp.moex.com/pub/FAST/Spectra/test_templates/internet_test/configuration.xml.
Подключение использует IGMP v.3 поэтому требуется адрес источника 172.27.129.77.
Посмотреть результат подключения написанный на Erlang'е можно следующим образом:

1> A = ["172.27.129.77","239.192.7.27","24027"].
["172.27.129.77","239.192.7.27","24027"].
2> fast_sample:start(A).
<<"Reset">> [] 
<<"Heartbeat">> [{<<"ApplVerID">>,<<"8">>}, 
                 {<<"MessageType">>,<<"0">>}, 
                 {<<"SenderCompID">>,<<"RTS">>}, 
                 {<<"MsgSeqNum">>,896}, 
                 {<<"SendingTime">>,20140731055040077}] 
<<"Reset">> [] 
<<"Heartbeat">> [{<<"ApplVerID">>,<<"8">>}, 
                 {<<"MessageType">>,<<"0">>}, 
                 {<<"SenderCompID">>,<<"RTS">>}, 
                 {<<"MsgSeqNum">>,897}, 
                 {<<"SendingTime">>,20140731055110077}] 
<<"Reset">> [] 
<<"Heartbeat">> [{<<"ApplVerID">>,<<"8">>}, 
                 {<<"MessageType">>,<<"0">>}, 
                 {<<"SenderCompID">>,<<"RTS">>}, 
                 {<<"MsgSeqNum">>,898}, 
                 {<<"SendingTime">>,20140731055140093}] 
                
Вариант на OTP пока не рабочий
s5@s5:~/fast_v_1.6$ erl -pa ./ebin 
Erlang/OTP 17 [erts-6.2] [source] [64-bit] [smp:8:8] [async-threads:10] [kernel-poll:false] 

Eshell V6.2  (abort with ^G) 
1> application:start(fast). 
ok 
2> fast_server:start_link(). 
{ok,<0.41.0>} 
<<"Reset">> [] 
<<"DefaultIncrementalRefreshMessage">> [{<<"ApplVerID">>,<<"8">>}, 
                                        {<<"MessageType">>,<<"X">>}, 
                                        {<<"SenderCompID">>,<<"RTS">>}, 
                                        {<<"MsgSeqNum">>,108569}, 
                                        {<<"SendingTime">>,40282026182627766}, 
                                        {<<"MDEntries">>, 
                                         [[{<<"MDUpdateAction">>,0}, 
                                           {<<"MDEntryType">>,<<"w">>}, 
                                           {<<"SecurityID">>,134478988}, 
                                           {<<"SecurityIDSource">>,8}, 
                                           {<<"ExchangeTradingSessionID">>, 
                                            4671}, 
                                           {<<"RptSeq">>,15986}, 
                                           {<<"MDEntrySize">>,801}, 
                                           {<<"MDEntryTime">>,182615522}, 
                                           {<<"NumberOfOrders">>,66}]]}] 
** exception error: no function clause matching 
                    fast_server:terminate({{badmatch,{error,not_enough_data}}, 
                                           [{fast_server,handle_info,2, 
                                             [{file,"src/fast_server.erl"},{line,62}]}, 
                                            {gen_server,handle_msg,5, 
                                             [{file,"gen_server.erl"},{line,599}]}, 
                                            {proc_lib,init_p_do_apply,3, 
                                             [{file,"proc_lib.erl"},{line,237}]}]}, 
                                          {context,undef,#Fun<fast_server.0.26748577>,[],undef, 
                                           <<"any">>,undefined, 
                                           {27, 
                                            {<<"DefaultIncrementalRefreshMessage201030081916789_template_">>, 
                                             [], 
                                             {"77",[], 
                                              {"44",[], 
                                               {"120",[], 
                                                {global,[],nil,{type,...}}, 
                                                {"303",[],nil,nil}}, 
                                               {"66",[],{"49",[],nil,nil},nil}}, 
                                              {"80",[], 
