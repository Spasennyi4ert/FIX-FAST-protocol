FIX-FAST-protocol
=================
FIX-FAST-protocol подключения к moex.com. Образец получаемых данных.

Erlang/OTP 17 [erts-6.2] [source] [64-bit] [smp:8:8] [async-threads:10] [kernel-poll:false]

Eshell V6.2  (abort with ^G)
1> application:start(fast).
ok
2> A = ["172.27.129.77","239.192.7.27","24027"].
["172.27.129.77","239.192.7.27","24027"]
3> fast_sample:start(A).
<<"Reset">> []
<<"DefaultIncrementalRefreshMessage">> [{<<"ApplVerID">>,<<"8">>},
                                        {<<"MessageType">>,<<"X">>},
                                        {<<"SenderCompID">>,<<"RTS">>},
                                        {<<"MsgSeqNum">>,12573},
                                        {<<"SendingTime">>,20141021063626921},
                                        {<<"MDEntries">>,
                                         [[{<<"MDUpdateAction">>,0},
                                           {<<"MDEntryType">>,<<"w">>},
                                           {<<"SecurityID">>,67242566},
                                           {<<"SecurityIDSource">>,8},
                                           {<<"ExchangeTradingSessionID">>,
                                            4697},
                                           {<<"RptSeq">>,4013},
                                           {<<"MDEntrySize">>,289967},
                                           {<<"MDEntryTime">>,63626918},
                                           {<<"NumberOfOrders">>,185}]]}]
<<"Reset">> []
<<"DefaultIncrementalRefreshMessage">> [{<<"ApplVerID">>,<<"8">>},
                                        {<<"MessageType">>,<<"X">>},
                                        {<<"SenderCompID">>,<<"RTS">>},
                                        {<<"MsgSeqNum">>,12574},
                                        {<<"SendingTime">>,20141021063628187},
                                        {<<"MDEntries">>,
                                         [[{<<"MDUpdateAction">>,0},
                                           {<<"MDEntryType">>,<<"v">>},
                                           {<<"SecurityID">>,66490694},
                                           {<<"SecurityIDSource">>,8},
                                           {<<"ExchangeTradingSessionID">>,
                                            4697},
                                           {<<"RptSeq">>,3711},
                                           {<<"MDEntrySize">>,8951},
                                           {<<"MDEntryTime">>,63628179},
                                           {<<"NumberOfOrders">>,232}]]}]
<<"Reset">> []
<<"DefaultIncrementalRefreshMessage">> [{<<"ApplVerID">>,<<"8">>},
                                        {<<"MessageType">>,<<"X">>},
                                        {<<"SenderCompID">>,<<"RTS">>},
                                        {<<"MsgSeqNum">>,12575},
                                        {<<"SendingTime">>,20141021063628202},
                                        {<<"MDEntries">>,
                                         [[{<<"MDUpdateAction">>,0},
                                           {<<"MDEntryType">>,<<"v">>},
                                           {<<"SecurityID">>,66490694},
                                           {<<"SecurityIDSource">>,8},
                                           {<<"ExchangeTradingSessionID">>,
                                            4697},
                                           {<<"RptSeq">>,3712},
                                           {<<"MDEntrySize">>,8949},
                                           {<<"MDEntryTime">>,63628190},
                                           {<<"NumberOfOrders">>,230}]]}]
