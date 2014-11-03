FIX-FAST-protocol
=================
FIX-FAST-protocol подключения к секции срочного рынка moex.com. Образец получаемых данных.

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
                                           
Erlang/OTP 17 [erts-6.2] [source] [64-bit] [smp:8:8] [async-threads:10] [kernel-poll:false]

Eshell V6.2  (abort with ^G)

1> application:start(fast).

ok

2> fast_server:start_link().

{ok,<0.41.0>}

  [[{<<"MDUpdateAction">>,0},
  
   {<<"MDEntryType">>,<<"v">>},
   
   {<<"SecurityID">>,66489926},
   
   {<<"SecurityIDSource">>,8},
   
   {<<"ExchangeTradingSessionID">>,4715},
   
   {<<"RptSeq">>,31659},
   
   {<<"MDEntrySize">>,500},
   
   {<<"MDEntryTime">>,111157184},
   
   {<<"NumberOfOrders">>,72}],
   
  [{<<"MDUpdateAction">>,0},
  
   {<<"MDEntryType">>,<<"v">>},
   
   {<<"SecurityID">>,67239494},
   
   {<<"SecurityIDSource">>,8},
   
   {<<"ExchangeTradingSessionID">>,4715},
   
   {<<"RptSeq">>,46545},
   
   {<<"MDEntrySize">>,2229},
   
   {<<"MDEntryTime">>,111157184},
   
   {<<"NumberOfOrders">>,136}]]
 
 
 [[{<<"MDUpdateAction">>,0},
 
   {<<"MDEntryType">>,<<"B">>},
   
   {<<"SecurityID">>,66489926},
   
   {<<"SecurityIDSource">>,8},
   
   {<<"ExchangeTradingSessionID">>,4715},
   
   {<<"RptSeq">>,31660},
   
   {<<"MDEntryPx">>,{11128785055,-1}},
   
   {<<"MDEntrySize">>,12302},
   
   {<<"MDEntryTime">>,111157184}],
   
  [{<<"MDUpdateAction">>,0},
  
   {<<"MDEntryType">>,<<"C">>},
   
   {<<"SecurityID">>,66489926},
   
   {<<"SecurityIDSource">>,8},
   
   {<<"ExchangeTradingSessionID">>,4715},
   
   {<<"RptSeq">>,31661},
   
   {<<"MDEntrySize">>,36286},
   
   {<<"MDEntryTime">>,111157184}],
   
  [{<<"MDUpdateAction">>,0},
  
   {<<"MDEntryType">>,<<"w">>},
   
   {<<"SecurityID">>,67239494},
   
   {<<"SecurityIDSource">>,8},
   
   {<<"ExchangeTradingSessionID">>,4715},
   
   {<<"RptSeq">>,46546},
   
   {<<"MDEntrySize">>,1909},
   
   {<<"MDEntryTime">>,111157186},
   
   {<<"NumberOfOrders">>,71}],
   
  [{<<"MDUpdateAction">>,0},
  
   {<<"MDEntryType">>,<<"w">>},
   
   {<<"SecurityID">>,67241030},
   
   {<<"SecurityIDSource">>,8},
   
   {<<"ExchangeTradingSessionID">>,4715},
   
   {<<"RptSeq">>,21979},
   
   {<<"MDEntrySize">>,153},
   
   {<<"MDEntryTime">>,111157191},
   
   {<<"NumberOfOrders">>,65}]]
 
