-module(ars_handler).
-behavior(gen_server).

-export([start_link/1, get_data/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

% Файл предназначен для генерирования торговых сигналов на основе данных поступающих из
%fast_dispatcher.erl. Исходящие сигналы представлены 4 типами - sar, подготовка к фиксированию прибыли, 
%фиксирование прибыли и фиксирование убытков.
