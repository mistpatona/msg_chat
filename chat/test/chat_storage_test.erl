%% @author sergey
%% @doc @todo Add description to chat_storage_test.


-module(chat_storage_test).
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).







sanity_test() -> 2 = 1+1.

start_stop_test() ->
	{ok,P} = start(),
	stop(P).

store_some_msgs_test() ->
	{ok,P} = start(),
	chat_storage:add_message(P, a, b, "a2b"),
	chat_storage:add_message(P, b, a, "b2a"),
	chat_storage:add_message(P, a, b, "a2b-2"),
	chat_storage:add_message(P, b, a, "b2a-2"),
	L = chat_storage:get_client_list(P),
	?assertEqual([],L -- [a,b]),
	H = chat_storage:get_history(P, a, b),
	?assertEqual(length(H),4),
	Hc = chat_storage:get_history(P, a, c),
	?assertEqual(Hc,[]),
	stop(P).

%%--------------------------------------------------------

start() -> 
	gen_server:start_link(chat_storage, [], []).

stop(P) ->
	gen_server:stop(P).




%% ====================================================================
%% Internal functions
%% ====================================================================


