%% @author sergey
%% @doc @todo Add description to chat_online_test.


-module(chat_online_test).
-include_lib("eunit/include/eunit.hrl").

%-compile(export_all).
%% ====================================================================
%% API functions
%% ====================================================================
%-export([]).

reg_unreg_test() ->
	{ok,P} = start(),
	chat_online:register(P, aa, a),
	chat_online:register(P, bb, b),
	chat_online:register(P, bbb, b),
	chat_online:register(P, aaa, a),
	
	Pa = chat_online:get_pids_by_name(P,a),
	?assertEqual(length(Pa),2),
	?assertEqual(Pa -- [aa,aaa],[]),
	
	chat_online:unregister(P, ccc), % nonexisting
	chat_online:unregister(P, aaa),
	
	{U,_} = chat_online:get_user_list(P),
	?assertEqual(U,[a,b]), % usernames are uniq'ed and sorted
	chat_online:unregister(P, aa),
	{Ub,_} = chat_online:get_user_list(P),
	?assertEqual(Ub,[b]),
	P_no_a = chat_online:get_pids_by_name(P,a),
	?assertEqual(P_no_a,[]),
	stop(P).

start_stop_test() ->
	{ok,P} = start(),
	stop(P).

%% ====================================================================
%% Internal functions
%% ====================================================================

start() -> 
	gen_server:start_link(chat_online, [], []).

stop(P) ->
	gen_server:stop(P).
