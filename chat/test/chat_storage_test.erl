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


%%--------------------------------------------------------

start() -> 
	gen_server:start_link(chat_storage, [], []).

stop(P) ->
	gen_server:stop(P).




%% ====================================================================
%% Internal functions
%% ====================================================================


