%% @author sergey
%% @doc @todo Add description to chat_cli.


-module(chat_cli).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/2]).

-export([notify/4,send/3,get_users/1,get_history/2,logout/1]).

-export([get_login/1,get_client/1]).

start_link(Conn,Login) ->
	gen_server:start_link(?MODULE, [Conn,Login], []).

logout(Pid) ->
	gen_server:cast(Pid,logout).

notify(Pid,From,_To, Body) ->
	%io:format("chat_cli:notify ~p ",[Pid]),
	Conn = get_client(Pid),
	%io:format("conn: ~p~n",[Conn]),
	chat_client:notify_message(Conn,From,Body).

send(Pid,To,Body) ->
	gen_server:cast(Pid,{send,To,Body}).

%lists of online and offline usernames ({online,all})

get_users(Pid) ->
	gen_server:call(Pid,get_users).
	%chat_online:get_user_list(chat_online).

%% 	Onlines = chat_online:online_users(chat_online),
%% 	All 	= chat_online:offline_users(chat_online),
%% 	{Onlines,All}.

get_history(Pid,Friend) ->
	gen_server:call(Pid,{get_history,Friend}).

get_login(Pid) ->
	gen_server:call(Pid,get_login).

get_client(Pid) ->
	gen_server:call(Pid,get_client).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {client,login,reg}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([Conn,Login]) ->
    {ok, #state{client=Conn,login=Login,reg=no},0}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(get_login, _From, State) ->
	{reply, State#state.login, State};

handle_call(get_client, _From, State) ->
	{reply, State#state.client, State};

handle_call(get_users, _From, State) ->
	R = chat_online:get_user_list(chat_online),
	{reply, R, State};

handle_call({get_history,Friend}, _From, State) ->
	Login = State#state.login, %get_login(Pid),
	%io:format("chat_cli: login for history: ~p~n",[Login]),
	R=chat_storage:get_history(chat_storage, Login, Friend),
	{reply, R, State};

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(logout,State) ->
	chat_online:unregister(chat_online, self()),
	{stop,normal,State};

handle_cast({send,To,Body},State) ->
 	Login = State#state.login,
 	chat_msg_srv:send(Login,To,Body),
	{noreply, State};

handle_cast(Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(timeout,#state{reg=no}=State) ->
	io:format("chat_cli: registering ~p~n",[State#state.login]),
	chat_online:register(chat_online,self(),State#state.login),
	{noreply, State#state{reg=yes}};

handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
	chat_online:unregister(chat_online,self()),
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


