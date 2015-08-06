%% @author sergey
%% @doc @todo Add description to chat_storage.


-module(chat_storage).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,
		 get_client_list/1,add_message/4,get_history/3,
		 
		 add_message_1/4
	   ]).

start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE, [], []).

get_client_list(P) ->
	gen_server:call(P,get_client_list).

get_history(P,Name1,Name2) ->
	%{Messages,Senders,Receivers} = get_tables(P),
	gen_server:call(P,{get_history,Name1,Name2}).

add_message(P,From,To,Body) ->
	gen_server:call(P,{add_message,From,To,Body}).
add_message_1(P,From,To,Body) ->
	%should we wrap all in this function into a single transaction ?
	%unwrapped,it will not interfere with other add's,
	%but will sometimes make unpleasant influence on reads from our improvized database
	Mid = get_new_mid(P),
	{Messages,Senders,Receivers} = get_tables(P),
	ets:insert(Messages,{Mid,To,From,Body}), % timestamp not needed, messages can be sorted by message id
	ets:insert(Senders,{From,Mid}),
	ets:insert(Receivers,{To,Mid}),
	{ok,Mid}.

%helper functions
get_new_mid(P) ->
	gen_server:call(P,get_new_mid).
get_tables(P)  ->
	gen_server:call(P,	get_tables).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {users,messages,sndrs,rcvrs}).

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
init([]) ->
	io:format("chat_storage: ~w initing database~n",[self()]),
	Messages = ets:new(?MODULE,[set]),
	ets:insert(Messages,{lastMid,0}),
    {ok, #state{users 		= ets:new(?MODULE,[set]),
				messages 	= Messages, 
				sndrs 		= ets:new(?MODULE,[bag]),
				rcvrs 		= ets:new(?MODULE,[bag])}}.


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
handle_call(get_client_list,_From,#state{users=U}=State) ->
	T=ets:tab2list(U),
	R=[L || {L} <- T],
	{reply,R,State};

handle_call(get_new_mid,_From,State) ->
	Mid = case ets:lookup(State#state.messages,lastMid) of
	[{_,OldMid}] 	-> ets:insert(State#state.messages,{lastMid,OldMid+1}),
					   OldMid+1;
	_ 			 	-> {error,"No lastMid record found in messages table"}
	end,
	{reply,{ok,Mid},State};

handle_call(get_tables,_From,State) ->
	{reply,{ok,State#state.messages,State#state.sndrs,State#state.rcvrs},State};

handle_call({add_message,From,To,Body},_From,State) ->
	Mid = case ets:lookup(State#state.messages,lastMid) of
	[{_,OldMid}] -> ets:insert(State#state.messages,{lastMid,OldMid+1}), 
					OldMid+1;
	_ -> erlang:error("No lastMid record found in messages table")
	end,
	% only Mid generation is needed to be synchronous (as it must be a transaction);
	% the rest of adding to DB must relate on database's own concurrency
	% ets:insert(State#state.messages,{lastMid,Mid}),
	ets:insert(State#state.messages,{Mid,To,From,Body}), % timestamp not needed, messages can be sorted by message id
	ets:insert(State#state.sndrs,{From,Mid}),
	ets:insert(State#state.rcvrs,{To,Mid}),
	{reply,{ok,Mid},State};

handle_call({get_history,Login,Friend},_From,State) ->
	Lout1 = ets:lookup(State#state.sndrs,Login),
	Lout2 = ets:lookup(State#state.rcvrs,Friend),
	Lout =    intersect(mapsnd(Lout1), mapsnd(Lout2)),
	Lin1  = ets:lookup(State#state.sndrs,Friend),
	Lin2  = ets:lookup(State#state.rcvrs,Login),
	Lin   = intersect(mapsnd(Lin1), mapsnd(Lin2)),
	L = lists:sort(Lin ++ Lout),
	{reply,{ok,L},State};

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
handle_info(Info, State) ->
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
terminate(Reason, State) ->
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

intersect(M,N) ->
	I=sets:intersection(sets:from_list(M),sets:from_list(N)),
	sets:to_list(I).

mapsnd(L) -> [X || {_,X} <-L].



