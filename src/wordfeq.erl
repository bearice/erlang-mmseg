-module(wordfeq).
-compile([export_all]).
-export([
    start/1,
    start_link/1,
    handle_word/2,
    get_result/2,
    close/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-record(state,{words,seq}).

start([])->
    gen_server:start(?MODULE,[],[]).

start_link([])->
    gen_server:start_link(?MODULE,[],[]).

handle_word(P,Word) ->
    gen_server:call(P,{handle_word,Word}).

get_result(P,L) ->
    gen_server:call(P,{get_result,L}).

close(P) ->
    gen_server:cast(P,close).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(_) ->
    Table = ets:new(wordfeq,[]),
    put(seq,0),
    {ok, #state{words=Table,seq=0}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({handle_word,Word},From,State) ->
    Words = State#state.words,
    Seq = get(seq),
    put(seq,Seq+1),
    case ets:insert_new(Words,{Word,1,Seq}) of
        false ->
            ets:update_counter(Words,Word,{2,1}), 
            ets:update_element(Words,Word,{3,Seq});
        true ->
            ok
    end,
    {reply, ok, State};

handle_call({get_result,L},From,State) ->
    Words = State#state.words,
    Table = ets:table(Words),
    Sorted = qlc:keysort(2,Table,[{order,descending}]),
    C = qlc:cursor(Sorted),
    Ret = qlc:next_answers(C,L),
    qlc:delete_cursor(C),
    {reply, {ok,Ret}, State};

handle_call(Request, From, State) ->
    {reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(close,State) ->
    {stop,normal,State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
   {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.


