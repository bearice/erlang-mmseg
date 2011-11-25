-module(mmseg).
-export([
    start/1,
    start_link/1,
    tokenlize/1
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
start([])->
    gen_server:start({local,?MODULE},?MODULE,[],[]).

start_link([])->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

tokenlize(String) ->
    gen_server:call(mmseg,{tokenlize,String}).

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
init(Args) ->
    port_init(Args),
    put(queue,queue:new()),
    {ok, []}.

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
handle_call({tokenlize,String},From,State) ->
    case port_get(State) of 
        {ok,Port} ->
            Port ! {tokenlize,self(),From,String};
        {error,noavail} ->
            enqueue_request(State,From,String);
        {error,Other} ->
            gen_server:reply(From,{error,Other})
    end,
    {noreply,State};

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({tokenlizer,Port,From,Data}, State) ->
    gen_server:reply(From,Data),
    case dequeue_request(State) of
        undefined ->
            port_free(State,Port);
        {From,String} ->
            Port ! {tokenlize,self(),From,String}
    end,
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

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
port_init(Args) ->
    put(free_ports,port_init(Args,4,[])),
    ok.

port_init(Args,Count,Acc) ->
    [ spawn_link(mmseg_port,init,[Args]) || X <- lists:seq(0,Count)].

port_get(State) ->
    case get(free_ports) of 
        [] -> {error,noavail};
        [Port|Free] -> 
            put(free_ports,Free),
            {ok,Port}
    end.

port_free(State,Port) ->
    Free = get(free_ports),
    put(free_ports,[Port|Free]),
    ok.

enqueue_request(State,From,String) ->
    Q  = get(queue),
    Q1 = queue:in({From,String},Q),
    put(queue,Q1).

dequeue_request(State) ->
    Q  = get(queue),
    case queue:out(Q) of
        {empty,_} -> undefined;
        {R,Q1} ->
            put(queue,Q1),
            R
    end.
