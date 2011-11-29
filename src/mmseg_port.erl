-module(mmseg_port).
-export([
    init/1,
    stop/1
]).

stop(Port) ->
    Port ! close.

init(Args) ->
    P = init_port(Args),
    loop(P,Args).
    
init_port(Args) ->
    open_port(
        {spawn_executable,"priv/mmseg.exec"},
        [
            {args,Args},
            {packet, 2},
            binary
        ]
    ).
    
loop(P,Args) ->
    receive
        {tokenlize,Pid,Client,String} ->
            P ! {self(),{command,String}},
            R = recv(P,[]),
            Pid ! {tokenlizer,self(),Client,R},
            NP = case R of 
                {ok,_} -> 
                    P;
                {error,_} -> 
                    init_port(Args)
            end,
            erlang:hibernate(?MODULE,loop,[NP,Args]);
        stop ->
            port_close(P),
            ok;
        Any ->
            loop(P,Args)
    end.

recv(P,Acc) ->
    receive
        {P,{data,<<0>>}} -> 
            {ok,lists:reverse(Acc)};
        {P,{data,<<Pos:16/integer,Data/binary>>=Pkt}} ->
            Len = byte_size(Pkt)-2,
            %io:format("~p,~p,~ts~n",[Pos,Len,Data]),
            recv(P,[{Data,Pos,Len}|Acc]);
        {P,close} ->
            {error,port_closed};
        {'EXIT',P,Reason} ->
            {error,Reason}
    end.
