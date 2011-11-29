-module(ngram).
-export([ngram/3]).

break_word([26159|_]) -> %是
    1; 
break_word([30340|_]) -> %的
    1; 
break_word([12377,12391|_]) -> %desu
    2;
break_word(_)->
    0.


unigram([$\n|Tail],Handler) ->
    unigram(Tail,Handler);

unigram([Head|Tail],Handler) ->
    Handler({Head}),
    unigram(Tail,Handler);

unigram(_,_) -> ok.

bigram([$\n,H2|Tail],Handler) ->
    bigram([H2|Tail],Handler);

bigram([_,$\n|Tail],Handler) ->
    bigram(Tail,Handler);

bigram([H1,H2|Tail],Handler) ->
    Handler({H1,H2}),
    bigram([H2|Tail],Handler);

bigram(_,_) -> ok.


trigram([$\n,H2,H3|Tail],Handler) ->
    trigram([H2,H3|Tail],Handler);

trigram([_,$\n,H3|Tail],Handler) ->
    trigram([H3|Tail],Handler);

trigram([_,_,$\n|Tail],Handler) ->
    trigram(Tail,Handler);

trigram([H1,H2,H3|Tail],Handler) ->
    Handler({H1,H2,H3}),
    trigram([H2,H3|Tail],Handler);

trigram(_,_) -> ok.

fourgram([$\n,H2,H3,H4|Tail],Handler) ->
    fourgram([H2,H3,H4|Tail],Handler);

fourgram([_,$\n,H3,H4|Tail],Handler) ->
    fourgram([H3,H4|Tail],Handler);
 
fourgram([_,_,$\n,H4|Tail],Handler) ->
    fourgram([H4|Tail],Handler);
     
fourgram([_,_,_,$\n|Tail],Handler) ->
    fourgram(Tail,Handler);

fourgram([H1,H2,H3,H4|Tail],Handler) ->
    Handler({H1,H2,H3,H4}),
    fourgram([H2,H3,H4|Tail],Handler);

fourgram(_,_) -> ok.

fivegram([$\n,H2,H3,H4,H5|Tail],Handler) ->
    fivegram([H2,H3,H4,H5|Tail],Handler);

fivegram([_,$\n,H3,H4,H5|Tail],Handler) ->
    fivegram([H3,H4,H5|Tail],Handler);

fivegram([_,_,$\n,H4,H5|Tail],Handler) ->
    fivegram([H4,H5|Tail],Handler);

fivegram([_,_,_,$\n,H5|Tail],Handler) ->
    fivegram([H5|Tail],Handler);

fivegram([_,_,_,_,$\n|Tail],Handler) ->
    fivegram(Tail,Handler);

fivegram([H1,H2,H3,H4,H5|Tail],Handler) ->
    Handler({H1,H2,H3,H4,H5}),
    fivegram([H2,H3,H4,H5|Tail],Handler);

fivegram(_,_) -> ok.

%ngram(1,String,Handler) -> unigram(String,Handler);
%ngram(2,String,Handler) -> bigram(String,Handler);
%ngram(3,String,Handler) -> trigram(String,Handler);
%ngram(4,String,Handler) -> fourgram(String,Handler);
%ngram(5,String,Handler) -> fivegram(String,Handler);

ngram(N,String,Handler) when length(String) > N ->
    lists:foldl(fun ngram_fold/2,{N,Handler,0,[]},String).

ngram_fold($\n,{N,H,_,_}) ->
        {N,H,0,[]};

ngram_fold(X,{N,H,L,Acc}) when L<N ->
        {N,H,L+1,[X|Acc]};

ngram_fold(X,{N,H,N,Acc}) ->
        Token = list_to_tuple(lists:reverse(Acc)),
        H(Token),
        {N,H,1,[X]}.

filter(16#76ee,_)       -> {16#76ee,16#7684} ;   % 目的
filter(16#7684,16#7684) -> {16#7684,0} ;         % 的
filter(16#7684,_)       -> {$\n,0} ;             % 的

filter(Char,_) when ($a =< Char) and (Char =< $z) -> {Char,0};
filter(Char,_) when ($A =< Char) and (Char =< $Z) -> {Char,0};
filter(Char,_) when ($0 =< Char) and (Char =< $9) -> {Char,0};
filter(Char,_) when (16#4e00 =< Char) and (Char =< 16#9fff) -> {Char,0};
filter(Char,_) when (16#3400 =< Char) and (Char =< 16#4dbf) -> {Char,0};
filter(Char,_) when (16#3041 =< Char) and (Char =< 16#309f) -> {Char,0};
filter(Char,_) when (16#30a0 =< Char) and (Char =< 16#30ff) -> {Char,0};
filter(_,_) -> {$\n,0}.


handle_word(String) ->
    L = tuple_to_list(String),
    io:format("~ts\n",[L]),
    ok.

test2() ->
    mmseg:start([]),
    {ok,File} = file:open("in.txt",[read,binary]),
    {ok,Data} = file:read(File,60000),
    {ok,List} = mmseg:tokenlize(Data),
    {ok,P} = wordfeq:start([]),
    T1 = now(),
    lists:foreach(fun({Token,_,_}) ->
        wordfeq:handle_word(P,Token)
    end,List), 
    {ok,R} = wordfeq:get_result(P,50),
    T2 = now(),
    TDiff = timer:now_diff(T2,T1),
    io:format("Time used: ~p~n",[TDiff]),
    lists:foreach(fun({W,C,_}) ->
        io:format("~ts: ~p~n",[W,C])
    end,R),
    ok.

test() ->
    {ok,Data} = file:read_file("in.txt"),
    S = unicode:characters_to_list(Data),
    {SS,_} = lists:mapfoldl(fun filter/2,0,S),
    %io:format("~ts~n",[SS]),
    {ok,P} = wordfeq:start([]),
    F = fun(W) -> wordfeq:handle_word(P,W) end,
    T1 = now(),
    %ngram(1,SS,F),
    ngram(2,SS,F),
    ngram(3,SS,F),
    ngram(4,SS,F),
    ngram(5,SS,F),
    ngram(6,SS,F),
    
    T2 = now(),
    TDiff1 = timer:now_diff(T2,T1),
    io:format("Ngram Time used: ~p~n",[TDiff1]),

    {ok,R} = wordfeq:get_result(P,50),

    T3 = now(),
    TDiff2 = timer:now_diff(T3,T2),
    io:format("Sort Time used: ~p~n",[TDiff2]),
    lists:foreach(fun({W,C,_}) ->
        Str = tuple_to_list(W),
        io:format("~ts: ~p~n",[Str,C])
    end,R),
    ok.
