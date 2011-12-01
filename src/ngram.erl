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

english([Ch|Tail],Acc,Handler) when (($a =< Ch) and (Ch =< $z)) or (($A =< Ch) and (Ch =< $Z)) or (($0 =< Ch) and (Ch =< $9)) or (Ch == $-) or (Ch == $_) ->
    english(Tail,[Ch|Acc],Handler);

english([_|Tail],[],Handler) ->
    english(Tail,[],Handler);

english([_|Tail],Acc,Handler) ->
    Handler(list_to_tuple(lists:reverse(Acc))),
    english(Tail,[],Handler);

english([],Acc,Handler) ->
    case Acc of
        [] -> ok;
        _  -> 
            Handler(list_to_tuple(lists:reverse(Acc))),
            ok
    end.

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

ngram(1,String,Handler) -> unigram(String,Handler);
ngram(2,String,Handler) -> bigram(String,Handler);
ngram(3,String,Handler) -> trigram(String,Handler);
ngram(4,String,Handler) -> fourgram(String,Handler);
ngram(5,String,Handler) -> fivegram(String,Handler);

ngram(N,String,Handler) when length(String) > N ->
    lists:foldl(fun ngram_fold/2,{N,Handler,0,[]},String).

ngram_fold(X,{N,H,N,Acc}) ->
        String = lists:reverse(Acc),
        Token = list_to_tuple(String),
        H(Token),
        [_|Tail] = String,
        NewString = lists:reverse(Tail),
        ngram_fold(X,{N,H,N-1,NewString});

ngram_fold($\n,{N,H,_,_}) ->
        {N,H,0,[]};

ngram_fold(X,{N,H,L,Acc}) when L<N ->
        {N,H,L+1,[X|Acc]}.


filter_english(Char,_) when (Char == $-) or (Char == $_) -> {Char,0};
filter_english(Char,_) when ($a =< Char) and (Char =< $z) -> {Char,0};
filter_english(Char,_) when ($A =< Char) and (Char =< $Z) -> {Char,0};
filter_english(Char,_) when ($0 =< Char) and (Char =< $9) -> {Char,0};
filter_english(_,_) -> {$\n,0}.

filter_cjk(16#76ee,_)       -> {16#76ee,16#7684} ;   % 目的
filter_cjk(16#7684,16#7684) -> {16#7684,0} ;         % 的
filter_cjk(16#7684,_)       -> {$\n,0} ;             % 的
filter_cjk(Char,_) when (16#4e00 =< Char) and (Char =< 16#9fff) -> {Char,0};
filter_cjk(Char,_) when (16#3400 =< Char) and (Char =< 16#4dbf) -> {Char,0};
filter_cjk(Char,_) when (16#3041 =< Char) and (Char =< 16#309f) -> {Char,0};
filter_cjk(Char,_) when (16#30a0 =< Char) and (Char =< 16#30ff) -> {Char,0};
filter_cjk(_,_) -> {$\n,0}.


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

do_parallel(Jobs) ->
    Parent = self(),
    Ref = make_ref(),
    [receive {Ref, Pid, Result} -> Result end
        || Pid <- [spawn_link(fun() -> Parent ! {Ref, self(), X()} end) || X <- Jobs]].

test() ->
    {ok,P} = wordfeq:start([]),
    F = fun(W) -> wordfeq:handle_word(P,W) end,

    {ok,Data} = file:read_file("in.txt"),
    S = unicode:characters_to_list(Data),

    T0 = now(),
    [SS,SE] = do_parallel([
         fun() -> {SS,_} = lists:mapfoldl(fun filter_cjk/2,0,S),    SS end
        ,fun() -> {SE,_} = lists:mapfoldl(fun filter_english/2,0,S),SE end
    ]),

    %io:format("~ts~n",[SS]),
    T1 = now(),
    TDiff0 = timer:now_diff(T1,T0),
    io:format("Filter Time used: ~p~n",[TDiff0]),

    do_parallel([
         fun() -> english(SE,[],F) end
%       ,fun() -> ngram(1,SS,F) end
        ,fun() -> ngram(2,SS,F) end
        ,fun() -> ngram(3,SS,F) end
        ,fun() -> ngram(4,SS,F) end
        ,fun() -> ngram(5,SS,F) end
%       ,fun() -> ngram(6,SS,F) end
    ]),
    
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
