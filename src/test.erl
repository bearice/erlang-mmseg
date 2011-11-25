-module(test).
-export([go/0]).

go() ->
    P = open_port({spawn_executable,"priv/mmseg.exec"},[{packet, 2},binary]),
    P ! {self(),{command,<<"去年的今天，也就是可爱的饭友期盼等待的*（505）*后的去年的今天，饭否终于回来了,今年的今天，是饭否回归整整一周年，我们无比欢欣，无比激情，欢欣的是饭否终于回来了，我们可爱的旧饭友不仅在饭否上重聚一起，而且还在饭否上认识了更多的新饭友！激情的是我们要用我们的激情做出更多更好的应用来回报我们更加激情的饭友！ 鳥島（とりしま）は、伊豆諸島の島（無人島）。 全島が国の天然記念物（天然保護区域）に指定されている（後述を参照）。特別天然記念物アホウドリの生息地としても有名である。他の「鳥島」と区別して、特に伊豆鳥島とも呼ばれる。東京都に属するがいずれの町村にも属さない。東京都直轄であり、都総務局の出先機関である八丈支庁が管理している。Writes the data with standard syntax in the same way as ~w, but breaks terms whose printed representation is longer than one line into many lines and indents each line sensibly. It also tries to detect lists of printable characters and to output these as strings. For example: 1efe2321feadf/dwadw/wa http://fanfou.com">>}},
    recv(P).

recv(P) ->
    receive
        {P,{data,<<0>>}} -> ok;
        {P,{data,<<Pos:16/integer,D/binary>>=Pkt}} ->
            L = byte_size(Pkt)-2,
            io:format("~p,~p,~ts~n",[Pos,L,D]),
            recv(P);
        Any -> io:format("Error ~p",[Any])
    end.
