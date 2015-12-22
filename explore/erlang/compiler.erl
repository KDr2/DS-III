-module(compiler).

-export([echo/1, incr/1, add/2]).
-export([request/2]).

-define(INCR(A), (A + 1)).

echo(A) ->
    A.

incr(B) ->
    ?INCR(B).

add(A, B) ->
    A + B.


request(File, Data) ->
    Ref = do_request(File, {data, Data}),
    wait_reply(Ref).

%%-compile({inline,[do_request/2, wait_reply/1]}).

do_request(Io, Request) ->
    R = make_ref(),
    Io ! {request, R, Request},
    R.

wait_reply(Reference) ->
    receive
        {reply,Reference} ->
            ok
    end.
