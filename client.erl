%%%-------------------------------------------------------------------
%%% @author amergu
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Sep 2022 11:30 PM
%%%-------------------------------------------------------------------
-module(client).
-author("amergu").

%% API
-export([worker/0,start/1]).


start(ServerIP) ->
  ServerNode = list_to_atom("server@" ++ ServerIP),
  Server_PID = rpc:call(ServerNode, erlang, whereis, [server]),
  Server_PID ! {registerClient, node()}.

worker() ->
  receive
    done ->
      io:fwrite("I give up!");
    {Server_PID, MaxIter, ZeroCount} ->
      Result = logic(MaxIter, ZeroCount),
      if
        Result == false -> Server_PID ! sorrybro;
        true -> {SuccessString, ShaString} = Result, Server_PID ! {success, SuccessString, ShaString}
      end
  end.


logic(0, ZeroCount) ->
  false;

logic(MaxIter, ZeroCount) ->
  GatorLinkID = "amergu;",
  RandomString = GatorLinkID ++ get_random_string(rand:uniform(50), "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"),
  ShaString = io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, RandomString))]),
  Result = is_good_string(ZeroCount, RandomString, ShaString),

  if
    Result == false -> logic(MaxIter - 1, ZeroCount);
    true -> {Result, ShaString}
  end.

get_leading_string(0, CurString) -> CurString;
get_leading_string(ZeroCount, CurString) -> get_leading_string(ZeroCount - 1, CurString ++ "0").

is_good_string(ZeroCount, RandomString, ShaString) ->
  Res = string:prefix(ShaString, get_leading_string(ZeroCount, "")),
  if
    Res == nomatch -> false; %io:fwrite("Not the desired string~n"),
    true -> RandomString
  end.

get_random_string(Length, CharSet) ->
  lists:foldl(fun(_, CurStr) ->
    [lists:nth(rand:uniform(length(CharSet)),
      CharSet)]
    ++ CurStr
              end, [], lists:seq(1, Length)).
