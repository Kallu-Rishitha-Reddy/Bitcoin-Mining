%%%-------------------------------------------------------------------
%%% @author amergu
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Sep 2022 11:53 PM
%%%-------------------------------------------------------------------
-module(server).
-author("amergu").

%% API
-export([start/0, worker/0, spawner/4, server/3, client/1, shutdown/0, timer/0]).

start() ->
  MaxIter = 576460752303423488,
  MaxWorkers = 10,
  {ok, ZeroCount} = io:read("Number of leading zeros : "),
  Timer_PID = spawn(server, timer,[]),
  register(timer, Timer_PID),
  Server_PID = spawn(server, server, [ZeroCount, MaxWorkers, MaxIter]),
  register(server, Server_PID),
  server ! selfSolve,
  ok.

server(ZeroCount, MaxWorkers, MaxIter) ->
  receive
    selfSolve ->
      spawn(server, spawner, [ZeroCount, MaxIter, MaxWorkers, node()]),
      server(ZeroCount, MaxWorkers, MaxIter);
    {registerClient, ClientNode} ->
      spawn(server, spawner, [ZeroCount, MaxIter, MaxWorkers, ClientNode]),
      server(ZeroCount, MaxWorkers, MaxIter);
    shutdown ->
      timer ! endTimer
  end.

shutdown() ->
    server ! shutdown.

client(ServerIP) ->
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

spawner(ZeroCount, MaxIter, 0, Node) ->
  receive
    sorrybro -> false; %io:fwrite("No matches found :(~n");
    {success, SuccessString, ShaString} -> io:fwrite("~p\t~p~n", [ShaString, SuccessString])
  end;

spawner(ZeroCount, MaxIter, Max_Workers, Node) ->
  Worker_ID = spawn(Node, server, worker, []),
  Worker_ID ! {self(), MaxIter, ZeroCount},
  spawner(ZeroCount, MaxIter, Max_Workers - 1, Node),
  receive
    sorrybro -> false; %io:fwrite("No matches found :( ! I am : ~p~n",[Node]);
    {success, Result, ShaString} -> io:fwrite("~p\t~p~n", [ShaString, Result])
  end.

timer() ->
  {RealStartTime, _} = statistics(wall_clock),
  {CPUStartTime, _} = statistics(runtime),
  receive
    endTimer ->
      {CurRealTime, _} = statistics(wall_clock),
      {CurCPUTime, _} = statistics(runtime),
      RunTime = CurRealTime - RealStartTime,
      CPUTime = CurCPUTime - CPUStartTime,
      Ratio = CPUTime/RunTime,
      io:format("Real Time : ~p ms~nCPU Time : ~p ms~nRatio : ~p~n", [RunTime, CPUTime, Ratio])
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