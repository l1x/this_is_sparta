-module(this_is_sparta).

-author('leccine@gmail.com').

-export([
  delete_all_buckets/0, 
  delete_all_buckets/1 
]).

-define(CLUSTERS, [
  {cluster0,["node0", "node1", "node2"]},
  {cluster1,["node0", "node1", "node2"]}
]).

delete_all_buckets() ->
  error_logger:error_msg("Please specify a file ~n", []),
  {error, missing_file}.

delete_all_buckets(File) ->
  ets:new(process_registry, [set, named_table]),
  %% creating a randomized list from the file
  case file_to_list(File) of
    {ok, List} ->
      {ok, RandomizedList} = randomize_list(List), 
      %% iterating over buckets 
      lists:foreach(fun(Bucket) -> delete_a_bucket(Bucket) end, RandomizedList),
      set_alarm(500),
      delete_all_buckets_loop();
    %% could not open the file
    {error,Reason} ->
      {error,Reason}
  end.

delete_all_buckets_loop() ->
  receive
    {Pid, started, {perf, 0}} -> 
      %io:format("Got: ~p~n", [{Pid, started}]),
      ets:insert(process_registry, {Pid, started, {perf, 0}}),
      delete_all_buckets_loop();
    {Pid, running, {perf, Perf}} ->
      ets:insert(process_registry, {Pid, started, {perf, Perf}}),
      delete_all_buckets_loop();
    {'EXIT',Pid,_} ->
      %io:format("Got: ~p~n", [{'EXIT', Pid}]),
      ets:delete(process_registry, Pid),
      Len = length(ets:tab2list(process_registry)),
      case Len of
        0 -> init:stop();
        _ -> delete_all_buckets_loop()
      end;
    alarm ->
      Len = length(ets:tab2list(process_registry)),
      io:format("The current performance is: ~p req/s Running processes: ~p~n", [
        lists:sum(lists:flatten(ets:select(process_registry,[{{'_','_',{'_','$1'}},[],['$$']}]))), Len
      ]),
      set_alarm(1000),
      delete_all_buckets_loop();
    Else ->
      io:format("Got: ~p~n", [Else])
  after
    1000 -> 
      %%io:format("Timeout: ~p~n", [timeout]),
      delete_all_buckets_loop()
  end.

delete_a_bucket(Bucket) ->
  io:format("Deleting: ~p~n", [Bucket]),
  Pid = self(),
  lists:foreach(fun(Node) ->
    spawn_link(fun() -> delete_a_bucket(Bucket, Node, Pid) end)
  end, [Node || {_,List} <- ?CLUSTERS, Node <- List]),
  ok.

delete_a_bucket(Bucket, Node, ParentPid) ->
  io:format("Starting: ~p Bucket:~p Node:~p ParPid:~p ~n", [self(), Bucket, Node, ParentPid]),
  ParentPid ! {self(), started, {perf, 0}},
  {ok, Pid} = riakc_pb_socket:start_link(Node, 8087),
  riakc_pb_socket:stream_list_keys(Pid, Bucket),
  delete_loop(Pid, Bucket, ParentPid).

delete_loop(Pid, Bucket, ParentPid) ->
  receive
    {_, {_, List}} ->
      %%This is happening inside a process
      %%so doing synchronous sequential deletes are ok
      %%if not use pmap instead of foreach with callbacks 
      %%start timer
      Len = length(List),
      {Time,_} = timer:tc(fun() ->
        lists:foreach(fun(K) -> 
          %%print_dots(),
          riakc_pb_socket:delete(Pid, Bucket, K) 
        end, List) 
      end),
      ReqPerSec = req_per_sec(Time, Len),
      ParentPid ! {self(), running, {perf, ReqPerSec}},
      delete_loop(Pid, Bucket, ParentPid);
    {_, done} ->
      io:format("Finished with all of the keys in the ~p bucket.~nExiting process: ~p~n", [Bucket, self()]),
      exit("Bucket is done, terminating worker...");
    Else ->
      io:format("Got something else: ~p, terminating~n", [Else])
  end.

req_per_sec(_Time, 0) ->
  0;
req_per_sec(Time, Len) -> 
  trunc((Time / 1000) / Len).

%%print_dots() ->
%%  io:format("~c", [46]),
%%  timer:sleep(1),
%%  io:format("~c~c~c", [13,13,13]).

set_alarm(T) ->
  Pid = self(),
  spawn_link(fun() -> set(Pid, T) end).

set(Pid, T) ->
  receive
  after T -> Pid ! alarm
  end.

randomize_list(List) ->
  {ok, [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List]) ]}.
  
file_to_list(File) ->
  case filelib:is_file(File) of
    %%if it is a file
    true ->
      case file:consult(File) of
        {ok, List} ->
          %%error_logger:info_msg("Bucket list is read from file: ~p~n", [File]),
          {ok, List};
        {error,Reason} ->
          error_logger:error_msg("Cannot open file: ~p Reason: ~p~n", [File, Reason]),
          {error,Reason}
      end;
    %if it is NOT a file
    false ->
      error_logger:error_msg("Not a file: ~p~n", [File]),
      {error, not_a_file}
  end.
