-module(this_is_sparta).

-author('leccine@gmail.com').

-export([
  delete_all_buckets/0, 
  delete_all_buckets/1, 
  delete_a_bucket/2
]).

-define(CLUSTERS, [
  {cluster0,["node0", "node1", "node2"]},
  {cluster1,["node0", "node1", "node2"]}
]).

delete_all_buckets() ->
  error_logger:error_msg("Please specify a file ~n", []),
  {error, missing_file}.

delete_all_buckets(File) ->
  %% creating a randomized list from the file
  case file_to_list(File) of
    {ok, List} ->
      {ok, RandomizedList} = randomize_list(List), 
      %% iterating over buckets and spawning a process for each iteration
      lists:foreach(fun(Bucket) ->
        spawn_link(fun() -> delete_a_bucket(Bucket) end)
      end, RandomizedList),
      {ok, deleted_all};
    %% could not open the file
    {error,Reason} ->
      {error,Reason}
  end.

delete_a_bucket(Bucket) ->
  io:format("Deleting: ~p~n", [Bucket]),
  lists:foreach(fun(Node) ->
    spawn_link(fun() -> delete_a_bucket(Bucket, Node) end)
  end, [Node || {_,List} <- ?CLUSTERS, Node <- List]),
  ok.

delete_a_bucket(Bucket, Node) ->
  %io:format("B:~p N:~p~n", [Bucket, Node]),
  {ok, Pid} = riakc_pb_socket:start_link(Node, 8087),
  riakc_pb_socket:stream_list_keys(Pid, Bucket),
  delete_loop(Pid, Bucket).

delete_loop(Pid, Bucket) ->
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
      NumProcesses = length(erlang:processes()),
      io:format("# of deletes: ~p Time[ms] ~p Req/s: ~p NumProcesses: ~p Overall performance: ~p ~n", [Len, Time, ReqPerSec, NumProcesses, NumProcesses*ReqPerSec]),
      delete_loop(Pid, Bucket);
    {_, done} ->
      io:format("Finished with all of the keys in the ~p bucket. Exiting process... ~n", [Bucket]),
      exit("Bucket is done, terminating worker...");
    Else ->
      io:format("Got something else: ~p, terminating~n", [Else])
  end.

req_per_sec(_Time, 0) ->
  0;
req_per_sec(Time, Len) -> 
  trunc((Time / 1000) / Len).

print_dots() ->
  io:format("~c", [46]),
  timer:sleep(1),
  io:format("~c~c~c", [13,13,13]).

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
