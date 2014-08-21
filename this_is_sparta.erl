%%Copyright 2014 Istvan Szukacs and contributors
%%
%%Licensed under the Apache License, Version 2.0 (the "License");
%%you may not use this file except in compliance with the License.
%%You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%%Unless required by applicable law or agreed to in writing, software
%%distributed under the License is distributed on an "AS IS" BASIS,
%%WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%See the License for the specific language governing permissions and
%%limitations under the License

-module(this_is_sparta).

-author('leccine@gmail.com').

-export([
  delete_all_buckets/0
]).

-define(CLUSTERS, [
  {cluster0,["127.0.0.1:10017", "127.0.0.1:10027", "127.0.0.1:10037"]}
]).

-define(MIN_DAYS, 3).
-define(MAX_DAYS, 90).
-define(ONE_DAY_MS, 86400000).

delete_all_buckets() ->
  %% connects to Riak cluster 
  %% queries the to_be_deleted bucket for keys using 2i
  %% if finds keys checks if they exist in has_been_deleted
  %% if not deletes all of its keys and inserts it to the has_been_deleted
  %% TODO: open multiple connection, run parallel in different clusters

  %% creating a connection registry
  {ok, created} = create_connection_registry(),
  %% connecting to Riak
  {ok, connected} = connect_to_all_nodes(),
  %% get Pid to talk to Riak
  {ok, Pid} = get_connection_pid(),

  %% main loop
  {ok, List_of_dates} = list_of_dates(),
  %% get the partitioned list
  Plist = part(List_of_dates),
  %% iterates over  
  lists:foreach(fun(Pair) ->
    %% this is the safeguard for not allowing
    %% incorrect input and making sure we get a 
    %% range of the keys only
    case Pair of
      %% correct input, one list with two items
      [_, _] ->
        [Fst, Scnd] = Pair,
        %%io:format("1st: ~p 2nd: ~p~n", [Fst, Scnd]),
        {ok, Buckets} = list_keys_with_2i(Pid, <<"to_be_deleted">>, "created_at", Fst, Scnd),
        lists:foreach(fun(Bucket) ->
          io:format("Bucket submitted for deletion: ~p~n", [Bucket]),
          delete_keys_with_2i(Plist, Pid, Bucket, "created_at"),
          %% adding the name of the bucket as a key to the has_been_deleted bucket
          {ok, Ts} = get_timestamp(),
          Json = "{\"ts\":" ++ integer_to_list(Ts) ++ "}",
          Obj0 = riakc_obj:new(<<"has_been_deleted">>, Bucket, list_to_binary(Json)),
          Md0 = riakc_obj:get_update_metadata(Obj0),
          Md1 = riakc_obj:set_secondary_index(Md0, [{{integer_index, "created_at"}, [Ts]}]),
          Obj1 = riakc_obj:update_metadata(Obj0,Md1),
          riakc_pb_socket:put(Pid, Obj1)
        end, Buckets);
      %% incorrect input
      _ -> 
        io:format("Incorrect input: ~p~n", [Pair])
    end

  end, Plist),
   
  %% main end

  timer:sleep(1000),
  erlang:halt(0),
ok.

create_connection_registry() ->
  io:format("~p~n", [get_timestamp()]),
  ets:new(connection_registry, [bag, named_table]),
  {ok,created}.

connect_to_all_nodes() ->
  lists:foreach(fun(Node) ->
    [Host, Port] = string:tokens(Node,":"),
    io:format("Connecting to host: ~p on port: ~p ~n", [Host, Port]),
    case riakc_pb_socket:start_link(Host, list_to_integer(Port)) of
      {ok, Pid} ->
        error_logger:info_msg("Successfully connected to ~p~n", [Host]),
        ets:insert(connection_registry,{Host, Pid});
      _ ->
        error_logger:error_msg("Cannot connect to ~p", [Host]),
        timer:sleep(1000),
        erlang:halt(1)
    end
  end, [Node || {_,List} <- ?CLUSTERS, Node <- List]),
  {ok, connected}.

get_connection_pid() ->
  %%[{[Host],Pid}, {}, {}...]
  ConnList = ets:tab2list(connection_registry),
  Index = random:uniform(length(ConnList)),
  {_,Pid} = lists:nth(Index,ConnList),
  {ok, Pid}.

%% this is returning {ok, List}
list_keys_with_2i(Pid, Bucket, Index, Min, Max) ->
  %% this has to take the from and to values
  %% instead of getting all of the keys at once
  %% obvious cases should be captured
  %% Min < Max, Min > 0, Max > 0 etc
  %% io:format("Pid: ~p Bucket: ~p Index: ~p Min: ~p Max: ~p ~n", [Pid, Bucket, Index, Min, Max]),
  {ok,{_,List,_,_}} = riakc_pb_socket:get_index(
    Pid, 
    Bucket, 
    {integer_index, Index}, Min, Max),
  {ok, List}.

list_keys_with_2i(Pid, Bucket, Index, Min, Max, Maxresults) ->
  %% this is returning {ok, List, Continuation}
  %% if the continuation is undefinied you reached the end of the key range
  {ok,{_,List,_, Continuation}} = riakc_pb_socket:get_index_range(
    Pid, Bucket, 
    {integer_index, Index}, Min, Max,
    [{max_results, Maxresults}]),
  {ok, List, Continuation}.

list_keys_with_2i(Pid, Bucket, Index, Min, Max, Maxresults, Cont) ->
  %% this is returning {ok, List, Continuation}
  %% if the continuation is undefinied you reached the end of the key range
  %%{ok,{index_results_v1,
  %%  [<<"908eb9bd4473db38-test-1406913729350">>,<<"908eb9bd4473db38-test-1407000129350">>],
  %%  undefined,
  %%  <<"g2gCbgYARi++l0cBbQAAACM5MDhlYjliZDQ0NzNkYjM4LXRlc3QtMTQwNzAwMDEyOTM1MA==">>}}
  {ok,{_,List,_, Contnext}} = riakc_pb_socket:get_index_range(
    Pid, Bucket, 
    {integer_index, Index}, Min, Max,
    [{max_results, Maxresults}, {continuation, Cont}]),
  {ok, List, Contnext}.

delete_keys_with_2i_loop(Pid, Bucket, Index, Fst, Scnd, Maxresults) -> 
  {ok, Keys, Cont} = list_keys_with_2i(Pid, Bucket, Index, Fst, Scnd, Maxresults),
  lists:foreach(fun(Key) ->
    %%io:format("Deleting the following key: ~p in bucket: ~p ~n", [Key, Bucket]),
    %%deleting the key from the bucket
    riakc_pb_socket:delete(Pid, Bucket, Key,[{rw, 3}])
  end, Keys),
  delete_keys_with_2i_loop(Pid, Bucket, Index, Fst, Scnd, Maxresults, Cont).

delete_keys_with_2i_loop(_Pid, _Bucket, _Index, _Fst, _Scnd, _Maxresults, undefined) ->
  %% this is the exit point
  {ok, done};
delete_keys_with_2i_loop(Pid, Bucket, Index, Fst, Scnd, Maxresults, Cont) ->
  {ok, Keys, Contnext} = list_keys_with_2i(Pid, Bucket, Index, Fst, Scnd, Maxresults, Cont),
  lists:foreach(fun(Key) ->
    io:format("Deleting the following key: ~p in bucket: ~p ~n", [Key, Bucket]),
    %%deleting the key from the bucket
    riakc_pb_socket:delete(Pid, Bucket, Key,[{rw, 3}])
  end, Keys),
  delete_keys_with_2i_loop(Pid, Bucket, Index, Fst, Scnd, Maxresults,Contnext).

%% this can be a spawn()
delete_keys_with_2i(Plist, Pid, Bucket, Index) ->
  %% iterating over Plist
  %% each iteration lists a range of keys
  %% and deletes them
  lists:foreach(fun(Pair) ->
    %% this is the safeguard for not allowing
    %% incorrect input and making sure we get a
    %% range of the keys only
    case Pair of
      %% correct input, one list with two items
      [_, _] ->
        [Fst, Scnd] = Pair,
        %% this can be moved to a separate spawn() so
        %% the keys can be streamed and received
        %% while the continuation is not undefinied
        {ok,done} = delete_keys_with_2i_loop(Pid, Bucket, Index, Fst, Scnd, 100);
      %% incorrect input
      _ ->
        %%io:format("Incorrect input: ~p~n", [Pair]),
        ok
    end
  end, Plist),
  ok.

get_timestamp() ->
    {Mega,Sec,_} = erlang:now(),
    Ts = (Mega*1000000+Sec)*1000,
    {ok, Ts}.

list_of_dates() -> 
  %% returns a list of dates we need to
  %% delete the data of
  {ok, Ts} = get_timestamp(),
  Max = Ts - (3 * ?ONE_DAY_MS),
  Min = Max - (90 * ?ONE_DAY_MS),
  List = lists:seq(Min,Max,?ONE_DAY_MS),
  {ok, List}. 

%% partitioning lists
part(List) ->
  part(List, []).
part([], Acc) ->
  lists:reverse(Acc);
part([H], Acc) ->
  lists:reverse([[H]|Acc]);
part([H1,H2|T], Acc) ->
  part(T, [[H1,H2]|Acc]).

%% end
