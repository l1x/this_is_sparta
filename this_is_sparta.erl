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
  delete_all_buckets/0,
  load_fixtures/0
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
        io:format("1st: ~p 2nd: ~p~n", [Fst, Scnd]),
        {ok, Buckets} = list_keys_with_2i(Pid, <<"to_be_deleted">>, "created_at", Fst, Scnd),
        lists:foreach(fun(Bucket) ->
          io:format("Bucket submitted for deletion: ~p~n", [Bucket]),
          delete_keys_with_2i(Plist, Pid, Bucket, "created_at")
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

list_keys_with_2i(Pid, Bucket, Index, Min, Max) ->
  %% this has to take the from and to values
  %% instead of getting all of the keys at once
  %% obvious cases should be captured
  %% Min < Max, Min > 0, Max > 0 etc
  io:format("Pid: ~p Bucket: ~p Index: ~p Min: ~p Max: ~p ~n", [Pid, Bucket, Index, Min, Max]),
  {ok,{_,List,_,_}} = riakc_pb_socket:get_index(
    Pid, 
    Bucket, 
    {integer_index, Index}, Min, Max
  ),
  {ok, List}.

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
        io:format("1st: ~p 2nd: ~p~n", [Fst, Scnd]),
        {ok, Keys} = list_keys_with_2i(Pid, Bucket, Index, Fst, Scnd),
        lists:foreach(fun(Key) ->
          io:format("Deleting the following key: ~p in bucket: ~p ~n", [Key, Bucket]),
          %%deleting the key from the bucket
          riakc_pb_socket:delete(Pid, Bucket, Key,[{rw, 3}]),

          %%inserting 
          %%riak_pb_socket:put -> has_been_deleted

        end, Keys);
      %% incorrect input
      _ ->
        io:format("Incorrect input: ~p~n", [Pair])
    end
  end, Plist),
  ok.


load_fixtures() ->
  %% Java is using milliseconds for timestamps
  %%
  %% Timestamps:
  %% 
  %% 1407213871425 Mon Aug 04 2014 21:44:31 GMT-0700 (PDT)
  %% 1407223871425 Tue Aug 05 2014 00:31:11 GMT-0700 (PDT)
  %% 1407323871425 Wed Aug 06 2014 04:17:51 GMT-0700 (PDT)
  %% 1407465976425 Thu Aug 07 2014 19:46:16 GMT-0700 (PDT) 
  %%
  %% Real buckets for deletion:
  %% 808eb9bd4473db38-test-1407449017112 808eb9bd4473db38-test-1406174465901 etc.
  %% 
  %% riakc_obj:new(<<"bucket">>, <<"key">>, <<"data">>).
  %% creating a connection registry
  {ok, created} = create_connection_registry(),
  %% connecting to Riak
  {ok, connected} = connect_to_all_nodes(),
  %% get Pid to talk to Riak
  {ok, Pid} = get_connection_pid(),
  Buckets = [
    {<<"808eb9bd4473db38-test-1407449017112">>, 1407213871425},
    {<<"808eb9bd4473db38-test-1406174465901">>, 1407223871425},
    {<<"808eb9bd4473db38-test-1407450065843">>, 1407323871425},
    {<<"808eb9bd4473db38-test-1408451096759">>, 1407465976425}
  ],
  lists:foreach(fun(T) ->
    {Key, Ts} = T,
    io:format("Key: ~p Timestamp: ~p~n", [Key, Ts]),
    Obj0 = riakc_obj:new(<<"to_be_deleted">>, Key, <<"{\"ts\": 1407213871425}">>),
    %%Obj1 = riakc_obj:get_update_metadata(Obj0),
    %%Obj2 = riakc_obj:set_secondary_index(Obj0, [{{integer_index, "created_at"}, [Ts]}]),
    riakc_pb_socket:put(Pid, Obj0)
  end, [T || T <- Buckets]),
  erlang:halt(0),
  ok.

get_timestamp() ->
    {Mega,Sec,_} = erlang:now(),
    Ts = (Mega*1000000+Sec)*1000,
    {ok, Ts}.

list_of_dates() -> 
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

%%
%%bucket_deleted(Pid, Bucket, Key) ->
%%  {ok, Obj} = riakc_pb_socket:get(Pid, Bucket, Key),
%%  {ok, Obj}.

  %{ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087).
  %{ok, O} = riakc_pb_socket:get(Pid, <<"groceries">>, <<"mine">>).
  %riakc_pb_socket:get_bucket(Pid, <<"groceries">>).
