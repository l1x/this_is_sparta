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
  {cluster0,["node0:port0", "node1:port1", "node2:port2"]},
  {cluster1,["node0:port0", "node1:port1", "node2:port2"]}
]).

delete_all_buckets() ->
  %% connects to Riak cluster 
  %% queries the to_be_deleted bucket for keys using 2i
  %% if finds keys checks if they exist in has_been_deleted
  %% if not deletes all of its keys and inserts it to the has_been_deleted
  %% TODO: open multiple connection, run parallel in different clusters

  %{ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087).
  %{ok, O} = riakc_pb_socket:get(Pid, <<"groceries">>, <<"mine">>).
  %riakc_pb_socket:get_bucket(Pid, <<"groceries">>).

  %% creating a connection registry
  ets:new(connection_registry, [bag, named_table]),
  lists:foreach(fun(Node) ->
    [Host, Port] = string:tokens(Node,":"),
    io:format("Connecting to host: ~p on port: ~p ~n", [Host, Port]),
    case riakc_pb_socket:start_link(Host, Port) of
      {ok, Pid} -> 
        error_logger:info_msg("Successfully connected to ~p~n", [Host]),
        ets:insert(connection_registry,{Host, Pid});
      _ -> 
        error_logger:error_msg("Cannot connect to ~p", [Host])
    end
  end, [Node || {_,List} <- ?CLUSTERS, Node <- List]).




