this_is_sparta
==============

Delete all the keys in a defined list of buckets in Riak
```bash
erlc this_is_sparta.erl && 
erl -pa ../riak-erlang-client/ebin ../riak-erlang-client/deps/*/ebin -noshell -eval 'this_is_sparta:deletall_buckets("list_of_buckets.txt")'
```

The list file is using the Erlang binary syntax for strings ('<<string>>'.)
