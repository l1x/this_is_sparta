#!/usr/bin/env bash
erl \
-noshell \
-K true \
-A 32 \
-W w \
-zdbbl 131072 \
-P 256000 \
-pa riak-erlang-client-1.4.2/ebin riak-erlang-client-1.4.2/deps/*/ebin \
-eval "this_is_sparta:delete_all_buckets()"
