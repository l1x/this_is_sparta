#!/usr/bin/env bash

now=$(date +%s%N | cut -b1-13)
a_day_in_ms=86400000

for i in {1..20}; do
  ((first_ts=now-i*a_day_in_ms))
  echo $first_ts

  curl -XPUT \
  "http://127.0.0.1:10018/buckets/to_be_deleted/keys/908eb9bd4473db38-test-$first_ts" \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: $first_ts" \
  -d "{\"ts\": $first_ts}"

  for j in {1..70}; do
    ((second_ts=now-j*a_day_in_ms))
    curl -XPUT \
    "http://127.0.0.1:10018/buckets/908eb9bd4473db38-test-$first_ts/keys/$second_ts" \
    -H "Content-Type: application/json" \
    -H "x-riak-index-created_at_int: $second_ts" \
    -d "{\"ts\": $second_ts}"
    
  done
done

curl -s http://127.0.0.1:10018/buckets/to_be_deleted/index/created_at_int/0/9999999999999999 | python -mjson.tool

##
