
curl -v -XPUT \
  http://127.0.0.1:10018/buckets/to_be_deleted/keys/908eb9bd4473db38-test-1407449017112 \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: 1407213871425" \
  -d '{"ts":1407213871425}'

curl -v -XPUT \
  http://127.0.0.1:10018/buckets/to_be_deleted/keys/908eb9bd4473db38-test-1406174465901 \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: 1407223871425" \
  -d '{"ts":1407223871425}'

curl -v -XPUT \
  http://127.0.0.1:10018/buckets/to_be_deleted/keys/908eb9bd4473db38-test-1407450065843 \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: 1407323871425" \
  -d '{"ts":1407323871425}'

curl -v -XPUT \
  http://127.0.0.1:10018/buckets/to_be_deleted/keys/908eb9bd4473db38-test-1408451096759 \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: 1407465976425" \
  -d '{"ts":1407465976425}'

curl -s http://127.0.0.1:10018/buckets/to_be_deleted/index/created_at_int/0/9999999999999999 | python -mjson.tool

##

curl -v -XPUT \
  http://127.0.0.1:10018/buckets/908eb9bd4473db38-test-1407449017112/keys/1407465976425 \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: 1407465976425" \
  -d '{"ts":1407465976425}'

curl -v -XPUT \
  http://127.0.0.1:10018/buckets/908eb9bd4473db38-test-1407449017112/keys/1407223871425 \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: 1407223871425" \
  -d '{"ts":1407223871425}'

curl -v -XPUT \
  http://127.0.0.1:10018/buckets/908eb9bd4473db38-test-1407449017112/keys/1407323871425 \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: 1407323871425" \
  -d '{"ts":1407323871425}'

curl -v -XPUT \
  http://127.0.0.1:10018/buckets/908eb9bd4473db38-test-1407449017112/keys/1407465976425 \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: 1407465976425" \
  -d '{"ts":1407465976425}'



curl -v -XPUT \
  http://127.0.0.1:10018/buckets/908eb9bd4473db38-test-1406174465901/keys/1407465976425 \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: 1407465976425" \
  -d '{"ts":1407465976425}'

curl -v -XPUT \
  http://127.0.0.1:10018/buckets/908eb9bd4473db38-test-1406174465901/keys/1407223871425 \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: 1407223871425" \
  -d '{"ts":1407223871425}'

curl -v -XPUT \
  http://127.0.0.1:10018/buckets/908eb9bd4473db38-test-1406174465901/keys/1407323871425 \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: 1407323871425" \
  -d '{"ts":1407323871425}'

curl -v -XPUT \
  http://127.0.0.1:10018/buckets/908eb9bd4473db38-test-1406174465901/keys/1407465976425 \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: 1407465976425" \
  -d '{"ts":1407465976425}'

# 

curl -v -XPUT \
  http://127.0.0.1:10018/buckets/908eb9bd4473db38-test-1407450065843/keys/1407465976425 \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: 1407465976425" \
  -d '{"ts":1407465976425}'

curl -v -XPUT \
  http://127.0.0.1:10018/buckets/908eb9bd4473db38-test-1407450065843/keys/1407223871425 \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: 1407223871425" \
  -d '{"ts":1407223871425}'

curl -v -XPUT \
  http://127.0.0.1:10018/buckets/908eb9bd4473db38-test-1407450065843/keys/1407323871425 \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: 1407323871425" \
  -d '{"ts":1407323871425}'

curl -v -XPUT \
  http://127.0.0.1:10018/buckets/908eb9bd4473db38-test-1407450065843/keys/1407465976425 \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: 1407465976425" \
  -d '{"ts":1407465976425}'

#

curl -v -XPUT \
  http://127.0.0.1:10018/buckets/908eb9bd4473db38-test-1408451096759/keys/1407465976425 \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: 1407465976425" \
  -d '{"ts":1407465976425}'

curl -v -XPUT \
  http://127.0.0.1:10018/buckets/908eb9bd4473db38-test-1408451096759/keys/1407223871425 \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: 1407223871425" \
  -d '{"ts":1407223871425}'

curl -v -XPUT \
  http://127.0.0.1:10018/buckets/908eb9bd4473db38-test-1408451096759/keys/1407323871425 \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: 1407323871425" \
  -d '{"ts":1407323871425}'

curl -v -XPUT \
  http://127.0.0.1:10018/buckets/908eb9bd4473db38-test-1408451096759/keys/1407465976425 \
  -H "Content-Type: application/json" \
  -H "x-riak-index-created_at_int: 1407465976425" \
  -d '{"ts":1407465976425}'






