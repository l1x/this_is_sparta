this_is_sparta
==============
Compiling the Erlang client

Assuming you are on CentOS/RedHat:

Erlang
```  
 https://www.erlang-solutions.com/downloads/download-erlang-otp
 yum install esl-erlang-R15B03-2.x86_64
 Riak Client
```

Few dev libs might need to be installed
```
wget https://github.com/basho/riak-erlang-client/archive/1.4.2.zip
unzip 1.4.2.zip
cd riak-erlang-client-1.4.2/
make
```

Delete all the keys in a defined list of buckets in Riak

```bash
erlc this_is_sparta.erl && 
erl \
-noshell -K true -A 32 -W w -zdbbl 131072 -P 256000 \
-pa riak-erlang-client-1.4.2/ebin riak-erlang-client-1.4.2/deps/*/ebin \
-eval "this_is_sparta:delete_all_buckets()"
```

The list file is using the Erlang binary syntax for strings 

    <<string>>.
