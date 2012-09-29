#!/bin/sh
ulimit -n 1000000
exec erl -pa ebin edit deps/*/ebin  -boot start_sasl \
    -sname wsgame$1 \
    -s wsgame \
    -wsgame nodes [\'wsgame1@yourhost\',\'wsgame0@yourhost\'] \
    -wsgame port 808$1 \
    +A 128 +K true +P 1000000 

