#!/bin/sh

# ----------------------------------------------------------
# Script to dump msg statistic data
#
# Created by Xu Xiao, July 17 2017
# ----------------------------------------------------------

# sub function
usage(){
    echo "usage: "
    echo "   ./msg_dump "
}

# main function

# input parameter check
if [ $# -ne 0 ]
then
    usage
    exit 1
fi


cd ../src && erlc *.erl && cd -
erl -noshell -s msg_dump start &

