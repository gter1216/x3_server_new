#!/bin/sh

# ----------------------------------------------------------
# Script to dump msg statistic data
#
# Created by Xu Xiao, July 17 2017
# ----------------------------------------------------------

# sub function
usage(){
    echo "usage: "
    echo "   ./msg_dump start"
    echo "   ./msg_dump stop"
    echo "   ./msg_dump check"
}

# main function

# input parameter check
if [ $# -ne 1 ]
then
    usage
    exit 1
fi

case "$1" in
    "start")
        erl -noshell -s command start_msg_dump &
        ;;
    "stop")
        erl -noshell -s command stop_msg_dump &
        ;;
    "check")
        erl -noshell -s command check_msg_dump &
        ;;
    "show")
        erl -noshell -s command show_msg_dump &
        ;;
    *)
        usage
        exit 1
        ;;
esac




