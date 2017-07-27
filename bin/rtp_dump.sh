#!/bin/sh

# ----------------------------------------------------------
# Script to dump msg statistic data
#
# Created by Xu Xiao, July 17 2017
# ----------------------------------------------------------

# sub function
usage(){
    echo "usage: "
    echo "   ./rtp_dump start "
    echo "   ./rtp_dump stop "
    echo "   ./rtp_dump check"
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
        erl -noshell -s command start_rtp_dump &
        ;;
    "stop")
        erl -noshell -s command stop_rtp_dump &
        ;;
    "check")
        erl -noshell -s command check_rtp_dump &
        ;;
    *)
        usage
        exit 1
        ;;
esac

