#!/bin/sh

# ----------------------------------------------------------
# Script to start a X3 server
#
# Created by Xu Xiao, July 14 2017
# ----------------------------------------------------------

# sub function
usage(){
    echo "usage: "
    echo "   ./x3_server start   :"
    echo "        start a x3 server with configuration defined in config.txt"
    echo "   ./x3_server stop    :" 
    echo "        stop a x3 server"
    echo "   ./x3_server stop all :"
    echo "        kill all the Erlang VM process belongs to the user"
}


# main function

# input parameter check
if [ $# -gt 2 ]  || [ $# -eq 0 ]
then
    usage
    exit 1
fi


case "$1" in
    "start")
        if [ "$2" = debug ]
        then
           sleep 1s
           #cd ../src && erlc *.erl && cd -
           erl -s x3_server start -boot start_sasl -config elog.config &
        else
           if [ $# -gt 1 ]
           then
               usage
           elif [ $# -eq 1 ]
           then
               sleep 1s
               #cd ../src && erlc *.erl && cd -
               erl -noshell -s x3_server start -boot start_sasl -config elog.config &
           else
               usage
           fi
        fi
        ;;
    "stop")
        if [ $# -eq 1 ]
        then
            erl -noshell -s command stop_server &
            #ps -efww | grep -w 'beam.smp' | grep -v grep | cut -c 9-15 | xargs kill -9
        else
            if [ "$2" = all ]
            then
                if [ `whoami` = "root" ];then  
                    echo "Reject! root account is not allow to run this script"  
                else  
                    echo "Kill all erlang vm process under current user!"
                    ps -efww | grep -w 'beam.smp' | grep -v grep | grep `whoami` | cut -c 9-15 | xargs kill -9
                fi  
            else
               usage
            fi
        fi
        ;;
    #"restart")
    #    erl -noshell -s command stop_server &
    #    #ps -efww | grep -w 'beam.smp' | grep -v grep | cut -c 9-15 | xargs kill -9
    #    if [ $# -eq 1 ]
    #    then
    #        sleep 2s
    #        #cd ../src && erlc *.erl && cd -
    #        erl -noshell -s x3_server start &
    #    else
    #        if [ "$2" = debug ]
    #        then
    #            sleep 2s
    #            #cd ../src && erlc *.erl && cd -
    #            erl -s x3_server start
    #        else
    #            usage
    #        fi
    #    fi
    #    ;;
    *)
        usage
        exit 1
        ;;
esac


