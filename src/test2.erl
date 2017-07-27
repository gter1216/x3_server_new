

-module(test2).


-export([start/0]).


start() ->
	
	BinPack = <<170,0,0,46,0,46,
				48,44,
				128,1,
				2,161,
				39,160,
				37,
                128,2,
				0,213,
				129,6,
				80,71,
                87,49,
				54,56,
				130,19,
				99,
                115,99,
				102,45,50,48,49,
                55,48,54,50,55,49,53,50,
                50,52,53,131,2,39,172,
				
                170,0,0,46,0,46,48,44,
                128,1,2,161,39,160,37,
                128,2,0,213,129,6,80,71,
                87,49,54,56,130,19,99,
                115,99,102,45,50,48,49,
                55,48,54,50,55,49,53,50,
                50,52,53,131,2,39,172,
                170,0,0,46,0,46,48,44,
                128,1,2,161,39,160,37,
                128,2,0,213,129,6,80,71,
                87,49,54,56,130,19,99,
                115,99,102,45,50,48,49,
                55,48,54,50,55,49,53,50,
                50,52,53,131,2,39,172>>,

	List = un_smush(BinPack),

	io:format("~p~n", [List]),
	
    put(pid_name, 111),
	
	_Info = io_lib:format("Worker ~w received create lict req msg ~p", [get(pid_name), <<1,2,3>>]),
	
	_Str1 = get_time_micro(erlang:timestamp()),		
	Str2 = get_time_sec(erlang:timestamp()),
		
	%% Dir ===> "/home/xxu/project/x3_project/src"
	{ok, Dir} = file:get_cwd(),
	
	io:format("current dir is ~p~n", [Dir]),
	
	{ok, Des} = file:open(Dir++"/"++Str2++".log", [write]),
	
	io:format(Des,"laaaaaaaaaaaa!~n",[]),
	
	file:close(Des),
	
	ok.


get_time_micro({_,_,Micro} = Timestamp) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(Timestamp),
	
	%% "2017-07-25T08:12:51.885Z"
	lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~3..0sZ",
								[Year, Month, Day, Hour, Minute, Second, integer_to_list(Micro)])).


get_time_sec(Timestamp) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(Timestamp),
	
	%% "20170725081251"
	lists:flatten(io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w",
								[Year, Month, Day, Hour, Minute, Second])).

%% ==================================================================
%% un_smush(BinPack)
%%
%% input: 
%%
%% ouput:
%%
%% Result is:
%%
%% example:
%%
%% ==================================================================
un_smush(BinPack) ->
	
	decode_tcp_msg(BinPack, []).


decode_tcp_msg(<<>>, Acc) ->
	
	Acc;

decode_tcp_msg(BinPack, Acc) ->
	
	<<16#aa, 16#00, Len:16, Len:16, TotalBin/binary>> = BinPack,
	
	Length =  Len*8,
	
	<<Bin:Length, LeftBin/binary>> = TotalBin,
	
	decode_tcp_msg(LeftBin, Acc++[<<Bin:Length>>]).




