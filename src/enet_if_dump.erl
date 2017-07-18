%% @author xxu <xiao.a.xu@alcatel-sbell.com.cn>
%% @copyright 2017 Nokia, Inc
-module(enet_if_dump).

%% API
-export([hexblock/1]).

hexblock(Bin) ->
    FullLineBytes = (byte_size(Bin) div 16) * 16,
    <<FullLines:FullLineBytes/binary, LastLine/binary>> = Bin,
    Lines = [Line || <<Line:16/binary>> <= FullLines ],
    NumberedLines = lists:zip(lists:seq(0, FullLineBytes-1, 16), Lines),
    [ [hexblock_line(Offset, Line) || {Offset, Line} <- NumberedLines],
     hexblock_lastline(FullLineBytes, LastLine)].

hexblock_line(Offset, Line) ->
    %"0x0000:  4500 0045 0000 0000 4001 f564 c0a8 0202  E..E....@..d...."
    Groups = [ io_lib:format("~4.16.0b", [Group]) || << Group:16 >> <= Line ],
    JGroups = string:join(Groups, " "),
    io_lib:format("0x~4.16.0b:  ~s ~s~n", [Offset, JGroups,
                                           to_printable(Line)]).

hexblock_lastline(Offset, Line) ->
    Size = byte_size(Line),
    FullGroupSize = (Size div 2) * 2,
    LastGroupSize = (Size - FullGroupSize) * 8,
    <<FullGroup:FullGroupSize/binary, LastGroup:LastGroupSize>> = Line,
    Groups = [ io_lib:format("~4.16.0b", [Group])
               || << Group:16 >> <= FullGroup ],
    JGroups = string:join(Groups, " "),
    case LastGroupSize of
        0 ->
            io_lib:format("0x~4.16.0b:  ~s ~s~n", [Offset, JGroups,
                                                  to_printable(Line)]);
        _ ->
            Hex = [JGroups, " ",
                   erlang:integer_to_list(LastGroup, 16)],
            %% Full line would be 8 groups of 4 hex digits with 7
            %% spaces interspersed (4*8)+7
            Padding = binary:copy(<<" ">>, (4*8 + 7) - iolist_size(Hex)),
            io_lib:format("0x~4.16.0b:  ~s ~s~n",
                          [Offset, Hex,
                           [Padding,
                           to_printable(Line)]])
    end.

to_printable(Bin) when is_binary(Bin) ->
    [ case C of
          Printable when 16#20 =< C, C =< 16#7E ->
              Printable;
          _NonPrintable ->
              $.
      end
       || <<C>> <= Bin ].
