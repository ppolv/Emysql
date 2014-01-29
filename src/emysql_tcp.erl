%% Copyright (c) 2009-2011
%% Bill Warnecke <bill@rupture.com>,
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>,
%% Henning Diedrich <hd2010@eonblast.com>,
%% Eonblast Corporation <http://www.eonblast.com>
%%
%% Permission is  hereby  granted,  free of charge,  to any person
%% obtaining  a copy of this software and associated documentation
%% files (the "Software"),to deal in the Software without restric-
%% tion,  including  without  limitation the rights to use,  copy,
%% modify, merge,  publish,  distribute,  sublicense,  and/or sell
%% copies  of the  Software,  and to  permit  persons to  whom the
%% Software  is  furnished  to do  so,  subject  to the  following
%% conditions:
%%
%% The above  copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF  MERCHANTABILITY,  FITNESS  FOR  A  PARTICULAR  PURPOSE  AND
%% NONINFRINGEMENT. IN  NO  EVENT  SHALL  THE AUTHORS OR COPYRIGHT
%% HOLDERS  BE  LIABLE FOR  ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT,  TORT  OR OTHERWISE,  ARISING
%% FROM,  OUT OF OR IN CONNECTION WITH THE SOFTWARE  OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.

-module(emysql_tcp).
-export([send_and_recv_packet/3, recv_packet/3, response/4]).

-include("emysql.hrl").

-define(PACKETSIZE, 1460).

send_and_recv_packet(Sock, Packet, SeqNum) ->
    %-% io:format("~nsend_and_receive_packet: SEND SeqNum: ~p, Binary: ~p~n", [SeqNum, <<(size(Packet)):24/little, SeqNum:8, Packet/binary>>]),
    %-% io:format("~p send_and_recv_packet: send~n", [self()]),
    case gen_tcp:send(Sock, <<(size(Packet)):24/little, SeqNum:8, Packet/binary>>) of
        ok ->
            %-% io:format("~p send_and_recv_packet: send ok~n", [self()]),
            ok;
        {error, Reason} ->
            %-% io:format("~p send_and_recv_packet: ERROR ~p -> EXIT~n", [self(), Reason]),
            exit({failed_to_send_packet_to_server, Reason})
    end,
    %-% io:format("~p send_and_recv_packet: resonse_list~n", [self()]),
    DefaultTimeout = emysql_app:default_timeout(),
    case response_list(Sock, DefaultTimeout, ?SERVER_MORE_RESULTS_EXIST) of
        % This is a bit murky. It's compatible with former Emysql versions
        % but sometimes returns a list, e.g. for stored procedures,
        % since an extra OK package is sent at the end of their results.
        [Record | []] ->
            %-% io:format("~p send_and_recv_packet: record~n", [self()]),
            Record;
        List ->
            %-% io:format("~p send_and_recv_packet: list~n", [self()]),
            List
    end.

response_list(Sock, DefaultTimeout, ServerStatus) -> 
    response_list(Sock, DefaultTimeout, ServerStatus, <<>>).

response_list(_, _DefaultTimeout, 0, <<>>) -> [];  %%no further data received after last response.

response_list(Sock, DefaultTimeout, ?SERVER_MORE_RESULTS_EXIST, Buff) ->
    {Packet, Rest} = recv_packet(Sock, DefaultTimeout, Buff),
    {Response, ServerStatus, Rest2} = response(Sock, DefaultTimeout, Packet, Rest),
    [ Response | response_list(Sock, DefaultTimeout, ServerStatus band ?SERVER_MORE_RESULTS_EXIST, Rest2)].



recv_packet(Sock, DefaultTimeout, Buff) ->
    %-% io:format("~p recv_packet~n", [self()]),
    %-% io:format("~p recv_packet: recv_packet_header~n", [self()]),
    {PacketLength, SeqNum, Buff2} = recv_packet_header(Sock, DefaultTimeout, Buff),
    %-% io:format("~p recv_packet: recv_packet_body~n", [self()]),
    {Data, Rest} = recv_packet_body(Sock, PacketLength, DefaultTimeout, Buff2),
    %-% io:format("~nrecv_packet: len: ~p, data: ~p~n", [PacketLength, Data]),
    {#packet{size=PacketLength, seq_num=SeqNum, data=Data}, Rest}.


response(_Sock, _Timeout, #packet{seq_num = SeqNum, data = <<0:8, Rest/binary>>}=_Packet, Buff) ->
    %-% io:format("~nresponse (OK): ~p~n", [_Packet]),
    {AffectedRows, Rest1} = emysql_util:length_coded_binary(Rest),
    {InsertId, Rest2} = emysql_util:length_coded_binary(Rest1),
    <<ServerStatus:16/little, WarningCount:16/little, Msg/binary>> = Rest2, % (*)!
    %-% io:format("- warnings: ~p~n", [WarningCount]),
    %-% io:format("- server status: ~p~n", [emysql_conn:hstate(ServerStatus)]),
    { #ok_packet{
        seq_num = SeqNum,
        affected_rows = AffectedRows,
        insert_id = InsertId,
        status = ServerStatus,
        warning_count = WarningCount,
        msg = unicode:characters_to_list(Msg) },
      ServerStatus, Buff };

% EOF: MySQL format <= 4.0, single byte. See -2-
response(_Sock, _Timeout, #packet{seq_num = SeqNum, data = <<?RESP_EOF:8>>}=_Packet, Buff) ->
    %-% io:format("~nresponse (EOF v 4.0): ~p~n", [_Packet]),
    { #eof_packet{
        seq_num = SeqNum },
      ?SERVER_NO_STATUS, Buff };

% EOF: MySQL format >= 4.1, with warnings and status. See -2-
response(_Sock, _Timeout, #packet{seq_num = SeqNum, data = <<?RESP_EOF:8, WarningCount:16/little, ServerStatus:16/little>>}=_Packet, Buff) -> % (*)!
    %-% io:format("~nresponse (EOF v 4.1), Warn Count: ~p, Status ~p, Raw: ~p~n", [WarningCount, ServerStatus, _Packet]),
    %-% io:format("- warnings: ~p~n", [WarningCount]),
    %-% io:format("- server status: ~p~n", [emysql_conn:hstate(ServerStatus)]),
    { #eof_packet{
        seq_num = SeqNum,
        status = ServerStatus,
        warning_count = WarningCount },
      ServerStatus, Buff };

% ERROR response: MySQL format >= 4.1. See -3-
response(_Sock, _Timeout, #packet{seq_num = SeqNum, data = <<255:8, ErrNo:16/little, "#", SQLState:5/binary-unit:8, Msg/binary>>}=_Packet, Buff) ->
    %-% io:format("~nresponse (Response is ERROR): SeqNum: ~p, Packet: ~p~n", [SeqNum, _Packet]),
    { #error_packet{
        seq_num = SeqNum,
        code = ErrNo,
        status = SQLState,
        msg = binary_to_list(Msg) }, % todo: test and possibly conversion to UTF-8
     ?SERVER_NO_STATUS, Buff };

% ERROR response: MySQL format <= 4.0. See -3-
response(_Sock, _Timeout, #packet{seq_num = SeqNum, data = <<255:8, ErrNo:16/little, Msg/binary>>}=_Packet, Buff) ->
    %-% io:format("~nresponse (Response is ERROR): SeqNum: ~p, Packet: ~p~n", [SeqNum, _Packet]),
    { #error_packet{
        seq_num = SeqNum,
        code = ErrNo,
        status = 0,
        msg = binary_to_list(Msg) }, % todo: test and possibly conversion to UTF-8
     ?SERVER_NO_STATUS, Buff };

% DATA response.
response(Sock, DefaultTimeout, #packet{seq_num = SeqNum, data = Data}=_Packet, Buff) ->
    %-% io:format("~nresponse (DATA): ~p~n", [_Packet]),
    {FieldCount, Rest1} = emysql_util:length_coded_binary(Data),
    {Extra, _} = emysql_util:length_coded_binary(Rest1),

    {SeqNum1, FieldList, Buff2} = recv_field_list(Sock, SeqNum+1, DefaultTimeout, Buff),
    if
        length(FieldList) =/= FieldCount ->
            exit(query_returned_incorrect_field_count);
        true ->
            ok
    end,
    FieldList2 = [Field#field.type || Field <- FieldList],
    {SeqNum2, Rows, ServerStatus, Buff3} = recv_row_data(Sock, FieldList2, DefaultTimeout, SeqNum1+1, Buff2),

    { #result_packet{
        seq_num = SeqNum2,
        field_list = FieldList,
        rows = lists:reverse(Rows),
        extra = Extra },
      ServerStatus, Buff3 }.

recv_packet_header(_Sock, _Timeout, <<PacketLength:24/little-integer, SeqNum:8/integer, Rest/binary>>) ->
        {PacketLength, SeqNum, Rest};
recv_packet_header(Sock, Timeout, Buff) when erlang:byte_size(Buff) < 4 ->
    case gen_tcp:recv(Sock, 0, Timeout) of
        {ok, Data} ->
            recv_packet_header(Sock, Timeout, <<Buff/binary, Data/binary>>);
        {error, Reason} ->
            exit({failed_to_recv_packet_header, Reason})
    end;
recv_packet_header(_Sock, _Timeout, Buff) ->
    exit({bad_packet_header_data, Buff}).

recv_packet_body(Sock, PacketLength, Timeout, Buff) ->
    case Buff of
        <<Bin:PacketLength/binary, Rest/binary>> ->
            {Bin, Rest};
        _ when erlang:byte_size(Buff) < PacketLength ->
            case gen_tcp:recv(Sock, 0, Timeout) of
                    {ok, Bin} ->
                        recv_packet_body(Sock, PacketLength , Timeout, <<Buff/binary, Bin/binary>>);
                    {error, Reason1} ->
                        exit({failed_to_recv_packet_body, Reason1})
            end
    end.

recv_field_list(Sock, SeqNum, DefaultTimeout, Buff) ->
    recv_field_list(Sock, SeqNum, DefaultTimeout,[], Buff).

recv_field_list(Sock, _SeqNum, DefaultTimeout, _Acc, Buff) ->
    {SeqNum, Columns, _ServerStatus, Buff2} = recv_row_data(Sock, [
        ?FIELD_TYPE_VAR_STRING, 
        ?FIELD_TYPE_VAR_STRING,
        ?FIELD_TYPE_VAR_STRING, 
        ?FIELD_TYPE_VAR_STRING, 
        ?FIELD_TYPE_VAR_STRING, 
        ?FIELD_TYPE_VAR_STRING,
        ?FIELD_TYPE_FIELD_EXTRA
    ], DefaultTimeout, 0, Buff),
    {SeqNum, get_field_list(Columns, SeqNum), Buff2}.

get_field_list(Columns, Seqnum) ->
    get_field_list(Columns, Seqnum, []).

get_field_list([[Catalog, Db, Table, OrgTable, Name, OrgName, {CharSetNr, Length, Type, Flags, Decimals, Default}] | Rest], Seqnum, Acc) ->
    get_field_list(Rest, Seqnum - 1,[
        #field{
            seq_num = Seqnum - 1, 
            catalog = Catalog, 
            db = Db, 
            table = Table, 
            org_table = OrgTable, 
            name = Name, 
            org_name = OrgName, 
            type = Type, 
            default = case Default of undefined -> <<>>; _ -> Default end, 
            charset_nr = CharSetNr, 
            length = Length, 
            flags = Flags, 
            decimals = Decimals
        } | Acc]);
get_field_list([], _Seqnum, Acc) ->
    Acc.
           

recv_row_data(Socket, FieldList, DefaultTimeout, SeqNum, Buff) ->
    recv_row_data(Socket, FieldList, DefaultTimeout, SeqNum, Buff, []).

recv_row_data(Socket, FieldList, Timeout, SeqNum, Buff, Acc) ->
    case parse_buffer(FieldList,Buff, Acc) of
            {ok, NotParsed, NewAcc} ->
                case gen_tcp:recv(Socket, 0, Timeout) of
                    {ok, Data} ->
                        recv_row_data(Socket, FieldList, Timeout, SeqNum+1,  <<NotParsed/binary, Data/binary>>, NewAcc);
                    {error, Reason} ->
                        exit({failed_to_recv_row, Reason})
                end;
            {eof, Seq, NewAcc, ServerStatus, NotParsed} ->
                {Seq, NewAcc, ServerStatus, NotParsed}
    end.

parse_buffer(FieldList, Buff, Acc) ->
    case emysql_parse:parse_buffer(Buff, FieldList, Acc) of
        {eof, ServerStatus, SeqNum, Acc2, Rest} ->
            {eof, SeqNum, Acc2, ServerStatus, Rest};
        {eof, SeqNum, Acc2, Rest} ->
            {eof, SeqNum, Acc2, ?SERVER_NO_STATUS, Rest};
        {incomplete, Acc2, Rest} ->
            case Rest of
                empty ->
                    {ok, <<"">>, Acc2};
                _     ->
                    {ok, Rest, Acc2}
            end;
        A ->
            io:format("~w~n",[A])
    end.



% TODO: [http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol#COM_QUERY]
% field_count:          The value is always 0xfe (decimal ?RESP_EOF).
%                       However ... recall (from the
%                       section "Elements", above) that the value ?RESP_EOF can begin
%                       a Length-Encoded-Binary value which contains an 8-byte
%                       integer. So, to ensure that a packet is really an EOF
%                       Packet: (a) check that first byte in packet = 0xfe, (b)
%                       check that size of packet smaller than 9.


% This was used to approach a solution for proper handling of SERVER_MORE_RESULTS_EXIST
%
% recv_rest(Sock) ->
%   %-% io:format("~nrecv_rest: ", []),
%   case recv_packet_header_if_present(Sock) of
%       {PacketLength, SeqNum} ->
%           %-% io:format("recv_packet ('rest'): len: ~p, seq#: ~p ", [PacketLength, SeqNum]),
%           Data = recv_packet_body(Sock, PacketLength),
%           %-% io:format("data: ~p~n", [Data]),
%           Packet = #packet{size=PacketLength, seq_num=SeqNum, data=Data},
%           response(Sock, Packet);
%       none ->
%           %-% io:format("nothing~n", []),
%           nothing
%   end.


% -------------------------------------------------------------------------------
% Note: (*) The order of status and warnings count reversed for eof vs. ok packet.
% -------------------------------------------------------------------------------

% -----------------------------------------------------------------------------1-
% OK packet format
% -------------------------------------------------------------------------------
%
%  VERSION 4.0
%  Bytes                       Name
%  -----                       ----
%  1   (Length Coded Binary)   field_count, always = 0
%  1-9 (Length Coded Binary)   affected_rows
%  1-9 (Length Coded Binary)   insert_id
%  2                           server_status
%  n   (until end of packet)   message
%
%  VERSION 4.1
%  Bytes                       Name
%  -----                       ----
%  1   (Length Coded Binary)   field_count, always = 0
%  1-9 (Length Coded Binary)   affected_rows
%  1-9 (Length Coded Binary)   insert_id
%  2                           server_status
%  2                           warning_count
%  n   (until end of packet)   message
%
%  field_count:     always = 0
%
%  affected_rows:   = number of rows affected by INSERT/UPDATE/DELETE
%
%  insert_id:       If the statement generated any AUTO_INCREMENT number,
%                   it is returned here. Otherwise this field contains 0.
%                   Note: when using for example a multiple row INSERT the
%                   insert_id will be from the first row inserted, not from
%                   last.
%
%  server_status:   = The client can use this to check if the
%                   command was inside a transaction.
%
%  warning_count:   number of warnings
%
%  message:         For example, after a multi-line INSERT, message might be
%                   "Records: 3 Duplicates: 0 Warnings: 0"
%
% Source: http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol

% -----------------------------------------------------------------------------2-
% EOF packet format
% -------------------------------------------------------------------------------
%
%  VERSION 4.0
%  Bytes                 Name
%  -----                 ----
%  1                     field_count, always = 0xfe
%
%  VERSION 4.1
%  Bytes                 Name
%  -----                 ----
%  1                     field_count, always = 0xfe
%  2                     warning_count
%  2                     Status Flags
%
%  field_count:          The value is always 0xfe (decimal 254).
%                        However ... recall (from the
%                        section "Elements", above) that the value 254 can begin
%                        a Length-Encoded-Binary value which contains an 8-byte
%                        integer. So, to ensure that a packet is really an EOF
%                        Packet: (a) check that first byte in packet = 0xfe, (b)
%                        check that size of packet smaller than 9.
%
%  warning_count:        Number of warnings. Sent after all data has been sent
%                        to the client.
%
%  server_status:        Contains flags like SERVER_MORE_RESULTS_EXISTS
%
% Source: http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol

% -----------------------------------------------------------------------------3-
% Error packet format
% -------------------------------------------------------------------------------
%
%  VERSION 4.0
%  Bytes                       Name
%  -----                       ----
%  1                           field_count, always = 0xff
%  2                           errno (little endian)
%  n                           message
%
%  VERSION 4.1
%  Bytes                       Name
%  -----                       ----
%  1                           field_count, always = 0xff
%  2                           errno
%  1                           (sqlstate marker), always '#'
%  5                           sqlstate (5 characters)
%  n                           message
%
%  field_count:       Always 0xff (255 decimal).
%
%  errno:             The possible values are listed in the manual, and in
%                     the MySQL source code file /include/mysqld_error.h.
%
%  sqlstate marker:   This is always '#'. It is necessary for distinguishing
%                     version-4.1 messages.
%
%  sqlstate:          The server translates errno values to sqlstate values
%                     with a function named mysql_errno_to_sqlstate(). The
%                     possible values are listed in the manual, and in the
%                     MySQL source code file /include/sql_state.h.
%
%  message:           The error message is a string which ends at the end of
%                     the packet, that is, its length can be determined from
%                     the packet header. The MySQL client (in the my_net_read()
%                     function) always adds '\0' to a packet, so the message
%                     may appear to be a Null-Terminated String.
%                     Expect the message to be between 0 and 512 bytes long.
%
% Source: http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol
