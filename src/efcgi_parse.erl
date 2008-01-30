%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2006, 2007 Erlware
%%%
%%% Permission is hereby granted, free of charge, to any
%%% person obtaining a copy of this software and associated
%%% documentation files (the "Software"), to deal in the
%%% Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute,
%%% sublicense, and/or sell copies of the Software, and to permit
%%% persons to whom the Software is furnished to do so, subject to
%%% the following conditions:
%%%
%%% The above copyright notice and this permission notice shall
%%% be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%%% OTHER DEALINGS IN THE SOFTWARE.
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc
%%%  Parse and unparse fcgi protocols.
%%% @end
%%% @copyright 2006 Erlware
%%%---------------------------------------------------------------------------
-module(efcgi_parse).

-export([parse_pairs/1, write_record/4, end_stream/3]).

-include("efcgi.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec parse_pairs(Bin) -> NameValuePairs.
%%
%% @doc
%%  Parse the name value pairs out of the provided content binary.
%% @end
%%--------------------------------------------------------------------
parse_pairs(Bin) ->
    parse_pairs(Bin, []).

%%--------------------------------------------------------------------
%% @spec parse_pairs(Bin, Acc) -> NameValuePairs.
%%
%% @doc
%%  Parse the name value pairs out of the provided content binary.
%% @end
%%--------------------------------------------------------------------
parse_pairs(<<0:1, NameLen:7, 0:1, ValueLen:7, Rest/binary>>, Acc) ->
    <<Name:NameLen/binary, Value:ValueLen/binary, NRest/binary>> = Rest,
    parse_pairs(NRest, [{Name, Value} | Acc]);
parse_pairs(<<0:1, NameLen:7, 1:1, ValueLen:31, Rest/binary>>, Acc) ->
    <<Name:NameLen/binary, Value:ValueLen/binary, NRest/binary>> = Rest,
    parse_pairs(NRest, [{Name, Value} | Acc]);
parse_pairs(<<1:1, NameLen:31, 0:1, ValueLen:7, Rest/binary>>, Acc) ->
    <<Name:NameLen/binary, Value:ValueLen/binary, NRest/binary>> = Rest,
    parse_pairs(NRest, [{Name, Value} | Acc]);
parse_pairs(<<1:1, NameLen:31, 1:1, ValueLen:31, Rest/binary>>, Acc) ->
    <<Name:NameLen/binary, Value:ValueLen/binary, NRest/binary>> = Rest,
    parse_pairs(NRest, [{Name, Value} | Acc]);
parse_pairs(<< >>, Acc) ->
    Acc.


%%--------------------------------------------------------------------
%% @spec write_record(State, Type, RecordId, Content) -> ok.
%%
%% @doc
%%  Encode a record given the type information provided
%% @end
%%--------------------------------------------------------------------
write_record(Sock, Type, RecordId, Content) when is_binary(Content)->
    Len = size(Content),
    case gen_tcp:send(Sock, <<?FCGI_VERSION_1:8, Type:8,
                             RecordId:16,
                             Len:16, 0:8, 0:8, Content/binary>>) of
        ok ->
            ok;
        Else ->
            exit(Else)
    end;
write_record(Sock, Type, RecordId, Content)
  when is_list(Content) ->
    Body = create_body(Content, []),
    write_record(Sock, Type, RecordId, Body).

%%--------------------------------------------------------------------
%% @spec end_stream(Sock, Type, RecordId)-> ok.
%%
%% @doc
%%  End a stream record with an empty body.
%% @end
%%--------------------------------------------------------------------
end_stream(Sock, Type, RequestId)->
    case gen_tcp:send(Sock, <<?FCGI_VERSION_1:8, Type:8,
                             RequestId:16,
                             0:16, 0:8, 0:8>>) of
        ok ->
            ok;
        Else ->
            exit(Else)
    end.


%%--------------------------------------------------------------------
%% @spec create_body([{Name, Value} | T], Acc) -> Binary.
%%
%% @doc
%%  Encode each pair into its proper fastcgi encoding.
%% @end
%%--------------------------------------------------------------------
create_body([{Name, Value} | T], Acc)
  when length(Name) =< 127, length(Value) >= 127 ->
    NameLen = length(Name),
    ValueLen = length(Value),
    create_body(T, [<<0:1, NameLen:7, 0:1, ValueLen:7, Name, Value>> | Acc]);
create_body([{Name, Value} | T], Acc)
  when length(Name) =< 127, length(Value) >= 127 ->
    NameLen = length(Name),
    ValueLen = length(Value),
    create_body(T, [<<0:1, NameLen:7, 1:1, ValueLen:31, Name, Value>> | Acc]);
create_body([{Name, Value} | T], Acc)
  when length(Name) >= 127, length(Value) =< 127 ->
    NameLen = length(Name),
    ValueLen = length(Value),
    create_body(T, [<<1:1, NameLen:31, 0:1, ValueLen:7, Name, Value>> | Acc]);
create_body([{Name, Value} | T], Acc)
  when length(Name) >= 127, length(Value) >= 127 ->
    NameLen = length(Name),
    ValueLen = length(Value),
    create_body(T, [<<1:1, NameLen:31, 1:1, ValueLen:31, Name, Value>> | Acc]);
create_body([], Acc) ->
    list_to_binary(Acc).

%%====================================================================
%% Internal functions
%%====================================================================

