%%%-------------------------------------------------------------------
%%% Copyright 2006 Eric Merritt
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");  
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%       http://www.apache.org/licenses/LICENSE-2.0
%%%
%%%  Unless required by applicable law or agreed to in writing, software
%%%  distributed under the License is distributed on an "AS IS" BASIS,
%%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or 
%%%  implied. See the License for the specific language governing 
%%%  permissions and limitations under the License.
%%%
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc 
%%%  Parse and unparse fcgi protocols.
%%% @end
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

