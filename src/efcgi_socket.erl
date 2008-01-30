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
%%%  Support for basic fastcgi protocol parsing.
%%% @end 
%%%----------------------------------------------------------------------------
-module(efcgi_socket).

-include("efcgi.hrl").

-export([start_link/5, init/5]).

-record(c,  {sock,
             port,
             peer_addr,
             peer_port,
             pid,
             max_conns,
             max_reqs}).

-record(request, {version, type, request_id,
                  content_length, reserved, 
                  content=[]}).

-define(SERVER_IDLE_TIMEOUT, 30*1000).


%% Set various FastCGI constants
%% Maximum number of requests that can be handled
-define(FCGI_DEFAULT_MAX_REQS, 1000).
-define(FCGI_DEFAULT_MAX_CONNS, 100).
-define(FCGI_DEFAULT_MPXS_CONNS, 1).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @spec start_link(ListingPid::pid(), ListenSocket::socket(), 
%%                  ListenPort::integer(), MaxConns::integer(), 
%%                  MaxReqs::integer()) -> ok.
%% 
%% @doc 
%%  Start the socket worker for the efcgi protocol parsing.
%% @end
%%--------------------------------------------------------------------
start_link(ListenPid, ListenSocket, ListenPort, Pid, Options) ->
    proc_lib:spawn_link(?MODULE, init, [ListenPid, ListenSocket, 
                                         ListenPort, Pid,
                                         Options]).

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init(ListenPid::pid(), ListenSocket::socket(), 
%%                  ListenPort::integer()) -> ok.
%% 
%% @doc 
%%  Initialize the worker, listen to the main socket and accept
%%  connections then go on to handle the connections.
%% @end
%%--------------------------------------------------------------------
init(ListenPid, ListenSocket, ListenPort, Pid, Options) ->
    MaxConns = get_option(max_conns, Options, ?FCGI_DEFAULT_MAX_CONNS), 
    MaxReqs = get_option(max_reqs, Options, ?FCGI_DEFAULT_MAX_REQS),
    case catch gen_tcp:accept(ListenSocket) of
	{ok, Socket} ->
            %% Send the cast message to the listener process to 
            %% create a new acceptor
	    efcgi_server:create(ListenPid, self()),
	    {ok, {Addr, Port}} = inet:peername(Socket),
            C = #c{sock = Socket,
                   port = ListenPort,
                   peer_addr = Addr,
                   peer_port = Port,
                   max_conns = MaxConns,
                   max_reqs = MaxReqs,
                   pid=Pid},
	    request(C); %% Jump to state 'request'
	Else ->
	    error_logger:error_report([{application, efcgi},
				       "Accept failed error",
				       io_lib:format("~p",[Else])]),
	    exit({error, accept_failed})
    end.


%%--------------------------------------------------------------------
%% @spec request(State) -> ok.
%% 
%% @doc 
%%  Start processing requests from the server. 
%% @end
%%--------------------------------------------------------------------
request(C = #c{sock=Sock}) ->
    case gen_tcp:recv(Sock, ?FCGI_HEADER_LEN, 30000) of
        {ok, <<Version:8, Type:8, RequestId:16, ContentLength:16,
              PaddingLength:8, Reserved:8>>} ->
            process_request_body(C, #request{version=Version,
                                             type=Type,
                                             request_id=RequestId,
                                             content_length=ContentLength,
                                             reserved=Reserved}, 
                                 PaddingLength);
        ExitReason = {error, _} ->
            exit(ExitReason)
    end,
    request(C).


%%--------------------------------------------------------------------
%% @spec process_request_body(State, Request)-> ok.
%% 
%% @doc 
%%  Process the body of the request record using the content and 
%%  padding length provided. Then forward request for further processing.
%% @end
%%--------------------------------------------------------------------
process_request_body(C, Request = #request{content_length = 0}, 0) ->
    handle_request(C, Request#request{content= <<>>});
process_request_body(C = #c{sock=Sock}, 
                     Request = #request{content_length=ContentLength},
                     PaddingLength) ->
    case gen_tcp:recv(Sock, ContentLength + PaddingLength, 30000) of
        {ok, <<Content:ContentLength/binary, _Padding/binary>>} ->
            NewRequest = Request#request{content=Content},
            handle_request(C, NewRequest);
        ExitReason = {error, _} ->
            exit(ExitReason)
    end.

%%--------------------------------------------------------------------
%% @spec handle_request(State, Request) -> ok.
%% 
%% @doc 
%%  Handle the request by spinning it off for further processing.
%% @end
%%--------------------------------------------------------------------
handle_request(C, Request = #request{request_id=?FCGI_NULL_REQUEST_ID}) ->
    management_request(C, Request);
handle_request(C, Request) ->
    application_request(C, Request).


%%--------------------------------------------------------------------
%% @spec management_request(State, Request) -> ok.
%% 
%% @doc 
%%  Handle discrete management requests. 
%% @end
%%--------------------------------------------------------------------
management_request(C, #request{type=?FCGI_GET_VALUES,
                               content=Content}) ->
    Pairs = efcgi_parse:parse_pairs(Content),
    Resp = generate_values_response(C, Pairs, []),
    efcgi_parse:write_record(C, ?FCGI_GET_VALUES_RESULT, 
                             ?FCGI_NULL_REQUEST_ID, Resp);
management_request(C, #request{type=Type}) ->
    Content = <<Type:8, 0:56>>,
    efcgi_parse:write_record(C, ?FCGI_UNKNOWN_TYPE, 
                             ?FCGI_NULL_REQUEST_ID, Content).

%%--------------------------------------------------------------------
%% @spec generate_values_response(State, Pairs, Acc) -> Bin
%% 
%% @doc 
%%  Generate the appropriate responses to each of the get value
%%  types.
%% @end
%%--------------------------------------------------------------------
generate_values_response(C = #c{max_conns=MaxConns}, 
                         [{<<?FCGI_MAX_CONNS>>, _} | T], Acc) ->
    generate_values_response(C, T, [{<<?FCGI_MAX_CONNS>>, <<MaxConns:32>>} 
                                  | Acc]);
generate_values_response(C = #c{max_reqs=MaxReqs}, 
                         [{<<?FCGI_MAX_REQS>>, _} | T], Acc) ->
    generate_values_response(C, T, [{<<?FCGI_MAX_REQS>>, <<MaxReqs:32>>} |
                                 Acc]);
generate_values_response(C, 
                         [{<<?FCGI_MPXS_CONNS>>, _} | T], Acc) ->
    generate_values_response(C, T, [{<<?FCGI_MPXS_CONNS>>, 
                                  <<1:8>>} | Acc]);
generate_values_response(_C, [], Acc) ->
    Acc.


%%--------------------------------------------------------------------
%% @spec application_request(State, Request) -> ok.
%% 
%% @doc 
%%  Handle stream based application requests.
%% @end
%%--------------------------------------------------------------------
application_request(#c{sock=Sock, pid=Pid}, 
                    Request = #request{type=?FCGI_BEGIN_REQUEST, 
                                       request_id=RequestId}) ->
    case Request#request.content of
        <<?FCGI_RESPONDER:16, Flags:8, _/binary>> ->
            start_handler(Sock, responder, Flags, Pid, RequestId);
        <<?FCGI_AUTHORIZER:16, Flags:8, _/binary>> ->
            start_handler(Sock, authorizer, Flags, Pid, RequestId);
        <<?FCGI_FILTER:16, Flags:8, _/binary>> ->
            start_handler(Sock, filter, Flags, Pid, RequestId);
        Resp ->
            exit({error, "Received invalid response", Resp})
    end;
application_request(_C, Request = #request{request_id=Id,
                                          type=?FCGI_PARAMS}) ->
    efcgi_request_proxy:params(Id, Request#request.content);
application_request(_C, Request = #request{request_id=Id,
                                          type=?FCGI_STDIN}) ->
    efcgi_request_proxy:stdin(Id, Request#request.content);
application_request(_C, #request{request_id=Id,
                                 type=?FCGI_ABORT_REQUEST}) ->
    efcgi_request_proxy:abort_request(Id);
application_request(_C, #request{type=UnknownType}) ->
    exit({error, {unknown_type, UnknownType}}).


%%--------------------------------------------------------------------
%% @spec start_handler(Type, Flags) -> Pid.
%% 
%% @doc 
%%  Start a handler for the new request.
%% @end
%%--------------------------------------------------------------------
start_handler(Sock, Type, Flags, Pid, RequestId) ->
    case Flags band ?FCGI_KEEP_CONN of
        0 ->
            efcgi_request_sup:start_requestor(Sock, 
                                              Pid,
                                              RequestId,
                                              Type,
                                              no_keep_alive);
        _ ->            
            efcgi_request_sup:start_requestor(Sock,
                                              Pid,
                                              RequestId,
                                              Type,
                                              keep_alive)
    end.

%%--------------------------------------------------------------------
%% @spec get_option(Key, ValList, Default) -> Value | Default.
%% 
%% @doc 
%%  Get the option from the list. If no option exits, return default.
%% @end
%%--------------------------------------------------------------------
get_option(Key, [{Key, Value} | _], _) ->
    Value;
get_option(Key, [ _H | T], Default) ->
    get_option(Key, T, Default);
get_option(_Key, [], Default) ->
    Default.
