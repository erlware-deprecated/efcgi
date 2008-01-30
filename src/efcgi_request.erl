%%%----------------------------------------------------------------------------
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
%%% 
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc 
%%%  Support for direct request handler. One of these should exist per
%%%  request id.
%%% @end
%%% @copyright (C) 2006, Eric Merritt
%%% Created : 29 Nov 2006 by Eric Merritt 
%%%----------------------------------------------------------------------------
-module(efcgi_request).

-behaviour(gen_server).

-include("efcgi.hrl").

-define(SERVER, ?MODULE).
-define(TIMEOUT, 2000).


%% API
-export([start_link/5,  stdout/2, stderr/2, close_stdout/1, 
         close_stderr/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {sock, request_id, 
                pid, 
                keep_alive,
                params=[]}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}.
%% 
%% @doc 
%% Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link(Sock,
           Pid,
           RequestId,
           Type,
           KeepAlive) ->
    gen_server:start_link(?MODULE, [Sock,
                                    Pid,
                                    RequestId,
                                    Type,
                                    KeepAlive], []).


%%--------------------------------------------------------------------
%% @spec stdout(RequestId, Data) -> ok.
%% 
%% @doc  
%%  Send stdout data to the server
%% @end
%%--------------------------------------------------------------------
stdout(RequestId, Data) ->
    efcgi_request_proxy:stdout(RequestId, Data).

%%--------------------------------------------------------------------
%% @spec close_stdout(RequestId) -> ok.
%% 
%% @doc 
%%  Close the stdout stream and end the request.
%% @end
%%--------------------------------------------------------------------
close_stdout(RequestId) ->
    efcgi_request_proxy:stdout(RequestId, end_of_stream).

%%--------------------------------------------------------------------
%% @spec stderr(RequestId, Data) -> ok.
%% 
%% @doc 
%%  Send data to stderr.
%% @end
%%--------------------------------------------------------------------
stderr(RequestId, Data) ->
    efcgi_request_proxy:stderr(RequestId, Data).

%%--------------------------------------------------------------------
%% @spec close_stderr(RequestId) -> ok.
%% 
%% @doc 
%%  Close down the stderr stream and end the request.
%% @end
%%--------------------------------------------------------------------
close_stderr(RequestId) ->
    efcgi_request_proxy:stderr(RequestId, end_of_stream).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}.
%% 
%% @doc 
%% Initiates the server
%% @end 
%%--------------------------------------------------------------------
init([Sock, Pid, RequestId, Type, KeepAlive]) ->
    efcgi_request_proxy:register(RequestId, self()),
    Pid ! {init, RequestId, Type},
    {ok, #state{sock=Sock, pid=Pid,
                request_id=RequestId,
                keep_alive=KeepAlive}, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}.
%% 
%% @doc 
%% Handling call messages
%% @end 
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}.
%% 
%% @doc 
%% Handling cast messages
%% @end 
%%--------------------------------------------------------------------
handle_cast({params, <<>>}, State=#state{params=Params, 
                                         pid=Pid,
                                         request_id = RequestId}) ->
    ActualParams = efcgi_parse:parse_pairs(list_to_binary(
                                             lists:reverse(Params))),
    Pid ! {params, RequestId, ActualParams},
    {noreply, State#state{params = <<>>}, ?TIMEOUT};
handle_cast({params, NewParams}, State=#state{params=Params}) ->
    {noreply, State#state{params=[NewParams | Params]}};
handle_cast({stdin, <<>>}, State=#state{pid=Pid, request_id = RequestId}) ->
    Pid ! {stdin, RequestId, end_of_stream},
    {noreply, State, ?TIMEOUT};
handle_cast({stdin, NewStdin}, State=#state{pid=Pid, request_id = RequestId}) ->
    Pid ! {stdin, RequestId, NewStdin},
    {noreply, State, ?TIMEOUT};
handle_cast(Action = {stdout, _}, State) ->
    handle_output(State, Action),
    {noreply, State, ?TIMEOUT};
handle_cast(Action = {stderr, _}, State) ->
    handle_output(State, Action),
    {noreply, State, ?TIMEOUT};
handle_cast(abort, State = #state{pid = Pid, request_id = RequestId}) ->
    Pid ! {abort, RequestId, http_server_abort},
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}.
%% 
%% @doc 
%% Handling all non call/cast messages
%% @end 
%%--------------------------------------------------------------------
handle_info(timeout, State = #state{pid = Pid, request_id = RequestId}) ->
    Pid ! {abort, RequestId, timeout},
    handle_request_end(State).


%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void().
%% 
%% @doc 
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, #state{request_id=RequestId}) ->
    efcgi_request_proxy:unregister(RequestId),
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}.
%% 
%% @doc 
%% Convert process state when code is changed
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%%% Internal functions
%%====================================================================

handle_output(State = #state{sock=Sock, request_id=RequestId}, 
              {stdout, end_of_stream}) ->
    efcgi_parse:end_stream(Sock, 
                           ?FCGI_STDOUT, 
                           RequestId),
    handle_request_end(State);
handle_output(State = #state{sock=Sock, request_id=RequestId}, 
              {stderr, end_of_stream}) ->
    efcgi_parse:end_stream(Sock, 
                           ?FCGI_STDERR, 
                           RequestId),
    handle_request_end(State);
handle_output(#state{sock=Sock,
                             request_id=RequestId}, 
              {stdout, Content}) ->
    efcgi_parse:write_record(Sock, 
                             ?FCGI_STDOUT, 
                             RequestId, convert(Content));
handle_output(#state{sock=Sock,
                             request_id=RequestId}, 
              {stderr, Content}) ->
    efcgi_parse:write_record(Sock, 
                             ?FCGI_STDERR, 
                             RequestId, convert(Content));
handle_output(_State, Something) ->
    exit({error, {unexpected_return, Something}}).


%%--------------------------------------------------------------------
%% @spec handle_request_end(State) -> ok.
%% 
%% @doc 
%%  Handle the end of the request. Close the socket if no_keep_alive.
%%  shut down with normal in either case.
%% @end
%%--------------------------------------------------------------------
handle_request_end(#state{sock=Sock, request_id=RequestId, 
                          keep_alive=no_keep_alive}) ->
    ok = efcgi_parse:write_record(Sock, ?FCGI_END_REQUEST, RequestId,
                                  <<0:32, ?FCGI_REQUEST_COMPLETE:8, 0:24>>),
    ok = gen_tcp:close(Sock),
    exit(normal);
handle_request_end(#state{sock=Sock, request_id=RequestId, 
                          keep_alive=keep_alive}) ->
    ok = efcgi_parse:write_record(Sock, ?FCGI_END_REQUEST, RequestId,
                                  <<0:32, ?FCGI_REQUEST_COMPLETE:8, 0:24>>),
    exit(normal).


%%--------------------------------------------------------------------
%% @spec convert(Content) -> ConvertedContent.
%% 
%% @doc 
%%  Convert the output returned from the callback to a usable binary.
%% @end
%%--------------------------------------------------------------------
convert(Content) when is_list(Content) ->
    list_to_binary(Content);
convert(Content) when is_binary(Content) ->
    Content.
