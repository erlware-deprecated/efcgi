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
%%%  Supervisors for request handler.
%%% @end
%%% @copyright (C) 2006, Eric Merritt
%%% Created : 29 Nov 2006 by Eric Merritt 
%%%---------------------------------------------------------------------------
-module(efcgi_request_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_requestor/5]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}.
%% 
%% @doc 
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}.
%% 
%% @doc 
%%  Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RequestHandler = {request_handler,{efcgi_request, start_link,[]},
                      transient,2000,worker,[efcgi_request]},
    {ok,{{simple_one_for_one,0,1}, [RequestHandler]}}.


%%--------------------------------------------------------------------
%% @spec start_requestor(Sock, RequestId) -> {ok, Pid}.
%% 
%% @doc 
%%  Start chiled requestor with teh specified requestid.
%% @end
%%--------------------------------------------------------------------
start_requestor(Sock,
                Pid,
                RequestId,
                Type,
                KeepAlive) ->
    supervisor:start_child(?SERVER, [Sock,
                                     Pid,
                                     RequestId,
                                     Type,
                                     KeepAlive]).

%%====================================================================
%% Internal functions
%%====================================================================
