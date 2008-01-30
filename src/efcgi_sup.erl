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
%%% @author Eric Merritt
%%% @doc
%%%   Top level supervisor for the system.
%%% @end
%%%----------------------------------------------------------------------------
-module(efcgi_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    RequestProxy = {efcgi_request_proxy,{efcgi_request_proxy,start_link,[]},
                    permanent,2000,worker,[efcgi_request_proxy]},
    RequestSup = {efcgi_request_sup,{efcgi_request_sup,start_link,[]},
                  permanent,2000,supervisor,[efcgi_request_sup]},
    ServerSup = {efcgi_server_sup,{efcgi_server_sup,start_link,[]},
                  permanent,2000,supervisor,[efcgi_server_sup]},
    
    {ok,{{one_for_one, 1000, 10}, [RequestProxy, RequestSup, ServerSup]}}.

%%====================================================================
%% Internal functions
%%====================================================================
