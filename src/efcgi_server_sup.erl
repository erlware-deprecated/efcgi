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
%%%  Support for supervision of servers.
%%% @end
%%% @copyright (C) 2006, 
%%% Created :  5 Dec 2006 by Eric Merritt 
%%%---------------------------------------------------------------------------
-module(efcgi_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_server/3]).

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
    EfcgiServer = {efcgi_server,{efcgi_server,start_link,[]},
                   temporary,2000,worker, [efcgi_server]},
    {ok,{{simple_one_for_one,1000,10}, [EfcgiServer]}}.


%%--------------------------------------------------------------------
%% @spec start_server(Port, Pid) -> ok.
%% 
%% @doc 
%%  Start a new server on the proper port with the specified pid.
%% @end
%%--------------------------------------------------------------------
start_server(Port, Pid, Options) ->
    supervisor:start_child(?SERVER, [Port, Pid, Options]).

%%====================================================================
%% Internal functions
%%====================================================================

