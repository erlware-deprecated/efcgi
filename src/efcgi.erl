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
%%%  Efcgi configuration system.
%%% @end
%%%---------------------------------------------------------------------------
-module(efcgi).

%% API
-export([start/2, start/3]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start(Port, Pid) -> ok.
%% 
%% @doc
%%  Start a new fcgi server on the specified port
%% @end
%%--------------------------------------------------------------------
start(Port, Pid) ->   
    efcgi_server_sup:start_server(Port, Pid, []).
   

%%--------------------------------------------------------------------
%% @spec start(Port, Pid, Options) -> ok.
%% 
%% @doc 
%%  Start the server with max_conns and max_reqs options specified.
%% @end
%%--------------------------------------------------------------------
start(Port, Pid, Options) ->
    efcgi_server_sup:start_server(Port, Pid, Options).


