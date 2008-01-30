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
%%%   The efcgi dispatch server
%%% @end
%%% @copyright (c) 2006 Erlware
%%%----------------------------------------------------------------------------
-module(efcgi_server).

-behaviour(gen_server).

-export([start_link/3,
         create/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {listen_socket,
                port,
                acceptor,
                pid,
                options}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(Port, Pid) -> ok.
%%
%% @doc
%%  Start a new listener with a unique port and pid
%% @end
%%--------------------------------------------------------------------
start_link(Port, Pid, Options) when is_integer(Port) ->
    Name = list_to_atom(lists:flatten(io_lib:format("efcgi_~w",[Port]))),
    gen_server:start_link({local, Name}, ?MODULE, [Port, Pid,
                                                   Options], []).

%%--------------------------------------------------------------------
%% @spec create(ServerPid, Pid) -> ok
%%
%% @doc
%%  Send message to cause a new acceptor to be created
%% @end
%%--------------------------------------------------------------------
create(ServerPid, Pid) ->
    gen_server:cast(ServerPid, {create, Pid}).


%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init([Port, Pid]) -> ok.
%%
%% @doc
%%  Called by gen_server framework at process startup. Create listening socket
%% @end
%%--------------------------------------------------------------------
init([Port, Pid, Options]) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port,[binary,{packet, raw},
                              {reuseaddr, true},
                              {active, false},
                              {backlog, 30}]) of
	{ok, Listen_socket} ->
            %%Create first accepting process
	    APid = efcgi_socket:start_link(self(), Listen_socket,
                                          Port, Pid, Options),
	    {ok, #state{listen_socket = Listen_socket,
                        port = Port,
			acceptor = APid,
                        options = Options,
                        pid = Pid}};
	{error, Reason} ->
	    {stop, Reason}
    end.


%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}.
%% @doc
%%  Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}.
%% @dec
%%   Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({create,_APid}, State = #state{listen_socket = Listen_socket,
                                          port = Port,
                                          options = Options,
                                          pid = Pid}) ->
    New_pid = efcgi_socket:start_link(self(), Listen_socket,
                                      Port, Pid, Options),
    {noreply, State#state{acceptor=New_pid}};

handle_cast(_Msg, State) ->
    {noreply, State}.



%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}.
%% @doc
%%   Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', APid, normal}, #state{acceptor=APid} = State) ->
    {noreply, State};
%% The current acceptor has died, wait a little and try again
handle_info({'EXIT', APid, _Abnormal},
            State = #state{listen_socket = ListenSocket,
                           acceptor = APid,
                           port = Port,
                           options = Options,
                           pid = Pid}) ->
    timer:sleep(2000),
    efcgi_socket:start_link(self(),
                            ListenSocket,
                            Port,
                            Pid,
                            Options),
    {noreply,State};
handle_info(_Info, State) ->
    {noreply, State}.


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
terminate(_Reason, State) ->
    gen_tcp:close(State#state.listen_socket),
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}.
%% @doc
%%   Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
