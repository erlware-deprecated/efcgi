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
%%%  Supports request Id <> Pid handler.
%%% @end
%%% @copyright (C) 2006, Erlware
%%% Created : 30 Nov 2006 by Eric Merritt <cyberlync@gmail.com>
%%%----------------------------------------------------------------------------
-module(efcgi_request_proxy).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0, params/2, stdin/2,
         stdout/2, stderr/2, abort_request/1,
         register/2, unregister/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {mappings=[]}).

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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @spec params(RequestId, Params) -> ok.
%%
%% @doc
%%  Send the params binary to the service identified by params.
%% @end
%%--------------------------------------------------------------------
params(RequestId, Params) ->
    gen_server:cast(?SERVER, {params, RequestId, Params}).

%%--------------------------------------------------------------------
%% @spec stdin(RequestId, Stdin) -> ok.
%%
%% @doc
%%  Send the stdin stream part to the service.
%% @end
%%--------------------------------------------------------------------
stdin(RequestId, Stdin) ->
    gen_server:cast(?SERVER, {stdin, RequestId, Stdin}).


%%--------------------------------------------------------------------
%% @spec stdout(RequestId, Data) -> ok.
%%
%% @doc
%%  send stdout to the request
%% @end
%%--------------------------------------------------------------------
stdout(RequestId, Data) ->
    gen_server:cast(?SERVER, {stdout, RequestId, Data}).


%%--------------------------------------------------------------------
%% @spec stderr(RequestId, Data) -> ok.
%%
%% @doc
%%  send stderr to the request.
%% @end
%%--------------------------------------------------------------------
stderr(RequestId, Data) ->
    gen_server:cast(?SERVER, {stderr, RequestId, Data}).


%%--------------------------------------------------------------------
%% @spec abort_request(RequestId) -> ok.
%%
%% @doc
%%   Abort the current request being processed by request id.
%% @end
%%--------------------------------------------------------------------
abort_request(RequestId) ->
    gen_server:cast(?SERVER, {abort, RequestId}).

%%--------------------------------------------------------------------
%% @spec register(RequestId, Pid) -> ok.
%%
%% @doc
%%  Register a request id with a pid for future reference.
%% @end
%%--------------------------------------------------------------------
register(RequestId, Pid) ->
    gen_server:cast(?SERVER, {register, RequestId, Pid}).

%%--------------------------------------------------------------------
%% @spec unregister(RequestId) -> ok.
%%
%% @doc
%%  Unregister a currently registerd request id.
%% @end
%%--------------------------------------------------------------------
unregister(RequestId) ->
    gen_server:cast(?SERVER, {unregister, RequestId}).

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
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}.
%%
%% @doc
%% Handling call mappings
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
%% Handling cast mappings
%% @end
%%--------------------------------------------------------------------
handle_cast({register, RequestId, Pid}, State) ->
    Mappings = [{RequestId, Pid} | State#state.mappings],
    {noreply, State#state{mappings=Mappings}};
handle_cast({unregister, RequestId}, State) ->
    Mappings = remove(RequestId, State#state.mappings, []),
    {noreply, State#state{mappings=Mappings}};
handle_cast({params, RequestId, Params}, State) ->
    Pid = get(RequestId, State#state.mappings),
    send_params(Pid, Params),
    {noreply, State};
handle_cast({stdin, RequestId, Stdin}, State) ->
    Pid = get(RequestId, State#state.mappings),
    send_stdin(Pid, Stdin),
    {noreply, State};
handle_cast({abort, RequestId}, State) ->
    Pid = get(RequestId, State#state.mappings),
    send_abort(Pid),
    {noreply, State};
handle_cast({stdout, RequestId, Data}, State) ->
    Pid = get(RequestId, State#state.mappings),
    send_stdout(Pid, Data),
    {noreply, State};
handle_cast({stderr, RequestId, Data}, State) ->
    Pid = get(RequestId, State#state.mappings),
    send_stderr(Pid, Data),
    {noreply, State}.




%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}.
%%
%% @doc
%% Handling all non call/cast mappings
%% @end
%%--------------------------------------------------------------------
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
terminate(_Reason, _State) ->
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
%%--------------------------------------------------------------------
%% @spec remove(RequestId, Mappings, Acc) -> NMappings.
%%
%% @doc
%%  Remove the specified request id from the list of mappings
%% @end
%%--------------------------------------------------------------------
remove(RequestId, [{RequestId, _} | T], Acc) ->
    Acc ++ T;
remove(RequestId, [H | T], Acc) ->
    remove(RequestId, T, [H | Acc]);
remove(_RequestId, [], Acc) ->
    Acc.


%%--------------------------------------------------------------------
%% @spec get(RequestId, Ids) -> Value | no_pid.
%%
%% @doc
%%  Get the value associated with the passed in request id.
%% @end
%%--------------------------------------------------------------------
get(RequestId, [{RequestId, Value} | _]) ->
    Value;
get(RequestId, [_ | T]) ->
    get(RequestId, T);
get(_, []) ->
    no_pid.


%%--------------------------------------------------------------------
%% @spec params(Pid, Params) -> ok.
%%
%% @doc
%%  Send params to the system
%% @end
%%--------------------------------------------------------------------
send_params(Pid, Params) ->
    gen_server:cast(Pid, {params, Params}).


%%--------------------------------------------------------------------
%% @spec stdin(Pid, Data) -> ok
%%
%% @doc
%%  Send stdin data to the system
%% @end
%%--------------------------------------------------------------------
send_stdin(Pid, Data) ->
    gen_server:cast(Pid, {stdin, Data}).

%%--------------------------------------------------------------------
%% @spec send_stdout(Pid, Data) -> ok.
%%
%% @doc
%%  Send the data to stdout.
%% @end
%%--------------------------------------------------------------------
send_stdout(Pid, Data) ->
    gen_server:cast(Pid, {stdout, Data}).

%%--------------------------------------------------------------------
%% @spec send_stderr(Pid, Data) -> ok.
%%
%% @doc
%%  Send the data to the request.
%% @end
%%--------------------------------------------------------------------
send_stderr(Pid, Data) ->
    gen_server:cast(Pid, {stderr, Data}).

%%--------------------------------------------------------------------
%% @spec send_abort(Pid) -> ok.
%%
%% @doc
%%  Send the request abort sequence.
%% @end
%%--------------------------------------------------------------------
send_abort(Pid) ->
    gen_server:cast(Pid, abort).
