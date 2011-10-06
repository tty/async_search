%%%-------------------------------------------------------------------
%%% @author Ward Bekker <ward@tty.nl>
%%% @copyright
%%% Copyright (c) 2011 Ward Bekker / TTY Internet Solutions
%%% Permission is hereby granted, free of charge, to any person obtaining
%%% a copy of this software and associated documentation files (the
%%% "Software"), to deal in the Software without restriction, including
%%% without limitation the rights to use, copy, modify, merge, publish,
%%% distribute, sublicense, and/or sell copies of the Software, and to
%%% permit persons to whom the Software is furnished to do so, subject to
%%% the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be
%%% included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
%%% LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
%%% OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
%%% WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%% @end
%%%-------------------------------------------------------------------
-module(async_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    KeywordSup = {keyword_sup, {keyword_sup, start_link, []},
	      Restart, Shutdown, Type, [keyword_sup]},
    FacetSup = {facet_sup, {facet_sup, start_link, []},
	      Restart, Shutdown, Type, [facet_sup]},
    StackOverflowImporterSer = {stackoverflow_importer_ser, {stackoverflow_importer_ser, start_link, []},
	      Restart, Shutdown, Type, [stackoverflow_importer_ser]},
    DocumentSer = {document_ser, {document_ser, start_link, []},
	      Restart, Shutdown, Type, [document_ser]},
    QuerySer = {query_ser, {query_ser, start_link, []},
	      Restart, Shutdown, Type, [query_ser]},
    WebSocketSer = {websocket_ser, {websocket_ser, start_link, []},
	      Restart, Shutdown, Type, [websocket_ser]},

    {ok, {SupFlags, [KeywordSup, FacetSup, StackOverflowImporterSer, DocumentSer, QuerySer, WebSocketSer]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
