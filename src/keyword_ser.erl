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
-module(keyword_ser).
-behaviour(gen_server).

%% API
-export([start_link/1, add_keyword_position/3, do_query/1, get_ids/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {document_positions}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ServerName) ->
    gen_server:start_link({local, ServerName}, ?MODULE, [], []).
    

add_keyword_position(Keyword, DocCatId, Position) ->
    Server = get_keyword_server( Keyword ),
    gen_server:cast(Server, {add, DocCatId, Position}).
do_query(Terms) when is_list(Terms) ->
    DocumentMatches = lists:flatten(rpc:pmap( { keyword_ser, get_ids }, [], Terms)),
    MatchesPerTermAndDocument = lists:foldl( 
				  fun( {Term, DocIds}, AccIn1) ->
					  lists:foldl(fun(DocId, AccIn2) -> dict:update_counter( {Term, DocId}, 1, AccIn2) end, AccIn1, DocIds)
				  end, dict:new(), DocumentMatches),
    async_bm25:document_scores( Terms, DocumentMatches, MatchesPerTermAndDocument).

get_ids(Term)->
    case find_keyword_server( Term ) of
	{ ok, Server } ->
	    { Term, gen_server:call( Server, get_ids )};
	{ error, _ } ->
	    { Term, []}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{document_positions=dict:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_ids, _From, State) ->
    Reply = dict:fold(
	      fun( DocId, Positions, AccIn) -> 
		      lists:duplicate(length( Positions ), DocId) ++ AccIn 
	      end,
	      [], State#state.document_positions
	     ),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({add, DocCatId, Position}, State ) ->
    NewState = #state{ document_positions = dict:append( DocCatId, Position, State#state.document_positions ) },
    {noreply, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_keyword_server(Keyword) ->
    ServerName = keyword_server_name(Keyword),
    ServerProcessName = list_to_atom(ServerName),
    case whereis(ServerProcessName) of
	undefined ->
            {ok, Pid} = supervisor:start_child(keyword_sup, [ServerProcessName]);
        Pid ->
            Pid
    end,
    Pid.

find_keyword_server(Keyword) ->
    ServerName = keyword_server_name(Keyword),
    ServerProcessName = list_to_atom(ServerName),
    case whereis(ServerProcessName) of
	undefined ->
	    { error, "Not found" };
        Pid ->
            { ok, Pid }
    end.    

keyword_server_name(Keyword) ->
    integer_to_list(
      erlang:phash2(
	porter:stem(
	  string:to_lower(Keyword)
	 )
       )
     ) ++ "_keyword_server".
