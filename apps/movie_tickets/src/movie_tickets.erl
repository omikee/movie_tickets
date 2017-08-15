%%%-------------------------------------------------------------------
%%% @author mikee
%%% @copyright (C) 2017, Mikhail Oganesyan
%%% @doc
%%%   'Movie Tickets' server, handles requests from clients
%%% @end
%%% Created : 12. Aug 2017 03:51
%%%-------------------------------------------------------------------
-module(movie_tickets).
-author("mikee").

-include("movie_tickets.hrl").

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  stop/0,
  register_movie/3,
  reserve_seat/2,
  get_information/2
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-ifdef(TEST).
-export([
  imdb_get_title/1
]).
-endif.

-define(SERVER, ?MODULE).

-define(IMDB_REQUEST_STRING, "http://www.theimdbapi.org/api/movie?movie_id=").
%%-define(IMDB_REQUEST_STRING, "https://www.themoviedb.org/documentation/api").

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%%    Start the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%%    Stop the server
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
  gen_server:cast(?SERVER, stop).

%%--------------------------------------------------------------------
%% @doc
%%    Register movie in database
%% @spec register_movie(string(), string(), pos_integer()) -> ok | {error, atom()}
%% @end
%%--------------------------------------------------------------------
-spec register_movie(string(), string(), pos_integer()) -> ok | {error, atom()}.
register_movie(ImdbId, ScreenId, AvailableSeats) ->
  try
    gen_server:call(?SERVER, #register_movie{movie_id = #movie_id{imdbId = ImdbId, screenId = ScreenId},
                                             availableSeats = AvailableSeats})
  catch _:_ ->
    {error, ?INTERNAL_ERROR}
  end.

%%--------------------------------------------------------------------
%% @doc
%%    Reserve seat on the registered movie
%% @spec reserve_seat(string(), string()) -> ok | {error, atom()}
%% @end
%%--------------------------------------------------------------------
-spec reserve_seat(string(), string()) -> ok | {error, atom()}.
reserve_seat(ImdbId, ScreenId) ->
  try
    gen_server:call(?SERVER, #reserve_seat{movie_id = #movie_id{imdbId = ImdbId, screenId = ScreenId}})
  catch _:_ ->
    {error, ?INTERNAL_ERROR}
  end.

%%--------------------------------------------------------------------
%% @doc
%%    Get information about movie ang reservation
%% @spec get_information(string(), string()) -> ok | {error, atom()}
%% @end
%%--------------------------------------------------------------------
-spec get_information(string(), string()) -> {ok, #movie_information{}} | {error, atom()}.
get_information(ImdbId, ScreenId) ->
  try
    gen_server:call(?SERVER, #movie_id{imdbId = ImdbId, screenId = ScreenId})
  catch _:_ ->
    {error, ?INTERNAL_ERROR}
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Init the server
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {ok, #{}}.
init([]) ->
  Base = application:get_env(movie_tickets, base, "movie_tickets.dat"),
  movie_tickets_base:open_movie_base(Base),
  {ok, #{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request, {pid(), term()}, #{}) -> {reply, Reply, #{}} when
      Request :: #register_movie{} | #reserve_seat{} | #movie_id{},
      Reply :: ok | {error, atom()}.
handle_call(#register_movie{movie_id = MovieId, availableSeats = Available}, _From, State) ->
  Reply = case movie_tickets_base:register_movie(MovieId, Available) of
    ok ->
      async_imdb_get_title(MovieId),
      ok;
    Other ->
      Other
  end,
  {reply, Reply, State};
handle_call(#reserve_seat{movie_id = MovieId}, _From, State) ->
  Reply = movie_tickets_base:reserve_seat(MovieId),
  {reply, Reply, State};
handle_call(#movie_id{} = MovieId, _From, State) ->
  Reply = movie_tickets_base:get_information(MovieId),
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  ?LOG("Unexpected call ~p", [_Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(stop, #{}) -> {noreply, #{}} | {stop, normal, #{}}.
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_Request, State) ->
  ?LOG("Unexpected cast ~p", [_Request]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(term(), #{}) -> {noreply, #{}}.
handle_info(_Info, State) ->
  ?LOG("Unexpected info ~p", [_Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Termination callback
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), #{}) -> ok.
terminate(_Reason, _State) ->
  ?LOG("Termination with reason ~p", [_Reason]),
  movie_tickets_base:close_movie_base(),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), #{}, term()) -> {ok, #{}}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Get and update title asynchronously
%% @spec async_imdb_get_title(#movie_id{}) -> pid()
%% @end
%%--------------------------------------------------------------------
-spec async_imdb_get_title(#movie_id{}) -> pid().
async_imdb_get_title(MovieId) ->
  spawn(fun() ->
    movie_tickets_base:update_title(MovieId, imdb_get_title(MovieId#movie_id.imdbId))
  end).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Get IMDB title from the internet
%% @spec imdb_get_title(string()) -> string()
%% @end
%%--------------------------------------------------------------------
-spec imdb_get_title(string()) -> string().
imdb_get_title(ImdbId) ->
  try
    {ok, {_, _, Body}} = httpc:request(?IMDB_REQUEST_STRING ++ ImdbId),
    {ok, BinTitle} = maps:find(<<"title">>, jsone:decode(list_to_binary(Body))),
    Title = unicode:characters_to_list(BinTitle),
    Title
  catch _T:_E ->
    ?LOG("IMDB get title exception ~p:~p", [_T,_E]),
    ImdbId
  end.
