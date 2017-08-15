%%%-------------------------------------------------------------------
%%% @author mikee
%%% @copyright (C) 2017, Mikhail Oganesyan
%%% @doc
%%%   Database backend for 'Movie Tickets' using dets
%%% @end
%%% Created : 12. Aug 2017 06:22
%%%-------------------------------------------------------------------
-module(movie_tickets_base).
-author("mikee").

-include("movie_tickets.hrl").

%% API
-export([
  open_movie_base/1,
  register_movie/2,
  reserve_seat/1,
  get_information/1,
  update_title/2,
  close_movie_base/0
]).

-ifdef(TEST).
-export([
  delete_movie_base/0
]).
-endif.

-define(BASE_NAME, movie_base).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%    Open movie database
%%    Should be closed as many times as was opened
%% @spec open_movie_base(string()) -> ok | {error, internal_error}
%% @end
%%--------------------------------------------------------------------
-spec open_movie_base(string()) -> ok | {error, ?INTERNAL_ERROR}.
open_movie_base(Base) ->
  case dets:open_file(?BASE_NAME, [{file, Base}, {keypos, #movie_information.movie_id}]) of
    {ok, ?BASE_NAME} ->
      ?LOG("Base ~p opened", [Base]),
      ok;
    {error, _Reason} ->
      ?LOG("Cannot open base ~p reason ~p", [Base, _Reason]),
      {error, ?INTERNAL_ERROR}
  end.

%%--------------------------------------------------------------------
%% @doc
%%    Register movie and available seats
%% @spec register_movie(#movie_id{}, pos_integer()) -> ok |
%%          {error, invalid_parameters | already_exists | internal_error}
%% @end
%%--------------------------------------------------------------------
-spec register_movie(#movie_id{}, pos_integer()) -> ok | {error, Reason} when
      Reason :: ?INVALID_PARAMETERS | ?ALREADY_EXISTS | ?INTERNAL_ERROR.
register_movie(#movie_id{imdbId = ImdbId, screenId = ScreenId} = MovieId, AvailableSeats) when
               is_list(ImdbId), is_list(ScreenId), is_integer(AvailableSeats), AvailableSeats > 0 ->
  Movie = #movie_information{movie_id = MovieId, movieTitle = ImdbId, availableSeats = AvailableSeats},
  case dets:insert_new(?BASE_NAME, Movie) of
    true ->
      ok;
    false ->
      {error, ?ALREADY_EXISTS};
    {error, _Reason} ->
      ?LOG("Cannot insert reason ~p", [_Reason]),
      {error, ?INTERNAL_ERROR}
  end;
register_movie(_,_) ->
  {error, ?INVALID_PARAMETERS}.

%%--------------------------------------------------------------------
%% @doc
%%    Reserve seat to specified movie
%% @spec reserve_seat(#movie_id{}) -> ok |
%% {error, invalid_parameters | not_found | no_free_seats | internal_error}
%% @end
%%--------------------------------------------------------------------
-spec reserve_seat(#movie_id{}) -> ok | {error, Reason} when
      Reason :: ?INVALID_PARAMETERS | ?NOT_FOUND | ?NO_FREE_SEATS | ?INTERNAL_ERROR.
reserve_seat(#movie_id{imdbId = ImdbId, screenId = ScreenId} = MovieId) when
             is_list(ImdbId), is_list(ScreenId) ->
  case dets:lookup(?BASE_NAME, MovieId) of
    [#movie_information{availableSeats = A, reservedSeats = R} = Movie] when R < A  ->
      case dets:insert(?BASE_NAME, Movie#movie_information{reservedSeats = R+1}) of
        ok -> ok;
        {error, _Reason} ->
          ?LOG("Cannot insert reason ~p", [_Reason]),
          {error, ?INTERNAL_ERROR}
      end;
    [#movie_information{availableSeats = A, reservedSeats = R}] when R =:= A ->
      {error, ?NO_FREE_SEATS};
    {error, _Reason} ->
      ?LOG("Cannot lookup reason ~p", [_Reason]),
      {error, ?INTERNAL_ERROR};
    _ ->
      {error, ?NOT_FOUND}
  end;
reserve_seat(_) ->
  {error, ?INVALID_PARAMETERS}.

%%--------------------------------------------------------------------
%% @doc
%%    Get information for specified movie
%% @spec get_information(#movie_id{}) -> {ok, #movie_information{}} |
%%                {error, invalid_parameters | not_found | internal_error}
%% @end
%%--------------------------------------------------------------------
-spec get_information(#movie_id{}) -> {ok, #movie_information{}} | {error, Reason} when
      Reason :: ?INVALID_PARAMETERS | ?NOT_FOUND | ?INTERNAL_ERROR.
get_information(#movie_id{imdbId = ImdbId, screenId = ScreenId} = MovieId) when
                is_list(ImdbId), is_list(ScreenId) ->
  case dets:lookup(?BASE_NAME, MovieId) of
    [Movie] ->
      {ok, Movie};
    {error, _Reason} ->
      ?LOG("Cannot lookup reason ~p", [_Reason]),
      {error, ?INTERNAL_ERROR};
    _ ->
      {error, ?NOT_FOUND}
  end;
get_information(_) ->
  {error, ?INVALID_PARAMETERS}.

%%--------------------------------------------------------------------
%% @doc
%%    Update title for specified movie
%% @spec update_title(#movie_id{}, string()) -> ok |
%%              {error, invalid_parameters | not_found | internal_error}
%% @end
%%--------------------------------------------------------------------
-spec update_title(#movie_id{}, Title) -> ok | {error, Reason} when
      Title :: string(),
      Reason :: ?INVALID_PARAMETERS | ?NOT_FOUND | ?INTERNAL_ERROR.
update_title(#movie_id{imdbId = ImdbId, screenId = ScreenId} = MovieId, Title) when
             is_list(ImdbId), is_list(ScreenId), is_list(Title) ->
  case dets:lookup(?BASE_NAME, MovieId) of
    [Movie]  ->
      case dets:insert(?BASE_NAME, Movie#movie_information{movieTitle = Title}) of
        ok -> ok;
        {error, _Reason} ->
          ?LOG("Cannot insert reason ~p", [_Reason]),
          {error, ?INTERNAL_ERROR}
      end;
    {error, _Reason} ->
      ?LOG("Cannot lookup reason ~p", [_Reason]),
      {error, ?INTERNAL_ERROR};
    _ ->
      {error, ?NOT_FOUND}
  end;
update_title(_, _) ->
  {error, ?INVALID_PARAMETERS}.

%%--------------------------------------------------------------------
%% @doc
%%    Close movie base
%% @spec close_movie_base() -> ok | {error, internal_error}
%% @end
%%--------------------------------------------------------------------
-spec close_movie_base() -> ok | {error, ?INTERNAL_ERROR}.
close_movie_base() ->
  case dets:close(?BASE_NAME) of
    ok ->
      ?LOG("Base ~p closed", [?BASE_NAME]),
      ok;
    {error, _Reason} ->
      ?LOG("Cannot close base ~p reason ~p", [?BASE_NAME, _Reason]),
      {error, ?INTERNAL_ERROR}
  end.


%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Clear all information
%% @spec delete_movie_base() -> ok | internal_error
%% @end
%%--------------------------------------------------------------------
-ifdef(TEST).
-spec delete_movie_base() -> ok | ?INTERNAL_ERROR.
delete_movie_base() ->
  case dets:delete_all_objects(?BASE_NAME) of
    ok -> ok;
    {error, _Reason} ->
      ?LOG("Cannot delete_all_objects reason ~p", [_Reason]),
      {error, ?INTERNAL_ERROR}
  end.
-endif.
