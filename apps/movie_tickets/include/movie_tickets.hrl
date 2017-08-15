%%%-------------------------------------------------------------------
%%% @author mikee
%%% @copyright (C) 2017, Mikhail Oganesyan
%%% @doc
%%%   Common for 'Movie Tickets'
%%% @end
%%% Created : 12. Aug 2017 03:58
%%%-------------------------------------------------------------------
-author("mikee").

-ifndef(MOVIE_TICKETS_H).
-define(MOVIE_TICKETS_H, true).

-ifdef(DEBUG).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(LOG(F,D), ?debugFmt(F++"~n",D)).
-else. %% TEST
-define(LOG(F,D), io:format(F++"~n",D)).
-endif. %% TEST
-else. %% DEBUG
-define(LOG(F,D), ok).
-endif. %% DEBUG

-define(INVALID_PARAMETERS, invalid_parameters).
-define(ALREADY_EXISTS,         already_exists).
-define(NOT_FOUND,                   not_found).
-define(NO_FREE_SEATS,           no_free_seats).
-define(INTERNAL_ERROR,         internal_error).

% movie id
-record(movie_id, {
  imdbId              :: string(), % IMDB movie identifier
  screenId            :: string()  % externally managed identifier of information when and where the movie is screened
}).

-record(register_movie, {
  movie_id         :: #movie_id{}, % movie id
  availableSeats :: pos_integer()  % the total seats available for this movie
}).

-record(reserve_seat, {
  movie_id         :: #movie_id{}  % movie id
}).

-record(movie_information, {
  movie_id                :: #movie_id{}, % movie id
  movieTitle                 :: string(), % title of the movie
  availableSeats        :: pos_integer(), % total seats available for this movie
  reservedSeats = 0 :: non_neg_integer()  % total number of reserved seats for a movie and screen
}).

-endif. %% MOVIE_TICKETS_H
