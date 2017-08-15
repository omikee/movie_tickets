%%%-------------------------------------------------------------------
%%% @author mikee
%%% @copyright (C) 2017, Mikhail Oganesyan
%%% @doc
%%%   Tests for 'Movie Tickets'
%%% @end
%%% Created : 12. Aug 2017 13:32
%%%-------------------------------------------------------------------
-module(movie_tickets_tests).
-author("mikee").

-include_lib("eunit/include/eunit.hrl").

-include("movie_tickets.hrl").

-define(IMDB_ID1, "tt0111161").
-define(IMDB_ID2, "tt0068646").
-define(IMDB_ID3, "tt0071562").
-define(IMDB_ID4, "tt0468569").
-define(IMDB_ID5, "tt0050083").

-define(SCREEN_ID1, "screen_id1").
-define(SCREEN_ID2, "screen_id2").
-define(SCREEN_ID3, "screen_id3").
-define(SCREEN_ID4, "screen_id4").
-define(SCREEN_ID5, "screen_id5").

-define(TITLE1, "The Shawshank Redemption").
-define(TITLE2, "The Godfather").
-define(TITLE3, "The Godfather: Part II").
-define(TITLE4, "The Dark Knight").
-define(TITLE5, "12 Angry Men").

-define(TEST_BASE, "test_base.dat").

imdb_get_title_test_() ->
  [
    {"The Shawshank Redemption", ?_assertEqual(?TITLE1, movie_tickets:imdb_get_title(?IMDB_ID1))},
    {"The Godfather", ?_assertEqual(?TITLE2, movie_tickets:imdb_get_title(?IMDB_ID2))},
    {"The Godfather: Part II", ?_assertEqual(?TITLE3, movie_tickets:imdb_get_title(?IMDB_ID3))},
    {"The Dark Knight", ?_assertEqual(?TITLE4, movie_tickets:imdb_get_title(?IMDB_ID4))},
    {"12 Angry Men", ?_assertEqual(?TITLE5, movie_tickets:imdb_get_title(?IMDB_ID5))}
  ].

setup() ->
  ?assertEqual(ok, movie_tickets_base:open_movie_base(?TEST_BASE)).

cleanup(_) ->
  ?assertEqual(ok, movie_tickets_base:delete_movie_base()),
  file:delete(?TEST_BASE).

movie_tickets_base_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [
      {"register_movie_sanny_case", fun register_movie_sanny_case/0},
      {"register_movie_imdbid_not_string", fun register_movie_imdbid_not_string/0},
      {"register_movie_screenid_not_string", fun register_movie_screenid_not_string/0},
      {"register_movie_availableseats_not_integer", fun register_movie_availableseats_not_integer/0},
      {"register_movie_availableseats_less_or_equals_zero", fun register_movie_availableseats_less_or_equals_zero/0},
      {"register_movie_same_imdbid", fun register_movie_same_imdbid/0},
      {"register_movie_same_screenid", fun register_movie_same_screenid/0},
      {"register_movie_twice", fun register_movie_twice/0},
      {"reserve_seat_sunny_case", fun reserve_seat_sunny_case/0},
      {"reserve_seat_imdbid_not_string", fun reserve_seat_imdbid_not_string/0},
      {"reserve_seat_screenid_not_string", fun reserve_seat_screenid_not_string/0},
      {"reserve_seat_imdbid_not_registered", fun reserve_seat_imdbid_not_registered/0},
      {"reserve_seat_screenid_not_registered", fun reserve_seat_screenid_not_registered/0},
      {"reserve_seat_no_free_seats", fun reserve_seat_no_free_seats/0},
      {"get_information_sunny_case", fun get_information_sunny_case/0},
      {"get_information_imdbid_not_string", fun get_information_imdbid_not_string/0},
      {"get_information_screenid_not_string", fun get_information_screenid_not_string/0},
      {"get_information_not_registered", fun get_information_not_registered/0},
      {"update_title_sunny_case", fun update_title_sunny_case/0},
      {"update_title_imdbid_not_string", fun update_title_imdbid_not_string/0},
      {"update_title_screenid_not_string", fun update_title_screenid_not_string/0},
      {"update_title_title_not_string", fun update_title_title_not_string/0},
      {"update_title_not_registered", fun update_title_not_registered/0}
    ]
  }.

register_movie_sanny_case() ->
  MovieId = #movie_id{imdbId = ?IMDB_ID1, screenId = ?SCREEN_ID1},
  ?assertEqual(ok, movie_tickets_base:register_movie(MovieId, 1)).

register_movie_imdbid_not_string() ->
  MovieId = #movie_id{imdbId = ok, screenId = ?SCREEN_ID1},
  ?assertEqual({error, ?INVALID_PARAMETERS}, movie_tickets_base:register_movie(MovieId, 10)).

register_movie_screenid_not_string() ->
  MovieId = #movie_id{imdbId = ?IMDB_ID1, screenId = ok},
  ?assertEqual({error, ?INVALID_PARAMETERS}, movie_tickets_base:register_movie(MovieId, 10)).

register_movie_availableseats_not_integer() ->
  MovieId = #movie_id{imdbId = ?IMDB_ID1, screenId = ?SCREEN_ID1},
  ?assertEqual({error, ?INVALID_PARAMETERS}, movie_tickets_base:register_movie(MovieId, ok)).

register_movie_availableseats_less_or_equals_zero() ->
  MovieId = #movie_id{imdbId = ?IMDB_ID1, screenId = ?SCREEN_ID1},
  ?assertEqual({error, ?INVALID_PARAMETERS}, movie_tickets_base:register_movie(MovieId, -10)).

register_movie_same_imdbid() ->
  MovieId = #movie_id{imdbId = ?IMDB_ID1, screenId = ?SCREEN_ID2},
  ?assertEqual(ok, movie_tickets_base:register_movie(MovieId, 10)).

register_movie_same_screenid() ->
  MovieId = #movie_id{imdbId = ?IMDB_ID2, screenId = ?SCREEN_ID1},
  ?assertEqual(ok, movie_tickets_base:register_movie(MovieId, 10)).

register_movie_twice() ->
  MovieId = #movie_id{imdbId = ?IMDB_ID1, screenId = ?SCREEN_ID1},
  ?assertEqual({error, ?ALREADY_EXISTS}, movie_tickets_base:register_movie(MovieId, 10)).

reserve_seat_sunny_case() ->
  MovieId = #movie_id{imdbId = ?IMDB_ID1, screenId = ?SCREEN_ID1},
  ?assertEqual(ok, movie_tickets_base:reserve_seat(MovieId)).

reserve_seat_imdbid_not_string() ->
  MovieId = #movie_id{imdbId = ok, screenId = ?SCREEN_ID1},
  ?assertEqual({error, ?INVALID_PARAMETERS}, movie_tickets_base:reserve_seat(MovieId)).

reserve_seat_screenid_not_string() ->
  MovieId = #movie_id{imdbId = ?IMDB_ID1, screenId = ok},
  ?assertEqual({error, ?INVALID_PARAMETERS}, movie_tickets_base:reserve_seat(MovieId)).

reserve_seat_imdbid_not_registered() ->
  MovieId = #movie_id{imdbId = ?IMDB_ID3, screenId = ?SCREEN_ID1},
  ?assertEqual({error, ?NOT_FOUND}, movie_tickets_base:reserve_seat(MovieId)).

reserve_seat_screenid_not_registered() ->
  MovieId = #movie_id{imdbId = ?IMDB_ID1, screenId = ?SCREEN_ID3},
  ?assertEqual({error, ?NOT_FOUND}, movie_tickets_base:reserve_seat(MovieId)).

reserve_seat_no_free_seats() ->
  MovieId = #movie_id{imdbId = ?IMDB_ID1, screenId = ?SCREEN_ID1},
  ?assertEqual({error, ?NO_FREE_SEATS}, movie_tickets_base:reserve_seat(MovieId)).

get_information_sunny_case() ->
  MovieId = #movie_id{imdbId = ?IMDB_ID1, screenId = ?SCREEN_ID1},
  MovieInformation = #movie_information{movie_id = MovieId, movieTitle = ?IMDB_ID1, availableSeats = 1, reservedSeats = 1},
  ?assertEqual({ok, MovieInformation}, movie_tickets_base:get_information(MovieId)).

get_information_imdbid_not_string() ->
  MovieId = #movie_id{imdbId = ok, screenId = ?SCREEN_ID1},
  ?assertEqual({error, ?INVALID_PARAMETERS}, movie_tickets_base:get_information(MovieId)).

get_information_screenid_not_string() ->
  MovieId = #movie_id{imdbId = ?IMDB_ID1, screenId = ok},
  ?assertEqual({error, ?INVALID_PARAMETERS}, movie_tickets_base:get_information(MovieId)).

get_information_not_registered() ->
  MovieId = #movie_id{imdbId = ?IMDB_ID3, screenId = ?SCREEN_ID3},
  ?assertEqual({error, ?NOT_FOUND}, movie_tickets_base:get_information(MovieId)).

update_title_sunny_case() ->
  MovieId = #movie_id{imdbId = ?IMDB_ID1, screenId = ?SCREEN_ID1},
  MovieInformation = #movie_information{movie_id = MovieId, movieTitle = ?TITLE2, availableSeats = 1, reservedSeats = 1},
  ?assertEqual(ok, movie_tickets_base:update_title(MovieId, ?TITLE2)),
  ?assertEqual({ok, MovieInformation}, movie_tickets_base:get_information(MovieId)).

update_title_imdbid_not_string() ->
  MovieId = #movie_id{imdbId = ok, screenId = ?SCREEN_ID1},
  ?assertEqual({error, ?INVALID_PARAMETERS}, movie_tickets_base:update_title(MovieId, ?TITLE3)).

update_title_screenid_not_string() ->
  MovieId = #movie_id{imdbId = ?IMDB_ID1, screenId = ok},
  ?assertEqual({error, ?INVALID_PARAMETERS}, movie_tickets_base:update_title(MovieId, ?TITLE3)).

update_title_title_not_string() ->
  MovieId = #movie_id{imdbId = ?IMDB_ID1, screenId = ?SCREEN_ID1},
  ?assertEqual({error, ?INVALID_PARAMETERS}, movie_tickets_base:update_title(MovieId, ok)).

update_title_not_registered() ->
  MovieId = #movie_id{imdbId = ?IMDB_ID3, screenId = ?SCREEN_ID3},
  ?assertEqual({error, ?NOT_FOUND}, movie_tickets_base:update_title(MovieId, ?TITLE3)).

