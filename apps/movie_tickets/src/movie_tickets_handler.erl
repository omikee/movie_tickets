%%%-------------------------------------------------------------------
%%% @author mikee
%%% @copyright (C) 2017, Mikhail Oganesyan
%%% @doc
%%%   HTTP request handler
%%% @end
%%% Created : 12. Aug 2017 01:51
%%%-------------------------------------------------------------------
-module(movie_tickets_handler).
-author("mikee").

-include("movie_tickets.hrl").

-define(REQUEST,                              <<"request">>).
-define(PARAMS,                                <<"params">>).

-define(REGISTER_MOVIE,                <<"register_movie">>).
-define(RESERVE_SEAT,                    <<"reserve_seat">>).
-define(GET_INFORMATION,              <<"get_information">>).

-define(IMDB_ID,                               <<"imdbId">>).
-define(AVAILABLE_SEATS,               <<"availableSeats">>).
-define(SCREEN_ID,                           <<"screenId">>).
-define(TITLE,                             <<"movieTitle">>).
-define(RESERVED_SEATS,                 <<"reservedSeats">>).

-define(RESULT,                                <<"result">>).
-define(OK,                                        <<"ok">>).
-define(REPLY,                                  <<"reply">>).
-define(ERROR,                                  <<"error">>).
-define(REASON,                                <<"reason">>).

-define(REASON_UNKNOWN_REQUEST,       <<"unknown_request">>).
-define(REASON_INVALID_PARAMETERS, <<"invalid_parameters">>).
-define(REASON_ALREADY_EXISTS,         <<"already_exists">>).
-define(REASON_NOT_FOUND,                   <<"not_found">>).
-define(REASON_NO_FREE_SEATS,           <<"no_free_seats">>).
-define(REASON_INTERNAL_ERROR,         <<"internal_error">>).

%% API
-export([
  init/2
]).

%%--------------------------------------------------------------------
%% @doc
%%    cowboy handler callback
%% @end
%%--------------------------------------------------------------------
init(Req0, Opts) ->
  Req = case cowboy_req:method(Req0) of
    <<"GET">> ->
      #{?REQUEST := Request} = M = maps:from_list(cowboy_req:parse_qs(Req0)),
      R = #{?REQUEST => Request, ?PARAMS => maps:remove(?REQUEST, M)},
      reply(jsone:encode(handle_request(R)), Req0);
    <<"POST">> ->
      {ok, Params, _Req} = cowboy_req:read_body(Req0),
      reply(jsone:encode(handle_request(jsone:decode(Params))), Req0);
    _ ->
      %% Method not allowed.
      cowboy_req:reply(405, Req0)
  end,
  {ok, Req, Opts}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Dispatch incoming requests
%% @end
%%--------------------------------------------------------------------
-spec handle_request(map()) -> map().
handle_request(#{?REQUEST := ?REGISTER_MOVIE, ?PARAMS := M}) -> handle_register_movie(M);
handle_request(#{?REQUEST := ?RESERVE_SEAT, ?PARAMS := M}) -> handle_reserve_seat(M);
handle_request(#{?REQUEST := ?GET_INFORMATION, ?PARAMS := M}) -> handle_get_information(M);
handle_request(_) -> #{?RESULT => ?ERROR, ?REASON => ?REASON_UNKNOWN_REQUEST}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Handle register_movie request
%% @end
%%--------------------------------------------------------------------
-spec handle_register_movie(map()) -> map().
handle_register_movie(#{?IMDB_ID := ImdbId, ?SCREEN_ID := ScreenId, ?AVAILABLE_SEATS := AvailableSeats}) ->
  ?LOG("register_movie: ~p ~p ~p", [ImdbId, ScreenId, AvailableSeats]),
  Available = if
    is_integer(AvailableSeats) -> AvailableSeats;
    true -> list_to_integer(binary_to_list(AvailableSeats))
  end,
  case movie_tickets:register_movie(binary_to_list(ImdbId), binary_to_list(ScreenId), Available) of
    ok ->
      #{?RESULT => ?OK};
    {error, Reason} ->
      #{?RESULT => ?ERROR, ?REASON => map_reason(Reason)}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Handle reserve_seat request
%% @end
%%--------------------------------------------------------------------
-spec handle_reserve_seat(map()) -> map().
handle_reserve_seat(#{?IMDB_ID := ImdbId, ?SCREEN_ID := ScreenId}) ->
  ?LOG("reserve_seat: ~p ~p", [ImdbId, ScreenId]),
  case movie_tickets:reserve_seat(binary_to_list(ImdbId), binary_to_list(ScreenId)) of
    ok ->
      #{?RESULT => ?OK};
    {error, Reason} ->
      #{?RESULT => ?ERROR, ?REASON => map_reason(Reason)}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Handle get_information request
%% @end
%%--------------------------------------------------------------------
-spec handle_get_information(map()) -> map().
handle_get_information(#{?IMDB_ID := ImdbId, ?SCREEN_ID := ScreenId}) ->
  ?LOG("get_information: ~p ~p", [ImdbId, ScreenId]),
  case movie_tickets:get_information(binary_to_list(ImdbId), binary_to_list(ScreenId)) of
    {ok, #movie_information{movieTitle = Title, availableSeats = AvailableSeats, reservedSeats = ReservedSeats}} ->
      Reply = #{?IMDB_ID => ImdbId, ?SCREEN_ID => ScreenId,
                ?AVAILABLE_SEATS => AvailableSeats,
                ?TITLE => unicode:characters_to_binary(Title),
                ?RESERVED_SEATS => ReservedSeats},
      #{?RESULT => ?OK, ?REPLY => Reply};
    {error, Reason} ->
      #{?RESULT => ?ERROR, ?REASON => map_reason(Reason)}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Reply with binary string
%% @end
%%--------------------------------------------------------------------
-spec reply(binary(), term()) -> term().
reply(Reply, Req) ->
  cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain; charset=utf-8">>}, Reply, Req).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Convert reason to binary string formay
%% @end
%%--------------------------------------------------------------------
-spec map_reason(atom()) -> binary().
map_reason(?INVALID_PARAMETERS) -> ?REASON_INVALID_PARAMETERS;
map_reason(?ALREADY_EXISTS) -> ?REASON_ALREADY_EXISTS;
map_reason(?NOT_FOUND) -> ?REASON_NOT_FOUND;
map_reason(?NO_FREE_SEATS) -> ?REASON_NO_FREE_SEATS;
map_reason(?INTERNAL_ERROR) -> ?REASON_INTERNAL_ERROR.
