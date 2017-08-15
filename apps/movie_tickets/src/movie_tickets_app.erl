%%%-------------------------------------------------------------------
%% @doc movie_tickets public API
%% @end
%%%-------------------------------------------------------------------

-module(movie_tickets_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Ip = application:get_env(movie_tickets, ip, {127,0,0,1}),
    Port = application:get_env(movie_tickets, port, 8010),
    Dispatch = cowboy_router:compile([
        {'_', [{'_', movie_tickets_handler, #{}}]}
    ]),
    {ok, _} = cowboy:start_clear(http,
        [{ip, Ip}, {port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    movie_tickets_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
