%%%-------------------------------------------------------------------
%% @doc movie_tickets top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(movie_tickets_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
    SupFlags = #{strategy => one_for_all, intensity => 1000, period => 3600},
    MT = #{id => movie_tickets, start => {movie_tickets, start_link, []},
        restart => permanent, shutdown => 2000, type => worker, modules => [movie_tickets]},
    {ok, {SupFlags, [MT]} }.

%%====================================================================
%% Internal functions
%%====================================================================
