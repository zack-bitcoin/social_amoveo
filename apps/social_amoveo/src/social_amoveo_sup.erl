%%%-------------------------------------------------------------------
%% @doc social_amoveo top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(social_amoveo_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
child_maker([]) -> [];
child_maker([X|T]) -> 
    [#{id => X, start => {X, start_link, []}}|
     child_maker(T)].
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = 
        child_maker(
          [scan_height, accounts, pubkeys, posts, 
           dms, height_tracker, settings]),
    {ok, {SupFlags, ChildSpecs}}.

