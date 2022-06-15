%%%-------------------------------------------------------------------
%% @doc social_amoveo top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(social_amoveo_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1, stop/0]).

-define(SERVER, ?MODULE).

-define(keys, 
          [accounts, pubkeys, posts, nonces,
           dms, height_tracker, settings]).

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
        child_maker(?keys),
    {ok, {SupFlags, ChildSpecs}}.

    
stop() -> child_killer(?keys).
child_killer([]) -> [];
child_killer([H|T]) -> 
    supervisor:terminate_child(social_amoveo_sup, H),
    child_killer(T).
