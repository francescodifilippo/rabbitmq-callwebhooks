%%
%% Supervisor for the call webhooks plugin.
%%
%% On startup the supervisor reads the application environment key
%% rabbit_callwebhooks/webhooks and starts a worker process for each entry.

-module(rabbit_callwebhooks_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
%% supervisor callbacks
-export([init/1]).

%% Include to get access to maps:merge/2 in old Erlang

%%---------------------------------------------------------------------------
%% API
%%---------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%---------------------------------------------------------------------------
%% supervisor callback
%%---------------------------------------------------------------------------

init([]) ->
    %% Fetch webhook configuration from the application environment.
    Webhooks = case application:get_env(rabbit_callwebhooks, webhooks) of
        {ok, WH} -> WH;
        undefined -> [];
        _ -> []
    end,
    %% Normalise the configuration entries into maps.  Entries may be lists
    %% (proplists) or maps; convert lists to maps for convenience.
    HookMaps = [normalise(Webhook) || Webhook <- Webhooks],
    %% Create a child specification for each webhook.  Each worker is
    %% identified by a unique atom id derived from its index.  Workers are
    %% permanent processes: if they crash they will be restarted.
    ChildSpecs = lists:map(
      fun({HookMap, Index}) ->
              %% Generate a stable atom id such as wh_1, wh_2, ...
              Id = list_to_atom("wh_" ++ integer_to_list(Index)),
              #{id => Id,
                start => {rabbit_callwebhooks_worker, start_link, [HookMap]},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [rabbit_callwebhooks_worker]}
      end,
      lists:zip(HookMaps, lists:seq(1, length(HookMaps)))),
    %% Use a one‑for‑one strategy: if a worker dies only that worker is
    %% restarted.
    {ok, {{one_for_one, 10, 10}, ChildSpecs}}.

%% normalise/1
%% Convert a webhook configuration entry into a map.  Accepts both lists
%% (proplists) and maps.  Atoms and binaries are left unchanged.

normalise(L) when is_list(L) ->
    maps:from_list(L);
normalise(M) when is_map(M) ->
    M;
normalise(_) ->
    #{}.