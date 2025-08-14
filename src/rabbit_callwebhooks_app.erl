%%
%% Application behaviour module for rabbit_callwebhooks.
%%
%% This simple application delegates startup to the supervisor defined in
%% rabbit_callwebhooks_sup.  There is no state stored in the application
%% itself.

-module(rabbit_callwebhooks_app).
-behaviour(application).

%% API
-export([start/2, stop/1]).

%%---------------------------------------------------------------------------
%% application callbacks
%%---------------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    %% Start the top‑level supervisor.  The supervisor is responsible
    %% for reading configuration and starting worker processes for each
    %% configured webhook.
    rabbit_callwebhooks_sup:start_link().

stop(_State) ->
    %% Nothing special to do on shutdown.
    ok.