%%
%% Worker process for the call webhooks plugin.
%%
%% Each worker subscribes to a single queue and forwards each received
%% message to an HTTP endpoint.  The process uses Erlang's built‑in
%% `httpc` client to make requests.  Messages are acknowledged only on
%% successful delivery; otherwise they are nacked and requeued.

-module(rabbit_callwebhooks_worker).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {
          queue        :: binary() | string(),
          url          :: binary() | string(),
          method       :: atom(),
          channel      :: pid(),
          username     :: binary() | string(),
          password     :: binary() | string(),
          vhost        :: binary() | string()
         }).

%%---------------------------------------------------------------------------
%% API
%%---------------------------------------------------------------------------

%% Start a worker with the given webhook configuration.  The argument
%% `Webhook` must be a map containing at least `queue`, `url` and `method` keys.

start_link(Webhook) ->
    gen_server:start_link(?MODULE, Webhook, []).

%%---------------------------------------------------------------------------
%% gen_server callbacks
%%---------------------------------------------------------------------------

init(Webhook) ->
    process_flag(trap_exit, true),
    %% Ensure the inets application is started for httpc
    ok = ensure_inets(),
    %% Extract fields from the webhook map.  Accept both binaries and strings.
    Q     = maps:get(queue, Webhook),
    Url   = maps:get(url, Webhook),
    Meth  = maps:get(method, Webhook, post),
    User  = maps:get(username, Webhook, <<"guest">>),
    Pass  = maps:get(password, Webhook, <<"guest">>),
    VHost = maps:get(vhost, Webhook, <<"/">>),
    %% Start a direct connection to this broker.  The direct parameters tell
    %% amqp_client to connect to the current Erlang node rather than via TCP.
    Params = #amqp_params_direct{username = User,
                                 password = Pass,
                                 virtual_host = VHost},
    {ok, Conn} = amqp_connection:start(Params),
    {ok, Chan} = amqp_connection:open_channel(Conn),
    %% Register this process as a consumer for the queue.  We disable
    %% no_ack so that we control acknowledgements manually.
    _ConsumeOk = amqp_basic:consume(Chan, Q, self(), #amqp_basic.consume{no_ack = false}),
    State = #state{queue = Q,
                   url = Url,
                   method = Meth,
                   channel = Chan,
                   username = User,
                   password = Pass,
                   vhost = VHost},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle delivery of a message.  `Delivery` is a 'basic.deliver' record,
%% `Content` is a #amqp_msg{} record containing the payload and properties.
handle_info({#'basic.deliver'{delivery_tag = Tag}, Content}, State = #state{channel = Chan, url = Url, method = Meth}) ->
    Payload = Content#amqp_msg.payload,
    %% Attempt to send the HTTP request.  On success ack the message;
    %% otherwise requeue it.
    case send_http(Meth, Url, Payload, Content) of
        ok ->
            amqp_basic:ack(Chan, Tag);
        {error, _Reason} ->
            %% negative acknowledge and requeue
            amqp_basic:nack(Chan, Tag, false, true)
    end,
    {noreply, State};

handle_info({'basic.consume_ok', _}, State) ->
    %% Ignore confirmation of consuming
    {noreply, State};

handle_info({'basic.cancel_ok', _ConsumerTag}, State) ->
    %% Consumer has been cancelled
    {stop, normal, State};

handle_info({'basic.cancel', _ConsumerTag}, State) ->
    %% Consumer has been cancelled unexpectedly
    {stop, normal, State};

handle_info({'DOWN', _Ref, process, _Pid, Reason}, State) ->
    %% Underlying connection terminated
    {stop, Reason, State};

handle_info(Info, State) ->
    io:format("rabbit_callwebhooks_worker: unexpected message: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, State = #state{channel = Chan}) ->
    %% Attempt to cleanly close the channel and connection
    catch amqp_channel:close(Chan),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%---------------------------------------------------------------------------
%% Internal helpers
%%---------------------------------------------------------------------------

%% Ensure inets/httpc is started.  Idempotent.
ensure_inets() ->
    case application:ensure_all_started(inets) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

%% send_http(Method, Url, Body, Content) -> ok | {error, Reason}
%%
%% Send an HTTP request using httpc.  Supported methods are post, put,
%% delete and get.  The body is the raw message payload; no encoding is
%% performed.  You can modify this function to include headers derived
%% from the AMQP properties (Content#amqp_msg.props).

send_http(post, Url, Body, _Content) ->
    http_request(post, Url, Body);
send_http(put, Url, Body, _Content) ->
    http_request(put, Url, Body);
send_http(delete, Url, _Body, _Content) ->
    http_request(delete, Url, <<>>);
send_http(get, Url, _Body, _Content) ->
    http_request(get, Url, <<>>);
send_http(_, Url, Body, _Content) ->
    %% Default to POST if an unknown method is supplied
    http_request(post, Url, Body).

%% Perform the actual HTTP request via the inets httpc API.
http_request(Method, Url, Body) ->
    Headers = [],
    Request = case Method of
        get    -> {Url, Headers};
        delete -> {Url, Headers};
        post   -> {Url, Headers, <<"application/octet-stream">>, Body};
        put    -> {Url, Headers, <<"application/octet-stream">>, Body};
        _      -> {Url, Headers, <<"application/octet-stream">>, Body}
    end,
    case httpc:request(Method, Request, [], []) of
        {ok, {{_Version, Code, _ReasonPhrase}, _RespHeaders, _RespBody}}
          when Code >= 200, Code < 400 ->
            ok;
        {ok, {{_Version, Code, _ReasonPhrase}, _RespHeaders, _RespBody}} ->
            {error, {http_error, Code}};
        {error, Reason} ->
            {error, Reason}
    end.