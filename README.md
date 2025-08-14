# RabbitMQ Call Webhooks Plugin

This plugin extends RabbitMQ by forwarding messages from one or more queues to
arbitrary HTTP endpoints (often called *webhooks*).  It is inspired by the
unmaintained `rabbitmq-webhooks` plugin by Jon Brisbin and is designed to
work with modern RabbitMQ releases (3.10 and newer).  The code provided here
forms the basis for a complete plugin; it can be built into an `.ez` file and
enabled on a running broker.  The plugin connects to the broker using a
direct AMQP connection, consumes messages from configured queues and then
sends the message payload to the configured URL using the specified HTTP
method.  Messages are acknowledged only if the HTTP request succeeds (HTTP
status 2xx–3xx), otherwise they are requeued for another attempt.

## Features

* **Per‑queue webhooks:** configure one or more queues to be forwarded to
  different URLs.  Each webhook entry defines the queue name, the target URL
  and the HTTP method to use.
* **Direct connection:** the plugin uses `amqp_client`'s
  `#amqp_params_direct{}` type to connect to the same Erlang node that the
  broker is running on.  No network sockets need to be opened and the
  plugin can authenticate using the `guest` user or another account.
* **Synchronous HTTP calls:** outgoing requests are sent using Erlang’s
  built‑in `inets` HTTP client (`httpc`).  Only when a request returns a
  successful response does the plugin acknowledge the message; otherwise
  the message is nacked and requeued.
* **Simple configuration:** define webhook entries in either `rabbitmq.conf`
  format or the legacy `rabbitmq.config` format.  Multiple webhooks can be
  defined under the same plugin.

## Installation

1. **Clone the plugin source** into the RabbitMQ source tree or another
   directory under your build root.  The plugin is structured like other
   RabbitMQ plugins (see the [metronome example](https://github.com/rabbitmq/rabbitmq-metronome)).

2. **Build the plugin**.  RabbitMQ plugins are normally built with
   [`erlang.mk`](https://github.com/ninenines/erlang.mk) and the
   RabbitMQ‑specific `rabbitmq-components.mk` and `rabbitmq-plugins.mk` files.
   An example `Makefile` is provided in this directory.  If you already
   build RabbitMQ from source, copy or symlink `erlang.mk`,
   `rabbitmq-components.mk` and `rabbitmq-plugins.mk` from the RabbitMQ
   source tree into this directory and run:

   ```sh
   make dist
   ```

   The resulting `.ez` archive will be written to the `dist/` directory.

3. **Install the compiled plugin** by copying the `.ez` file into the
   broker’s `plugins/` directory and enabling it:

   ```sh
   cp dist/rabbit_callwebhooks.ez $RABBITMQ_HOME/plugins/
   rabbitmq-plugins enable rabbit_callwebhooks
   ```

4. **Configure the plugin** (see below) and restart the broker.

## Configuration

The plugin reads its configuration from the application environment key
`rabbit_callwebhooks` under the `webhooks` entry.  Each webhook entry must
include a queue name, a URL and a method.  Optional fields such as
`username`, `password` and `vhost` can be provided when the queue lives in
a vhost other than `/` or must be accessed by a different user.

Using the modern `rabbitmq.conf` format:

```ini
rabbit_callwebhooks.webhooks.1.queue = my.queue
rabbit_callwebhooks.webhooks.1.url   = https://example.com/webhook
rabbit_callwebhooks.webhooks.1.method = post

rabbit_callwebhooks.webhooks.2.queue = other.queue
rabbit_callwebhooks.webhooks.2.url   = https://api.example.net/endpoint
rabbit_callwebhooks.webhooks.2.method = put
rabbit_callwebhooks.webhooks.2.username = webhook_user
rabbit_callwebhooks.webhooks.2.password = s3cret
rabbit_callwebhooks.webhooks.2.vhost   = /myvhost
```

When using the legacy `rabbitmq.config` format (Erlang terms):

```erlang
{rabbit_callwebhooks, [
  {webhooks, [
    #{queue => <<"my.queue">>,
      url   => <<"https://example.com/webhook">>,
      method => post},
    #{queue => <<"other.queue">>,
      url    => <<"https://api.example.net/endpoint">>,
      method => put,
      username => <<"webhook_user">>,
      password => <<"s3cret">>,
      vhost   => <<"/myvhost">>}
  ]}
]}.
```

## File overview

| Path | Purpose |
|-----|---------|
| `src/rabbit_callwebhooks.app.src` | Defines the OTP application for the plugin and its dependencies. |
| `src/rabbit_callwebhooks_app.erl` | Application behaviour implementation; starts the top‑level supervisor. |
| `src/rabbit_callwebhooks_sup.erl` | Supervisor that starts a worker process for each configured webhook. |
| `src/rabbit_callwebhooks_worker.erl` | Worker process that consumes messages from a queue and delivers them to an HTTP endpoint. |
| `Makefile` | Example build file for compiling the plugin using `erlang.mk`. |

## Limitations

* The HTTP client used is Erlang’s built‑in `httpc` from the `inets` application.  For higher performance or advanced features (e.g. HTTPS client certificates), consider replacing it with another client such as [`hackney`](https://github.com/benoitc/hackney).
* Errors during HTTP delivery will result in messages being nacked and requeued.  There is no dead‑letter handling built in.
* There is no batching or rate limiting; each message triggers one HTTP call.
* The plugin currently forwards the raw payload as the request body and does not set any additional headers.  You can extend `send_http/4` in `rabbit_callwebhooks_worker.erl` to include message headers or properties.

## Acknowledgements

This plugin is heavily inspired by the historical
[`rabbitmq-webhooks`](https://github.com/jbrisbin/rabbitmq-webhooks) project【506323775653347†L260-L365】.  Many thanks to Jon Brisbin for the original idea.
