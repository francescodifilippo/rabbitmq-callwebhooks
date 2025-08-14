##
## Build file for the call webhooks plugin.
##
## This Makefile follows the conventions used by RabbitMQ plugins built with
## [erlang.mk](https://github.com/ninenines/erlang.mk).  To build the plugin
## you need copies of `erlang.mk`, `rabbitmq-components.mk` and
## `rabbitmq-plugins.mk` in this directory.  These files are usually
## available in the RabbitMQ source tree under `deps/` or at the top level.
##
## Usage:
##   make        # compile the code into ebin/
##   make dist   # package the plugin into a .ez file under dist/

PLUGIN_NAME = rabbit_callwebhooks

DEPS = rabbitmq_server amqp_client
DEP_RABBITMQ_SERVER_COMMIT ?= main
DEP_AMQP_CLIENT_COMMIT ?= main

DEPS_DIRS = $(DEPS)

## The following include will bring in erlang.mk and the RabbitMQ
## plugin build system.  They must be present in this directory or
## somewhere in $(CURDIR).
include erlang.mk
include rabbitmq-plugins.mk