# collector

Custom metrics collectors for Erlang.

## Build

```sh
rebar3 compile
```

## Usage

This module can be used in production environment or any running
environment. Just compile `collector.erl` and load it from its path.

```sh
# compile and copy collector.beam in /tmp
erlc collector.erl
cp collector.beam /tmp
```

```erlang
% load collector module from path
code:load_abs("/tmp/collector").
```

This module embeds many function to display accurate information about
all processes and export them to prometheus.

```erlang
% manual collection for prometheus
collector:prometheus_gauges().

% init prometheus application
collector:prometheus_init().

% set or reset the metrics.
collector:prometheus_apply().
collector:prometheus_reset().
```

It is also possible to filter the metrics from options.

```erlang
Filter = fun(X) ->
  case X of
    #{ registered_name := undefined } -> false;
	_ -> true
end.
collector:start_link(#{ filter => Filer }).
```

Start a local linked collector.

```erlang
% start prometheus collector
collector:start_link().

% stop prometheus collector
collector:stop().
```

Don't forget to purge the module if it has been loaded from outside of
the release path.

```erlang
% purge collector module
code:purge(collector).
```
