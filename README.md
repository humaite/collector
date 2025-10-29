# collector

Custom metrics collectors for Erlang.

## Build

```sh
rebar3 compile
```

## Usage

```sh
# compile and copy collector.beam in /tmp
erlc collector.erl
cp collector.beam /tmp
```

```erlang
% load collector module from path
code:load_abs("/tmp/collector").

% manual collection for prometheus
collector:prometheus_gauges().
collector:prometheus_apply().
collector:prometheus_reset().

% start prometheus collector
collector:start_link().

% stop prometheus collector
collector:stop().

% purge collector module
code:purge(collector).
```
