# `echo` – Connectivity test

```
POST /kabue/rpc/echo
```

The simplest RPC: it returns whatever JSON object you send.  This is useful to
verify that

* The HTTP route (`/kabue/rpc/echo`) is reachable.
* Authorization (if any reverse-proxy is in front) is working.
* The Cowboy → JSON decode/encode → `kabue_rpc` function chain behaves as
  expected.

## Request body

Any JSON object, for example:

```jsonc
{
  "hello": "world",
  "value": 123
}
```

## Response body

Exactly the same JSON object is returned:

```jsonc
{
  "hello": "world",
  "value": 123
}
```

> Implementation note: `kabue_rpc:echo/1` simply returns its argument.
