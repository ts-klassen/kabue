# `echo` – Connectivity test

```
POST /kabue/rpc/echo
```

最も単純な RPC です。受け取った JSON をそのまま返します。

## Request body

Any JSON object. 例：

```jsonc
{
  "hello": "world",
  "value": 123
}
```

## Response body

リクエストで送った JSON がそのまま返ります。

```jsonc
{
  "hello": "world",
  "value": 123
}
```

## 用途

* API エンドポイントまで通信／認証経路が確立しているかを確認する。
* `kabue_rpc` のハンドラチェーン（Cowboy, JSON デコード／エンコード）が正しく動作しているかをテストする。

> **備考**: サーバコード側では `kabue_rpc:echo/1` が単に引数を返しているだけです。
