# `order_list` – Show open / filled orders

```
POST /kabue/rpc/order_list
```

現在発注中の注文を一覧取得し、**注文数量と約定数量の差**に基づいて簡易的に 3 つのカテゴリにグループ化します。

* `all` – 完全約定した注文（残数量 = 0）
* `partial` – 一部約定（残数量 > 0 かつ 約定数量 > 0）
* `none` – まったく約定していない注文（約定数量 = 0）

## Request body

空オブジェクトのみを受け付けます。

```json
{}
```

## Response body

```jsonc
{
  "order_list": [ /* kabu REST /orders 互換配列 */ ],
  "id_buckets": {
    "all":     ["202504240001"],
    "partial": ["202504240002"],
    "none":    ["202504240003", "202504240004"]
  }
}
```

* `order_list` は kabu ステーション REST API `/orders` のレスポンス構造をそのまま返します。
* `id_buckets` は追加で計算した **注文 ID のみ** の配列です。UI での色分けやフィルタリングに利用できます。
