# `quick_take` – One-shot day-trade helper (demo)

```
POST /kabue/rpc/quick_take
```

> **⚠️ 警告**: 実際に **成行**で買い注文を発注し、その後すぐに利確注文（指値）を置く *デモユーティリティ* です。運用環境での使用は推奨しません。コード上では銘柄 **1459** がハードコーディングされています。

## 機能概要

1. `1459` を 1 株 **成行買い**します。
2. 約定価格 + 1 円で **指値売り**（建玉返済）を即座に発注します。
3. 注文 ID などを返却します。

## Request body

| field  | type | required | description |
|--------|------|----------|-------------|
| symbol | string | Yes | **必ず "1459"** を指定してください。それ以外の値を送ると 500 になります（関数内でパターンマッチ）。 |

```jsonc
{
  "symbol": "1459"
}
```

## Response body

| field | type   | description |
|-------|--------|-------------|
| order_id | string | 買い注文の ID |
| close_order_id | string | 売り（返済）注文の ID |
| close_order_price | number | 指値価格（約定最⾼値 + 1 円） |

```jsonc
{
  "order_id": "202504240001",
  "close_order_id": "202504240002",
  "close_order_price": 2032.5
}
```

## 注意事項

* 取引区分は **信用・一般デイトレ（制度／プレ空ではない）** 固定です。
* 売り返済注文は `nonlimit_open_afternoon`（寄成 OPGO 相当？）がハードコードされています。市場状況により約定しない可能性があります。
* 銘柄や数量・利幅はコードを改修しない限り変更できません。あくまで PoC（Proof-of-Concept）用です。
