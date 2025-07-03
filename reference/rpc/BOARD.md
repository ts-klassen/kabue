# `board` – Get board (order-book) information

```
POST /kabue/rpc/board
```

指定した銘柄（**現物株式**のみ）について、最新の板情報（気配・出来高等）を取得します。

内部実装では以下 2 つのデータソースを自動的に切り替えています。

1. **WebSocket キャッシュ** – `kabue_mufje_ws_apic` で既に購読済みの銘柄であれば、メモリにキャッシュされた値を即座に返します（`is_ws_data = true`）。
2. **REST API** – 未購読の場合は `kabuステーション REST /board` エンドポイントにアクセスし、同時に WebSocket の購読登録を行います（`is_ws_data = false`）。

## Request body

| field  | type   | required | description |
|--------|--------|----------|-------------|
| symbol | string | Yes      | 銘柄コード（4 桁） |

```jsonc
{
  "symbol": "7203"
}
```

## Response body

```jsonc
{
  "is_ws_data": true,    // or false – データ取得元
  "board": { /* kabu REST /board 互換オブジェクト */ },
  "time": 1714452123456   // UNIX epoch (ms)
}
```

* `board` の構造は kabu ステーション REST API の `/board` 応答と同一です。
* `time` は取得時刻をミリ秒精度のエポックで返します。

### 例

```jsonc
{
  "is_ws_data": true,
  "board": {
    "Symbol": "7203",
    "CurrentPrice": 2030.5,
    "Bids": [ { "Price": 2030.0, "Qty": 1200 }, … ],
    "Asks": [ { "Price": 2031.0, "Qty": 800 }, … ]
  },
  "time": 1714452123456
}
```

## Notes

* 呼び出し側が明示的に WebSocket 購読する処理を書く必要はありません。未購読の場合でも自動的に登録されます。
* **初回呼び出し**で REST API を叩いた場合は、WebSocket データが届くまでに最大数百 ms 程度の遅延が発生することがあります。
