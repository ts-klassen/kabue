# `panic_exit` – Emergency flat position

```
POST /kabue/rpc/panic_exit
```

**全ての未約定注文を取消し、残っている建玉を即座に反対売買で決済する** 緊急用 RPC です。

## 処理内容

1. REST API `/orders` で取得した注文のうち、`state != finished`（受付 / 活動中）を **すべて取消** します。
2. REST API `/positions` で取得した **残数量 > 0** の建玉に対し、
   * 現物 – `market` 成行で反対売買 (`cash_margin = spot`)
   * 信用 – `market` 成行で返済 (`cash_margin = margin_repay`)
   を一括で発注します。
3. キャンセル・返済それぞれの結果をまとめて返却します。

## Request body

空オブジェクトのみを受け付けます。

```json
{}
```

## Response body

```jsonc
{
  "cancel_list": ["202504240001", "202504240002"],
  "exit_list": [ /* 発注に使用した注文オブジェクト */ ],
  "cancel_result": [
    { "success": true,  "payload": { /* REST /cancelorder 応答 */ } },
    { "success": false, "payload": { "Code": 4001020, "Message": "失効" } }
  ],
  "exit_result": [
    { "success": true,  "payload": { /* REST /sendorder 応答 */ } }
  ]
}
```

* `cancel_result` / `exit_result` は個別の REST API 呼び出しを **left / right** いずれかの `either` タプルから組み立てたものです。
  * `success = true` – タプルが `{right, Payload}` だった場合
  * `success = false` – タプルが `{left,  Payload}` だった場合

## 注意事項

* 建玉が **複数の商品区分（スポット現物、信用 etc.）** に跨っていても単一呼び出しでクローズします。
* 取消不能（すでに全約定 / 失効済み）の注文は `cancel_result` に `success = false` で格納されます。エラーとしては扱いません。
* 建玉が無い場合は `exit_list` と `exit_result` が空配列になります。
