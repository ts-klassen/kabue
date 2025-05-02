# kabuステーション WebSocket (PUSH) API

kabu ステーションでは、板情報や約定情報などの **リアルタイムデータ** を WebSocket で PUSH 配信する機能が提供されています。以下はその概要をまとめたドキュメントです。正式な仕様は kabu ステーション公式マニュアルを参照してください。

---

## 接続エンドポイント

| 用途 | URL |
|------|-----|
| 本番 | `ws://localhost:18080/kabusapi/streaming` |
| 検証 | `ws://localhost:18081/kabusapi/streaming` |

### 認証

接続時、以下いずれかの方法で認証トークンを渡します。

1. **クエリパラメータに付与**
 `ws://localhost:18080/kabusapi/streaming?token=<取得したトークン>`
2. **HTTP ヘッダー `X-API-KEY`**
 ライブラリがヘッダー送信をサポートしている場合は利用可能です。

トークンは REST API の `/token` エンドポイントで取得したものと同一です。

---

## 配信トピック

WebSocket で送られてくるメッセージは以下の形式です。

```jsonc
{
 "Topic": "board", // トピック識別子
 "Message": { /* データ本体 (JSON) */ }
}
```

| Topic | 概要 |
|-------|------|
| `board` | 気配 (板) 情報 |
| `ticker` | 約定ティック情報 |
| `symbol` | 銘柄基本情報 |
| `order` | 自身の注文受付/約定通知 |
| `position` | 自身の建玉変化通知 |

> 実際に受け取れるトピックは kabu ステーションのバージョンにより増減する場合があります。

---

## 購読 (Subscription) フロー

1. **銘柄登録**
 REST API の `/register` エンドポイント (`PUT`) に、購読したい銘柄コードを配列で送信します。
2. **WebSocket に接続**
 上記エンドポイントへ WebSocket で接続し、メッセージ受信待ちします。
3. **データ受信**
 登録済み銘柄に関するリアルタイムデータが PUSH 配信されます。

> 最大 50 銘柄まで登録可能です (REST と PUSH を合わせた合計)。

### サンプルワークフロー (擬似コード)

```pseudo
POST /token // 認証トークンを得る
PUT /register // 例: ["7203", "9984"] を登録

WS ws://localhost:18080/kabusapi/streaming?token=<token>
while (socket open):
 msg = await socket.recv()
 switch msg.Topic:
 case "board":
 handleBoard(msg.Message)
 case "ticker":
 handleTicker(msg.Message)
 ...
```

---

## トピック別メッセージ仕様

| Topic | 主なフィールド | 型 | 説明 |
|-------|--------------|----|------|
| `board` | `Symbol` | string | 銘柄コード |
| | `Exchange` | int | 市場コード |
| | `CurrentPrice` | number | 現在値 |
| | `Bids[]` / `Asks[]` | array<object> | 板 (10 本)。`Price` / `Qty` |
| `ticker` | `Symbol` | string | 銘柄コード |
| | `Price` | number | 約定値段 |
| | `Qty` | int | 約定数量 |
| | `Datetime` | string | 約定日時 (YYYY-MM-DD hh:mm:ss.xxx) |
| `symbol` | `Symbol` / `SymbolName` | string | コード / 名称 |
| | `UpperLimit` / `LowerLimit` | number | 制限値幅上限 / 下限 |
| `order` | `OrderId` | string | 注文番号 |
| | `State` | string | 注文状態 (`受付済`, `約定`, `失効` …) |
| | `Price` / `Qty` / `ExecutedQty` | number / int | 注文価格 / 発注数量 / 約定数量 |
| `position` | `HoldID` | string | 建玉 ID |
| | `Side` | string | `1`:売 / `2`:買 |
| | `Qty` / `Price` | int / number | 建玉数量 / 建単価 |

> 配信 JSON は **各トピックで上記以外にも多くのフィールド** を持ちますが、実務で特に参照頻度が高い項目を中心に抜粋しています。完全なフィールドセットは kabu ステーション公式マニュアルを参照してください。

---

## サンプルメッセージ

```jsonc
{
 "Topic": "board",
 "Message": {
 "Symbol": "7203",
 "Exchange": 1,
 "CurrentPrice": 2030.5,
 "CurrentPriceTime": "13:23:45",
 "Bids": [ { "Price": 2030.0, "Qty": 1200 }, … ],
 "Asks": [ { "Price": 2031.0, "Qty": 800 }, … ]
 }
}
```

---

## 列挙値（抜粋）

### 市場コード (Exchange)

| 値 | 市場 |
|----|------|
| 1 | 東証 |
| 3 | 名証 |
| 5 | 福証 |
| 6 | 札証 |
| 9 | SOR |

### Side

| 値 | 意味 |
|----|------|
| 1 | 売 |
| 2 | 買 |

> WebSocket で配信される値は REST API と共通です。ここに載っていない列挙値は REST_API.md のリファレンスを参照してください。

---

---

## 切断・再接続の扱い

* 接続が切れた場合、トークンの有効期限内であればそのまま再接続可能です。
* 長時間接続し続けると、ネットワーク環境によりタイムアウトされる場合があります。クライアント側でハートビート送信や再接続を実装してください。

---

## よくあるエラー

| エラー内容 | 原因・対処 |
|-----------|-----------|
| 401 Unauthorized | トークンが無効 / 失効している。 `/token` で再発行する。 |
| 429 TooManyRequests | 再接続を短時間に繰り返している。リトライ間隔を空ける。 |
| 接続直後に切断される | `/register` で銘柄を登録していない、または 50 銘柄制限超過。 |

---

## 参考

* REST API ドキュメント: [REST_API.md](REST_API.md)
* 列挙値や詳細パラメータは REST API ドキュメント末尾の「列挙値リファレンス」を参照してください。

以上。