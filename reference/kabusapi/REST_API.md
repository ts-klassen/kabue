# kabuステーション REST API (v1.5)

このドキュメントは、`kabu_STATION_API.yaml` で定義されている kabu ステーション REST API の概要を Markdown 形式でまとめたものです。OpenAPI 仕様の全文はリポジトリ内の YAML ファイルを参照してください。

---

## ベース URL

| 用途 | URL |
|------|-----|
| 本番 | `http://localhost:18080/kabusapi` |
| 検証 | `http://localhost:18081/kabusapi` |

> プロトコルは HTTP です。HTTPS は利用出来ません (ローカルホスト限定の通信を想定しているため)。

## 認証

1. `/token` エンドポイントに `POST` して API トークンを取得します。  
   リクエストボディには YAML に定義されている `RequestToken` スキーマを JSON で渡します。
2. 以降のリクエストでは、**必ず** HTTP ヘッダー `X-API-KEY` に取得したトークンを設定してください。  
   (例) `X-API-KEY: a1b2c3d4…`
3. トークンは以下のタイミングで失効します。
   - kabu ステーションを終了 / ログアウトしたとき
   - 新しいトークンを再発行したとき (古いトークンは無効化されます)

## レートリミット

- 発注系: おおよそ **5 req/sec**
- 情報系 / 取引余力系: おおよそ **10 req/sec**

サーバから `429 TooManyRequests` が返ってきた場合は、適切にリトライを行ってください。

---

## エンドポイント一覧

以下の表は YAML から自動抽出した「メソッド / パス / 概要」のみを掲載しています。詳細なパラメータやレスポンスは OpenAPI 仕様を参照してください。

### 認証 (auth)

| Method | Path | Summary |
|--------|------|---------|
| `POST` | `/token` | トークン発行 |

### 発注 (order)

| Method | Path | Summary |
|--------|------|---------|
| `POST` | `/sendorder` | 注文発注（現物・信用） |
| `POST` | `/sendorder/future` | 注文発注（先物） |
| `POST` | `/sendorder/option` | 注文発注（オプション） |
| `PUT`  | `/cancelorder` | 注文取消 |

### 発注エンドポイント詳細

#### POST `/sendorder/future`

リクエストボディ（JSON）

| キー | 型 | 必須 | 説明 |
|------|----|------|------|
| `Symbol` | string | ✓ | 銘柄コード |
| `Exchange` | int | ✓ | 市場コード（2:日通し 23:日中 24:夜間 32〜34:SOR） |
| `TradeType` | int | ✓ | 取引区分（1:新規 2:返済 10:決済付替 13:決済） |
| `TimeInForce` | int | ✓ | 執行条件（1:FAK, 2:FAS, 3:FAK指値, 4:FAS指値） |
| `Side` | string | ✓ | 1:売, 2:買 |
| `Qty` | int | ✓ | 発注数量 |
| `Price` | int | ✓ | 注文価格（成行時は 0） |
| `ExpireDay` | int | ✓ | 執行期限（日付 YYYYMMDD, 当日:0） |
| `FrontOrderType` | int | ✓ | 注文種別（10:成行 20:指値 30:STOP成行 etc.） |
| `ReverseLimitOrder` | object |  | 逆指値注文パラメータ（`TriggerSec`, `TriggerPrice`, `UnderOver`, `AfterHitOrderType`, `AfterHitPrice`） |

レスポンス: `OrderSuccess` スキーマ  
`{ "OrderId": "***", "Result": 0 }`

---

#### POST `/sendorder/option`

リクエストボディは `/sendorder/future` と同一構造（`RequestSendOrderDerivOption` スキーマ）です。

違いは `Symbol` にオプション銘柄コードを指定する点のみ。

---

#### PUT `/cancelorder`

リクエストボディ（JSON）

| キー | 型 | 必須 | 説明 |
|------|----|------|------|
| `OrderId` | string | ✓ | 取消したい注文番号（`sendorder` 系レスポンスで取得） |

成功レスポンス: `{ "Result": 0 }`

### 取引余力 (wallet)

| Method | Path | Summary |
|--------|------|---------|
| `GET` | `/wallet/cash` | 取引余力（現物） |
| `GET` | `/wallet/cash/{symbol}` | 取引余力（現物）（銘柄指定） |
| `GET` | `/wallet/future` | 取引余力（先物） |
| `GET` | `/wallet/future/{symbol}` | 取引余力（先物）（銘柄指定） |
| `GET` | `/wallet/margin` | 取引余力（信用） |
| `GET` | `/wallet/margin/{symbol}` | 取引余力（信用）（銘柄指定） |
| `GET` | `/wallet/option` | 取引余力（オプション） |
| `GET` | `/wallet/option/{symbol}` | 取引余力（オプション）（銘柄指定） |

### 情報 (info)

| Method | Path | Summary |
|--------|------|---------|
| `GET` | `/board/{symbol}` | 板情報を取得 |
| `GET` | `/symbol/{symbol}` | 銘柄情報を取得 |
| `GET` | `/symbol/{symbol}/future` | 先物銘柄情報を取得 |
| `GET` | `/symbol/{symbol}/option` | オプション銘柄情報を取得 |
| `GET` | `/exchange` | 取引所情報を取得 |
| `GET` | `/order` | 注文一覧取得 |
| `GET` | `/order/{orderId}` | 注文詳細取得 |
| `GET` | `/position` | 建玉一覧取得 |

### 銘柄登録 (register)

| Method | Path | Summary |
|--------|------|---------|
| `PUT` | `/register` | PUSH 配信銘柄を登録 |

> API 登録銘柄リストに登録出来る銘柄は **最大 50 銘柄** です (REST / PUSH 合算)。

---

## HTTP ステータスコード

| コード | 説明 |
|-------:|------|
| `200` | 正常終了 |
| `400` | リクエスト不正 (バリデーションエラー等) |
| `401` | 未認証 (`X-API-KEY` が無い / 無効) |
| `403` | 権限不足 |
| `404` | リソースが存在しない |
| `429` | レートリミット超過 |
| `500` | サーバ内部エラー |

以上。
