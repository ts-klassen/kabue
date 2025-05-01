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

#### POST `/token`

**Request JSON** (`RequestToken`)

| Key | Type | Req | Description |
|-----|------|-----|-------------|
| APIPassword | string | ✓ | kabuステーションのAPIパスワード |

**Success** (`TokenSuccess`)

```json
{
  "ResultCode": 0,
  "Token": "a1b2c3d4..."
}
```

このトークンを以降すべてのリクエストヘッダー `X-API-KEY` に設定してください。

### 発注 (order)

| Method | Path | Summary |
|--------|------|---------|
| `POST` | `/sendorder` | 注文発注（現物・信用） |
| `POST` | `/sendorder/future` | 注文発注（先物） |
| `POST` | `/sendorder/option` | 注文発注（オプション） |
| `PUT`  | `/cancelorder` | 注文取消 |

---

#### POST `/sendorder`（現物・信用）

**Request JSON** (`RequestSendOrder`)

| Key | Type | Req | Description |
|-----|------|-----|-------------|
| Symbol | string | ✓ | 銘柄コード |
| Exchange | int | ✓ | 市場コード (1:東証/3:名証/5:福証/6:札証/9:SOR) |
| SecurityType | int | ✓ | 1:株式 |
| Side | string | ✓ | 1:売, 2:買 |
| CashMargin | int | ✓ | 1:現物, 2:信用新規, 3:信用返済 |
| MarginTradeType | int |  | 1:制度/2:一般長期/3:一般デイトレ |
| DelivType | int | ✓ | 0:指定なし, 2:お預り金, 3:auマネーコネクト |
| FundType | string |  | 資金区分 (`"  "`, `"02"`, …) |
| AccountType | int | ✓ | 2:特定, 4:一般, 12:法人 |
| Qty | int | ✓ | 発注数量 |
| ClosePositionOrder | int |  | 建玉整理順序 |
| ClosePositions | array |  | 個別建玉指定 ({ HoldID, Qty }) |
| FrontOrderType | int | ✓ | 10:成行, 20:指値 … |
| Price | int | ✓ | 注文価格 (成行時は0) |
| ExpireDay | int | ✓ | 有効期限 (YYYYMMDD, 当日0) |
| ReverseLimitOrder | object |  | 逆指値 ({ TriggerSec, TriggerPrice, UnderOver, AfterHitOrderType, AfterHitPrice }) |

成功レスポンス `OrderSuccess` → `{ "OrderId": "…", "Result": 0 }`

> ※発注系エンドポイントは秒間 5 req 程度のレート制限があります。


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

---

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

#### 共通

これらの取引余力 API は **リクエストボディを持ちません**。  
ヘッダー `X-API-KEY` のみ必須です。

| パス変数 | 説明 |
|-----------|------|
| `symbol`  | 銘柄コード@市場コード (`6502@1` など) |

成功レスポンスはそれぞれ `Wallet*Success` スキーマを参照してください。

---

### 情報エンドポイント詳細

#### GET `/board/{symbol}`

| パス変数 | 説明 | 必須 |
|-----------|------|------|
| `symbol` | 銘柄コード@市場コード | ✓ |

レスポンス: `BoardSuccess` – 板情報 + 時価情報。

---

#### GET `/symbol/{symbol}`

現物銘柄情報を取得。パス変数は同上。レスポンス `SymbolSuccess`。

#### GET `/symbol/{symbol}/future` / `/symbol/{symbol}/option`

先物・オプション銘柄情報。パス変数同上。レスポンス `SymbolFutureSuccess` / `SymbolOptionSuccess`。

---

#### GET `/exchange`

リクエストパラメータなし。取引所営業日/時間帯情報を返す (`ExchangeSuccess`)。

---

### 注文・建玉照会

#### GET `/orders`

クエリ

| Name | Type | 説明 |
|------|------|------|
| `product` | string | 0:すべて / 1:現物 / 2:信用 / 3:先物 / 4:OP |

レスポンス `OrdersSuccess` – `Orders` 配列。

#### GET `/orders/{id}`

詳細取得。`id` は `OrderId` 文字列。レスポンス `OrderDetailSuccess`。

---

#### GET `/positions`

クエリ

| Name | Type | 説明 |
|------|------|------|
| `product` | string | 0:すべて / 1:現物 / 2:信用 / 3:先物 / 4:OP |

レスポンス `PositionsSuccess` – `Positions` 配列。

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

#### PUT `/register`

リクエストボディ (`RequestRegister`)

| Key | Type | Req | Description |
|-----|------|-----|-------------|
| Symbols | array<object> |  | 登録する銘柄リスト (最大50) |
| &nbsp;&nbsp;└ Symbol | string | ✓ | 銘柄コード |
| &nbsp;&nbsp;└ Exchange | int | ✓ | 市場コード |

成功レスポンス: `201` Created – `RegistList` 配列に現在登録されている銘柄が返ります。

---

#### PUT `/unregister`

同じ payload 構造 (`RequestUnregister`) で指定銘柄のみ解除します。

---

#### PUT `/unregister/all`

リクエストボディなし – 登録済み銘柄をすべて解除します。

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
