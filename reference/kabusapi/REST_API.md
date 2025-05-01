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
