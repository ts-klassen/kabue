# kabuステーション REST API v1.5 – クイックリファレンス

本ファイルは **エンドポイントの一覧** と **共通情報** のみをまとめた簡易版です。
詳細なリクエスト／レスポンス仕様・サンプル・フィールド説明は `FULL.md` を参照してください。

---

## エンドポイント一覧

| Method | Path | Summary |
|--------|------|---------|
| `POST` | `/token` | トークン発行 |
| `POST` | `/sendorder` | 注文発注（現物・信用） |
| `POST` | `/sendorder/future` | 注文発注（先物） |
| `POST` | `/sendorder/option` | 注文発注（オプション） |
| `PUT` | `/cancelorder` | 注文取消 |
| `GET` | `/wallet/cash` | 取引余力（現物） |
| `GET` | `/wallet/cash/{symbol}` | 取引余力（現物）（銘柄指定） |
| `GET` | `/wallet/margin` | 取引余力（信用） |
| `GET` | `/wallet/margin/{symbol}` | 取引余力（信用）（銘柄指定） |
| `GET` | `/wallet/future` | 取引余力（先物） |
| `GET` | `/wallet/future/{symbol}` | 取引余力（先物）（銘柄指定） |
| `GET` | `/wallet/option` | 取引余力（オプション） |
| `GET` | `/wallet/option/{symbol}` | 取引余力（オプション）（銘柄指定） |
| `GET` | `/board/{symbol}` | 時価情報・板情報 |
| `GET` | `/symbol/{symbol}` | 銘柄情報 |
| `GET` | `/orders` | 注文約定照会 |
| `GET` | `/positions` | 残高照会 |
| `PUT` | `/register` | 銘柄登録 |
| `PUT` | `/unregister` | 銘柄登録解除 |
| `PUT` | `/unregister/all` | 銘柄登録全解除 |
| `GET` | `/symbolname/future` | 先物銘柄コード取得 |
| `GET` | `/symbolname/option` | オプション銘柄コード取得 |
| `GET` | `/symbolname/minioptionweekly` | ミニオプション（限週）銘柄コード取得 |
| `GET` | `/ranking` | 詳細ランキング |
| `GET` | `/exchange/{symbol}` | 為替情報 |
| `GET` | `/regulations/{symbol}` | 規制情報 |
| `GET` | `/primaryexchange/{symbol}` | 優先市場 |
| `GET` | `/apisoftlimit` | ソフトリミット |
| `GET` | `/margin/marginpremium/{symbol}` | プレミアム料取得 |

---

## 共通エラーレスポンス

ほぼすべての API が下記のフォーマットでエラーを返します。フィールド定義は共通です。

| HTTP Status | 説明 | Schema |
|-------------|------|--------|
| 400 Bad Request | 不正なリクエスト | `{ "Code": integer, "Message": string }` |
| 401 Unauthorized | 認証エラー | 同上 |
| 403 Forbidden | 権限エラー | 同上 |
| 404 Not Found | リソースなし | 同上 |
| 405 Method Not Allowed | メソッド不許可 | 同上 |
| 413 Request Entity Too Large | リクエストサイズ超過 | 同上 |
| 415 Unsupported Media Type | Content-Type 不正 | 同上 |
| 429 Too Many Requests | レートリミット超過 | 同上 |
| 500 Internal Server Error | サーバ内部エラー | 同上 |

> `Code` には API 独自のエラーコードが、`Message` には日本語メッセージが返ります。詳細は FULL.md の「エラーメッセージ一覧」を参照してください。

---

## 代表的な列挙値

詳細な一覧は FULL.md に記載されていますが、利用頻度の高いものを抜粋して載せます。

### 市場コード (Exchange)

| 値 | 市場 |
|----|------|
| 1 | 東証 |
| 3 | 名証 |
| 5 | 福証 |
| 6 | 札証 |
| 9 | SOR |

### 商品区分 (product クエリ)

| 値 | 区分 |
|----|------|
| 0 | すべて |
| 1 | 現物 |
| 2 | 信用 |
| 3 | 先物 |
| 4 | オプション |

---

## 使い方

1. 呼び出したいエンドポイントを上の表から見つける。
2. 詳細仕様を確認する場合は `FULL.md` へジャンプして同じパスを検索。
3. 共通エラーは本ファイルの表を参照。

これにより、**REST_API.md は常時 1 画面程度で俯瞰**でき、詳しく調査したい場合のみ FULL.md に移動できます。
