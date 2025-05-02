# kabuステーション REST API v1.5 – 完全リファレンス

このドキュメントは OpenAPI 定義を展開して **すべての情報** を Markdown に整形したものです。YAML ファイル無しで本ページだけで利用できます。

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

### POST /token
**トークン発行**
APIトークンを発行します。
 発行したトークンは有効である限り使用することができ、リクエストごとに発行する必要はありません。
 発行されたAPIトークンは以下のタイミングで無効となります。
 ・kabuステーションを終了した時
 ・kabuステーションからログアウトした時
 ・別のトークンが新たに発行された時
 ※kabuステーションは早朝、強制的にログアウトいたしますのでご留意ください。

#### Request Body (application/json)

| Field | Type | Description |
|-------|------|-------------|
| APIPassword | string | APIパスワード |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| ResultCode | integer | 結果コード
0が成功。それ以外はエラーコード。 |
| Token | string | APIトークン |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

## 列挙値リファレンス

以下は API で頻出するコード類や列挙値の対照表です。エンドポイント詳細と合わせて参照してください。

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

### ランキング種別 (Type)

| 値 | 説明 |
|----|------|
| 1 | 値上がり率 |
| 2 | 値下がり率 |
| 3 | 売買高上位 |
| 4 | 売買代金上位 |
| 5 | TICK 回数 |
| 6 | 業種別指数 (上昇率) |
| 7 | 信用売残増加 |
| 8 | 信用買残増加 |

### トレンド (ランキング内 `Trend`)

| 値 | 状態 |
|----|------|
| 0 | 対象データ無し |
| 1 | 過去 10 営業日より 20 位以上上昇 |
| 2 | 過去 10 営業日より 1〜19 位上昇 |
| 3 | 過去 10 営業日と変わらず |
| 4 | 過去 10 営業日より 1〜19 位下降 |
| 5 | 過去 10 営業日より 20 位以上下降 |

### 通貨ペア (`/exchange/{symbol}`)

| Symbol | ペア |
|--------|------|
| usdjpy | USD/JPY |
| eurjpy | EUR/JPY |
| gbpjpy | GBP/JPY |
| audjpy | AUD/JPY |

> 上記以外にも kabu ステーションで取り扱う通貨ペアが追加される場合があります。

##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### POST /sendorder
**注文発注（現物・信用）**
注文を発注します。
 同一の銘柄に対しての注文は同時に5件ほどを上限としてご利用ください。

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |

#### Request Body (application/json)

| Field | Type | Description |
|-------|------|-------------|
| Symbol | string | 銘柄コード |
| Exchange | integer | 市場コード |
| SecurityType | integer | 商品種別 |
| Side | string | 売買区分 |
| CashMargin | integer | 信用区分 |
| MarginTradeType | integer | 信用取引区分
※現物取引の場合は指定不要。
※信用取引の場合、必須。 |
| MarginPremiumUnit | number | １株あたりのプレミアム料(円)
 ※プレミアム料の刻値は、プレミアム料取得APIのレスポンスにある"TickMarginPremium"にてご確認ください。
 ※入札受付中(19:30～20:30)プレミアム料入札可能銘柄の場合、「MarginPremiumUnit」は必須となります。
 ※入札受付中(19:30～20:30)のプレミアム料入札可能銘柄以外の場合は、「MarginPremiumUnit」の記載は無視されます。
 ※入札受付中以外の時間帯では、「MarginPremiumUnit」の記載は無視されます。 |
| DelivType | integer | 受渡区分
※現物買は指定必須。
※現物売は「0(指定なし)」を設定
※信用新規は「0(指定なし)」を設定
※信用返済は指定必須 
※auマネーコネクトが有効の場合にのみ、「3」を設定可能 |
| FundType | string | 資産区分（預り区分）
※現物買は、指定必須。
※現物売は、「' '」 半角スペース2つを指定必須。
※信用新規と信用返済は、「11」を指定するか、または指定なしでも可。指定しない場合は「11」が自動的にセットされます。 |
| AccountType | integer | 口座種別 |
| Qty | integer | 注文数量
※信用一括返済の場合、返済したい合計数量を入力してください。 |
| ClosePositionOrder | integer | 決済順序
※信用返済の場合、必須。
※ClosePositionOrderとClosePositionsはどちらか一方のみ指定可能。
※ClosePositionOrderとClosePositionsを両方指定した場合、エラー。 |
| ClosePositions | array | 返済建玉指定
※信用返済の場合、必須。
※ClosePositionOrderとClosePositionsはどちらか一方のみ指定可能。
※ClosePositionOrderとClosePositionsを両方指定した場合、エラー。
※信用一括返済の場合、各建玉IDと返済したい数量を入力してください。
※建玉IDは「E」から始まる番号です。 |
| HoldID | string | 返済建玉ID |
| Qty | integer | 返済建玉数量 |
| FrontOrderType | integer | 執行条件 ※SOR以外は以下、全て指定可能です。 |
| Price | number | 注文価格
※FrontOrderTypeで成行を指定した場合、0を指定する。
※詳細について、”FrontOrderType”をご確認ください。 |
| ExpireDay | integer | 注文有効期限
 yyyyMMdd形式。
 「0」を指定すると、kabuステーション上の発注画面の「本日」に対応する日付として扱います。
 「本日」は直近の注文可能日となり、以下のように設定されます。
 引けまでの間 : 当日
 引け後 : 翌取引所営業日
 休前日 : 休日明けの取引所営業日
 ※ 日替わりはkabuステーションが日付変更通知を受信したタイミングです。 |
| ReverseLimitOrder | object | 逆指値条件
 ※FrontOrderTypeで逆指値を指定した場合のみ必須。 |
| TriggerSec | integer | トリガ銘柄
 ※未設定の場合はエラーになります。 |
| TriggerPrice | number | トリガ価格
 ※未設定の場合はエラーになります。
 ※数字以外が設定された場合はエラーになります。 |
| UnderOver | integer | 以上／以下
 ※未設定の場合はエラーになります。
 ※1、2以外が指定された場合はエラーになります。 |
| AfterHitOrderType | integer | ヒット後執行条件
 ※未設定の場合はエラーになります。
 ※1、2、3以外が指定された場合はエラーになります。 |
| AfterHitPrice | number | ヒット後注文価格
 ※未設定の場合はエラーになります。
 ※数字以外が設定された場合はエラーになります。

 ヒット後執行条件に従い、下記のようにヒット後注文価格を設定してください。	 |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| Result | integer | 結果コード
0が成功。それ以外はエラーコード。 |
| OrderId | string | 受付注文番号 |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### POST /sendorder/future
**注文発注（先物）**
先物銘柄の注文を発注します。
 同一の銘柄に対しての注文は同時に5件ほどを上限としてご利用ください。

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |

#### Request Body (application/json)

| Field | Type | Description |
|-------|------|-------------|
| Symbol | string | 銘柄コード
※取引最終日に「先物銘柄コード取得」でDerivMonthに0（直近限月）を指定した場合、日中・夜間の時間帯に関わらず、取引最終日を迎える限月の銘柄コードを返します。取引最終日を迎える銘柄の取引は日中取引をもって終了となりますので、ご注意ください。 |
| Exchange | integer | 市場コード SOR日中、SOR夜間、SOR日通しは一部銘柄のみ対象となります。
 SOR対象銘柄は以下をご参照ください。 |
| TradeType | integer | 取引区分 |
| TimeInForce | integer | 有効期間条件 ※執行条件(FrontOrderType)、有効期限条件(TimeInForce)、市場コード(Exchange)で選択できる組み合わせは下表のようになります。
 
■日中、夜間、日通し対応表 
■SOR日中、SOR夜間、SOR日通し対応表 |
| Side | string | 売買区分 |
| Qty | integer | 注文数量 |
| ClosePositionOrder | integer | 決済順序
※ClosePositionOrderとClosePositionsはどちらか一方のみ指定可能。
※ClosePositionOrderとClosePositionsを両方指定した場合、エラー。 |
| ClosePositions | array | 返済建玉指定
※ClosePositionOrderとClosePositionsはどちらか一方のみ指定可能。
※ClosePositionOrderとClosePositionsを両方指定した場合、エラー。 |
| HoldID | string | 返済建玉ID |
| Qty | integer | 返済建玉数量 |
| FrontOrderType | integer | 執行条件 |
| Price | number | 注文価格
※FrontOrderTypeで成行を指定した場合、0を指定する。
※詳細について、”FrontOrderType”をご確認ください。 |
| ExpireDay | integer | 注文有効期限
 yyyyMMdd形式。
 「0」を指定すると、kabuステーション上の発注画面の「本日」に対応する日付として扱います。
 「本日」は直近の注文可能日となり、以下のように設定されます。
 その市場の引けまでの間 : 当日
 その市場の引け後 : 翌取引所営業日
 その市場の休前日 : 休日明けの取引所営業日
 ※ 日替わりはkabuステーションが日付変更通知を受信したタイミングです。
 ※ 日通しの場合、夜間取引の引け後に日付が更新されます。 |
| ReverseLimitOrder | object | 逆指値条件
 ※FrontOrderTypeで逆指値を指定した場合のみ必須。 |
| TriggerPrice | number | トリガ価格
 ※未設定の場合はエラーになります。
 ※数字以外が設定された場合はエラーになります。 |
| UnderOver | integer | 以上／以下
 ※未設定の場合はエラーになります。
 ※1、2以外が指定された場合はエラーになります。 |
| AfterHitOrderType | integer | ヒット後執行条件
 ※未設定の場合はエラーになります。
 ※日通の注文で2以外が指定された場合はエラーになります。
 ※日中、夜間の注文で1、2以外が指定された場合はエラーになります。
 ※逆指値（成行）で有効期間条件(TimeInForce)にFAK以外を指定された場合はエラーになります。
 ※逆指値（指値）で有効期間条件(TimeInForce)にFAS以外を指定された場合はエラーになります。 |
| AfterHitPrice | number | ヒット後注文価格
 ※未設定の場合はエラーになります。
 ※数字以外が設定された場合はエラーになります。

 ヒット後執行条件に従い、下記のようにヒット後注文価格を設定してください。	 |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| Result | integer | 結果コード
0が成功。それ以外はエラーコード。 |
| OrderId | string | 受付注文番号 |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### POST /sendorder/option
**注文発注（オプション）**
オプション銘柄の注文を発注します。
 同一の銘柄に対しての注文は同時に5件ほどを上限としてご利用ください。

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |

#### Request Body (application/json)

| Field | Type | Description |
|-------|------|-------------|
| Symbol | string | 銘柄コード
※取引最終日に「オプション銘柄コード取得」でDerivMonthに0（直近限月）を指定した場合、日中・夜間の時間帯に関わらず、取引最終日を迎える限月の銘柄コードを返します。取引最終日を迎える銘柄の取引は日中取引をもって終了となりますので、ご注意ください。 |
| Exchange | integer | 市場コード |
| TradeType | integer | 取引区分 |
| TimeInForce | integer | 有効期間条件 ※執行条件(FrontOrderType)、有効期限条件(TimeInForce)、市場コード(Exchange)で選択できる組み合わせは下表のようになります。 |
| Side | string | 売買区分 |
| Qty | integer | 注文数量 |
| ClosePositionOrder | integer | 決済順序
※ClosePositionOrderとClosePositionsはどちらか一方のみ指定可能。
※ClosePositionOrderとClosePositionsを両方指定した場合、エラー。 |
| ClosePositions | array | 返済建玉指定
※ClosePositionOrderとClosePositionsはどちらか一方のみ指定可能。
※ClosePositionOrderとClosePositionsを両方指定した場合、エラー。 |
| HoldID | string | 返済建玉ID |
| Qty | integer | 返済建玉数量 |
| FrontOrderType | integer | 執行条件 |
| Price | number | 注文価格
※FrontOrderTypeで成行を指定した場合、0を指定する。
※詳細について、”FrontOrderType”をご確認ください。 |
| ExpireDay | integer | 注文有効期限
 yyyyMMdd形式。
 「0」を指定すると、kabuステーション上の発注画面の「本日」に対応する日付として扱います。
 「本日」は直近の注文可能日となり、以下のように設定されます。
 その市場の引けまでの間 : 当日
 その市場の引け後 : 翌取引所営業日
 その市場の休前日 : 休日明けの取引所営業日
 ※ 日替わりはkabuステーションが日付変更通知を受信したタイミングです。
 ※ 日通しの場合、夜間取引の引け後に日付が更新されます。 |
| ReverseLimitOrder | object | 逆指値条件
 ※FrontOrderTypeで逆指値を指定した場合のみ必須。 |
| TriggerPrice | number | トリガ価格
 ※未設定の場合はエラーになります。
 ※数字以外が設定された場合はエラーになります。 |
| UnderOver | integer | 以上／以下
 ※未設定の場合はエラーになります。
 ※1、2以外が指定された場合はエラーになります。 |
| AfterHitOrderType | integer | ヒット後執行条件
 ※未設定の場合はエラーになります。
 ※日通の注文で2以外が指定された場合はエラーになります。
 ※日中、夜間の注文で1、2以外が指定された場合はエラーになります。
 ※逆指値（成行）で有効期間条件(TimeInForce)にFAK以外を指定された場合はエラーになります。
 ※逆指値（指値）で有効期間条件(TimeInForce)にFAS以外を指定された場合はエラーになります。 |
| AfterHitPrice | number | ヒット後注文価格
 ※未設定の場合はエラーになります。
 ※数字以外が設定された場合はエラーになります。

 ヒット後執行条件に従い、下記のようにヒット後注文価格を設定してください。	 |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| Result | integer | 結果コード
0が成功。それ以外はエラーコード。 |
| OrderId | string | 受付注文番号 |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### PUT /cancelorder
**注文取消**
注文を取消します

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |

#### Request Body (application/json)

| Field | Type | Description |
|-------|------|-------------|
| OrderId | string | 注文番号
sendorderのレスポンスで受け取るOrderID。 |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| Result | integer | 結果コード
0が成功。それ以外はエラーコード。 |
| OrderId | string | 受付注文番号 |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /wallet/cash
**取引余力（現物）**
口座の取引余力（現物）を取得します

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| StockAccountWallet | number | 現物買付可能額
 ※auマネーコネクトが有効の場合、auじぶん銀行の残高を含めた合計可能額を表示する
 ※auマネーコネクトが無効の場合、三菱UFJ eスマート証券の可能額のみを表示する |
| AuKCStockAccountWallet | number | うち、三菱UFJ eスマート証券可能額 |
| AuJbnStockAccountWallet | number | うち、auじぶん銀行残高
※auマネーコネクトが無効の場合、「0」を表示する |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /wallet/cash/{symbol}
**取引余力（現物）（銘柄指定）**
指定した銘柄の取引余力（現物）を取得します

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |
| symbol | path | ✓ | string | 銘柄コード 
 ※次の形式で入力してください。
 [銘柄コード]@[市場コード]
 ※市場コードは下記の定義値から選択してください。 <b>市場コード</b> | |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| StockAccountWallet | number | 現物買付可能額
 ※auマネーコネクトが有効の場合、auじぶん銀行の残高を含めた合計可能額を表示する
 ※auマネーコネクトが無効の場合、三菱UFJ eスマート証券の可能額のみを表示する |
| AuKCStockAccountWallet | number | うち、三菱UFJ eスマート証券可能額 |
| AuJbnStockAccountWallet | number | うち、auじぶん銀行残高
※auマネーコネクトが無効の場合、「0」を表示する |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /wallet/margin
**取引余力（信用）**
口座の取引余力（信用）を取得します

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| MarginAccountWallet | number | 信用新規可能額 |
| DepositkeepRate | number | 保証金維持率
※銘柄指定の場合のみ
※銘柄が指定されなかった場合、0.0を返す。 |
| ConsignmentDepositRate | number | 委託保証金率
※銘柄指定の場合のみ。
※銘柄が指定されなかった場合、Noneを返す。 |
| CashOfConsignmentDepositRate | number | 現金委託保証金率
※銘柄指定の場合のみ。
※銘柄が指定されなかった場合、Noneを返す。 |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /wallet/margin/{symbol}
**取引余力（信用）（銘柄指定）**
指定した銘柄の取引余力（信用）を取得します

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |
| symbol | path | ✓ | string | 銘柄コード 
 ※次の形式で入力してください。
 [銘柄コード]@[市場コード]
 ※市場コードは下記の定義値から選択してください。 <b>市場コード</b> | |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| MarginAccountWallet | number | 信用新規可能額 |
| DepositkeepRate | number | 保証金維持率
※銘柄指定の場合のみ
※銘柄が指定されなかった場合、0.0を返す。 |
| ConsignmentDepositRate | number | 委託保証金率
※銘柄指定の場合のみ。
※銘柄が指定されなかった場合、Noneを返す。 |
| CashOfConsignmentDepositRate | number | 現金委託保証金率
※銘柄指定の場合のみ。
※銘柄が指定されなかった場合、Noneを返す。 |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /wallet/future
**取引余力（先物）**
口座の取引余力（先物）を取得します

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| FutureTradeLimit | number | 新規建玉可能額 |
| MarginRequirement | number | 買い必要証拠金額
※銘柄指定の場合のみ。
※銘柄が指定されなかった場合、空を返す。 |
| MarginRequirementSell | number | 売り必要証拠金額
※銘柄指定の場合のみ。
※銘柄が指定されなかった場合、空を返す。 |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /wallet/future/{symbol}
**取引余力（先物）（銘柄指定）**
指定した銘柄の取引余力（先物）を取得します

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |
| symbol | path | ✓ | string | 銘柄コード 
 ※次の形式で入力してください。
 [銘柄コード]@[市場コード]
 ※市場コードは下記の定義値から選択してください。 ※SOR市場は取扱っておりませんのでご注意ください。<b>市場コード</b>
 | |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| FutureTradeLimit | number | 新規建玉可能額 |
| MarginRequirement | number | 買い必要証拠金額
※銘柄指定の場合のみ。
※銘柄が指定されなかった場合、空を返す。 |
| MarginRequirementSell | number | 売り必要証拠金額
※銘柄指定の場合のみ。
※銘柄が指定されなかった場合、空を返す。 |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /wallet/option
**取引余力（オプション）**
口座の取引余力（オプション）を取得します

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| OptionBuyTradeLimit | number | 買新規建玉可能額 |
| OptionSellTradeLimit | number | 売新規建玉可能額 |
| MarginRequirement | number | 必要証拠金額
※銘柄指定の場合のみ。
※銘柄が指定されなかった場合、空を返す。 |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /wallet/option/{symbol}
**取引余力（オプション）（銘柄指定）**
指定した銘柄の取引余力（オプション）を取得します

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |
| symbol | path | ✓ | string | 銘柄コード 
 ※次の形式で入力してください。
 [銘柄コード]@[市場コード]
 ※市場コードは下記の定義値から選択してください。 <b>市場コード</b> | |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| OptionBuyTradeLimit | number | 買新規建玉可能額 |
| OptionSellTradeLimit | number | 売新規建玉可能額 |
| MarginRequirement | number | 必要証拠金額
※銘柄指定の場合のみ。
※銘柄が指定されなかった場合、空を返す。 |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /board/{symbol}
**時価情報・板情報**
指定した銘柄の時価情報・板情報を取得します
 レスポンスの一部にnullが発生した場合、該当銘柄を銘柄登録をしてから、 
再度時価情報・板情報APIを実行してください。

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |
| symbol | path | ✓ | string | 銘柄コード 
 ※次の形式で入力してください。
 [銘柄コード]@[市場コード]
 ※市場コードは下記の定義値から選択してください。
 ※SOR市場は取扱っておりませんのでご注意ください。<b>市場コード</b>
 | |

#### Responses

##### 200
※①：レスポンスにある「Bid」と「Ask」は、本来の意味である「買気配」と「売気配」と逆になっております。実際に返却される値は日本語の説明に準じたものになりますので、ご注意いただきますようお願い申し上げます。ご迷惑をおかけしまして、誠に申し訳ございません。
影響するキー名：
BidQty, BidPrice, BidTime, BidSign
AskQty, AskPrice, AskTime, AskSign
| Field | Type | Description |
|-------|------|-------------|
| Symbol | string | 銘柄コード |
| SymbolName | string | 銘柄名 |
| Exchange | integer | 市場コード
※株式・先物・オプション銘柄の場合のみ |
| ExchangeName | string | 市場名称
※株式・先物・オプション銘柄の場合のみ |
| CurrentPrice | number | 現値 |
| CurrentPriceTime | string | 現値時刻 |
| CurrentPriceChangeStatus | string | 現値前値比較 |
| CurrentPriceStatus | integer | 現値ステータス |
| CalcPrice | number | 計算用現値 |
| PreviousClose | number | 前日終値 |
| PreviousCloseTime | string | 前日終値日付 |
| ChangePreviousClose | number | 前日比 |
| ChangePreviousClosePer | number | 騰落率 |
| OpeningPrice | number | 始値 |
| OpeningPriceTime | string | 始値時刻 |
| HighPrice | number | 高値 |
| HighPriceTime | string | 高値時刻 |
| LowPrice | number | 安値 |
| LowPriceTime | string | 安値時刻 |
| TradingVolume | number | 売買高
※株式・先物・オプション銘柄の場合のみ |
| TradingVolumeTime | string | 売買高時刻
※株式・先物・オプション銘柄の場合のみ |
| VWAP | number | 売買高加重平均価格（VWAP）
※株式・先物・オプション銘柄の場合のみ |
| TradingValue | number | 売買代金
※株式・先物・オプション銘柄の場合のみ |
| BidQty | number | 最良売気配数量 ※①
※株式・先物・オプション銘柄の場合のみ |
| BidPrice | number | 最良売気配値段 ※①
※株式・先物・オプション銘柄の場合のみ |
| BidTime | string | 最良売気配時刻 ※①
※株式銘柄の場合のみ |
| BidSign | string | 最良売気配フラグ ※①
※株式・先物・オプション銘柄の場合のみ |
| MarketOrderSellQty | number | 売成行数量
※株式銘柄の場合のみ |
| Sell1 | object | 売気配数量1本目 |
| Time | string | 時刻
※株式銘柄の場合のみ |
| Sign | string | 気配フラグ
※株式・先物・オプション銘柄の場合のみ |
| Price | number | 値段
※株式・先物・オプション銘柄の場合のみ |
| Qty | number | 数量
※株式・先物・オプション銘柄の場合のみ |
| Sell2 | object | 売気配数量2本目 |
| Sell3 | object | 売気配数量3本目 |
| Sell4 | object | 売気配数量4本目 |
| Sell5 | object | 売気配数量5本目 |
| Sell6 | object | 売気配数量6本目 |
| Sell7 | object | 売気配数量7本目 |
| Sell8 | object | 売気配数量8本目 |
| Sell9 | object | 売気配数量9本目 |
| Sell10 | object | 売気配数量10本目 |
| AskQty | number | 最良買気配数量 ※①
※株式・先物・オプション銘柄の場合のみ |
| AskPrice | number | 最良買気配値段 ※①
※株式・先物・オプション銘柄の場合のみ |
| AskTime | string | 最良買気配時刻 ※①
※株式銘柄の場合のみ |
| AskSign | string | 最良買気配フラグ ※①
※株式・先物・オプション銘柄の場合のみ |
| MarketOrderBuyQty | number | 買成行数量
※株式銘柄の場合のみ |
| Buy1 | object | 買気配数量1本目 |
| Buy2 | object | 買気配数量2本目 |
| Buy3 | object | 買気配数量3本目 |
| Buy4 | object | 買気配数量4本目 |
| Buy5 | object | 買気配数量5本目 |
| Buy6 | object | 買気配数量6本目 |
| Buy7 | object | 買気配数量7本目 |
| Buy8 | object | 買気配数量8本目 |
| Buy9 | object | 買気配数量9本目 |
| Buy10 | object | 買気配数量10本目 |
| OverSellQty | number | OVER気配数量
※株式銘柄の場合のみ |
| UnderBuyQty | number | UNDER気配数量
※株式銘柄の場合のみ |
| TotalMarketValue | number | 時価総額
※株式銘柄の場合のみ |
| ClearingPrice | number | 清算値
※先物銘柄の場合のみ |
| IV | number | インプライド・ボラティリティ
※オプション銘柄かつ日通しの場合のみ |
| Gamma | number | ガンマ
※オプション銘柄かつ日通しの場合のみ |
| Theta | number | セータ
※オプション銘柄かつ日通しの場合のみ |
| Vega | number | ベガ
※オプション銘柄かつ日通しの場合のみ |
| Delta | number | デルタ
※オプション銘柄かつ日通しの場合のみ |
| SecurityType | integer | 銘柄種別 |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /symbol/{symbol}
**銘柄情報**
指定した銘柄情報を取得します

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |
| symbol | path | ✓ | string | 銘柄コード 
 ※次の形式で入力してください。
 [銘柄コード]@[市場コード]
 ※市場コードは下記の定義値から選択してください。
 ※SOR市場は取扱っておりませんのでご注意ください。<b>市場コード</b>
 | |
| addinfo | query | | string | 追加情報出力フラグ（未指定時：true）
 ※追加情報は、「時価総額」、「発行済み株式数」、「決算期日」、「清算値」を意味します。 | |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| Symbol | string | 銘柄コード |
| SymbolName | string | 銘柄名 |
| DisplayName | string | 銘柄略称
※株式・先物・オプション銘柄の場合のみ |
| Exchange | integer | 市場コード
※株式・先物・オプション銘柄の場合のみ |
| ExchangeName | string | 市場名称
※株式・先物・オプション銘柄の場合のみ |
| BisCategory | string | 業種コード名
※株式銘柄の場合のみ |
| TotalMarketValue | number | 時価総額
※株式銘柄の場合のみ
追加情報出力フラグ：falseの場合、null |
| TotalStocks | number | 発行済み株式数（千株）
※株式銘柄の場合のみ
追加情報出力フラグ：falseの場合、null |
| TradingUnit | number | 売買単位
※株式・先物・オプション銘柄の場合のみ |
| FiscalYearEndBasic | integer | 決算期日
※株式銘柄の場合のみ
追加情報出力フラグ：falseの場合、null |
| PriceRangeGroup | string | 呼値グループ
 ※株式・先物・オプション銘柄の場合のみ
 ※各呼値コードが対応する商品は以下となります。
 　株式の呼値の単位の詳細は [JPXページ](https://www.jpx.co.jp/equities/trading/domestic/07.html) をご覧ください。
 　10000：株式(TOPIX500採用銘柄以外)　
 　10003：株式(TOPIX500採用銘柄)
 　10118 : 日経平均先物
 　10119 : 日経225mini
 　10318 : 日経平均オプション
 　10706 : ﾐﾆTOPIX先物
 　10718 : TOPIX先物
 　12122 : JPX日経400指数先物
 　14473 : NYダウ先物
 　14515 : 日経平均VI先物
 　15411 : グロース250先物
 　15569 : 東証REIT指数先物
 　17163 : TOPIXCore30指数先物
 |
| KCMarginBuy | boolean | 一般信用買建フラグ
※trueのとき、一般信用(長期)または一般信用(デイトレ)が買建可能
※株式銘柄の場合のみ |
| KCMarginSell | boolean | 一般信用売建フラグ
※trueのとき、一般信用(長期)または一般信用(デイトレ)が売建可能
※株式銘柄の場合のみ |
| MarginBuy | boolean | 制度信用買建フラグ
※trueのとき制度信用買建可能
※株式銘柄の場合のみ |
| MarginSell | boolean | 制度信用売建フラグ
※trueのとき制度信用売建可能
※株式銘柄の場合のみ |
| UpperLimit | number | 値幅上限
※株式・先物・オプション銘柄の場合のみ |
| LowerLimit | number | 値幅下限
※株式・先物・オプション銘柄の場合のみ |
| Underlyer | string | 原資産コード
※先物・オプション銘柄の場合のみ |
| DerivMonth | string | 限月-年月
※「限月-年月」は「年(yyyy)/月(MM)」で表示します。
※先物・オプション銘柄の場合のみ |
| TradeStart | integer | 取引開始日
※先物・オプション銘柄の場合のみ |
| TradeEnd | integer | 取引終了日
※先物・オプション銘柄の場合のみ |
| StrikePrice | number | 権利行使価格
※オプション銘柄の場合のみ |
| PutOrCall | integer | プット/コール区分
※オプション銘柄の場合のみ |
| ClearingPrice | number | 清算値
※先物銘柄の場合のみ
追加情報出力フラグ：falseの場合、null |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /orders
**注文約定照会**
注文一覧を取得します。
 ※下記Queryパラメータは任意設定となります。

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |
| product | query | | string | 取得する商品 | 0,1,2,3,4 |
| id | query | | string | 注文番号
 ※指定された注文番号と一致する注文のみレスポンスします。
 ※指定された注文番号との比較では大文字小文字を区別しません。
 ※複数の注文番号を指定することはできません。 | |
| updtime | query | | string | 更新日時
 ※形式：yyyyMMddHHmmss （例：20201201123456）
 ※指定された更新日時以降（指定日時含む）に更新された注文のみレスポンスします。
 ※複数の更新日時を指定することはできません。 | |
| details | query | | string | 注文詳細抑止 | |
| symbol | query | | string | 銘柄コード
※指定された銘柄コードと一致する注文のみレスポンスします。
※複数の銘柄コードを指定することができません。 | |
| state | query | | string | 状態
 ※指定された状態と一致する注文のみレスポンスします。
 ※フィルタには数字の入力のみ受け付けます。
 ※複数の状態を指定することはできません。 | 1,2,3,4,5 |
| side | query | | string | 売買区分
 ※指定された売買区分と一致する注文のみレスポンスします。
 ※フィルタには数字の入力のみ受け付けます。
 ※複数の売買区分を指定することができません。 | 1,2 |
| cashmargin | query | | string | 取引区分
 ※指定された取引区分と一致する注文のみレスポンスします。
 ※フィルタには数字の入力のみ受け付けます。
 ※複数の取引区分を指定することができません。 | 2,3 |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| ID | string | 注文番号 |
| State | integer | 状態
 ※OrderStateと同一である |
| OrderState | integer | 注文状態
 ※Stateと同一である |
| OrdType | integer | 執行条件 |
| RecvTime | string | 受注日時 |
| Symbol | string | 銘柄コード |
| SymbolName | string | 銘柄名 |
| Exchange | integer | 市場コード |
| ExchangeName | string | 市場名 |
| TimeInForce | integer | 有効期間条件
※先物・オプション銘柄の場合のみ |
| Price | number | 値段 |
| OrderQty | number | 発注数量
 ※注文期限切れと失効の場合、OrderQtyはゼロになりません。
 ※期限切れと失効の確認方法としては、DetailsのRecType（3: 期限切れ、7: 失効）にてご確認ください。 |
| CumQty | number | 約定数量 |
| Side | string | 売買区分 |
| CashMargin | integer | 取引区分 |
| AccountType | integer | 口座種別 |
| DelivType | integer | 受渡区分 |
| ExpireDay | integer | 注文有効期限
yyyyMMdd形式 |
| MarginTradeType | integer | 信用取引区分
 ※信用を注文した際に表示されます。 |
| MarginPremium | number | プレミアム料
 ※（注文中数量＋約定済数量）×１株あたりプレミアム料として計算されます。
 ※信用を注文した際に表示されます。
 ※制度信用売/買、一般（長期）買、一般（デイトレ）買の場合は、Noneと返されます。
 一般（長期）売、一般（デイトレ）売の場合は、プレミアム料=0の場合、0（ゼロ）と返されます。 |
| Details | array | 注文詳細 |
| SeqNum | integer | ※注文明細レコードの生成順序です。
※通番であるとは限りませんが、大小による順序は保たれています。 |
| ID | string | 注文詳細番号 |
| RecType | integer | 明細種別 |
| ExchangeID | string | 取引所番号 |
| State | integer | 状態 |
| TransactTime | string | 処理時刻 |
| OrdType | integer | 執行条件 |
| Price | number | 値段 |
| Qty | number | 数量 |
| ExecutionID | string | 約定番号 |
| ExecutionDay | string | 約定日時 |
| DelivDay | integer | 受渡日 |
| Commission | number | 手数料
※注文詳細の明細種別が約定（RecType=8)の場合に設定。 |
| CommissionTax | number | 手数料消費税
※明細種別は約定（RecType=8）の場合にのみ表示されます。 |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /positions
**残高照会**
残高一覧を取得します。
※下記Queryパラメータは任意設定となります。

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |
| product | query | | string | 取得する商品 | 0,1,2,3,4 |
| symbol | query | | string | 銘柄コード
※指定された銘柄コードと一致するポジションのみレスポンスします。
※複数の銘柄コードを指定することはできません。 | |
| side | query | | string | 売買区分フィルタ
 指定された売買区分と一致する注文を返す | 1,2 |
| addinfo | query | | string | 追加情報出力フラグ（未指定時：true）
 ※追加情報は、「現在値」、「評価金額」、「評価損益額」、「評価損益率」を意味します。 | |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| ExecutionID | string | 約定番号
※現物取引では、nullが返ります。 |
| AccountType | integer | 口座種別 |
| Symbol | string | 銘柄コード |
| SymbolName | string | 銘柄名 |
| Exchange | integer | 市場コード |
| ExchangeName | string | 市場名 |
| SecurityType | integer | 銘柄種別
※先物・オプション銘柄の場合のみ |
| ExecutionDay | integer | 約定日（建玉日）
※信用・先物・オプションの場合のみ
※現物取引では、nullが返ります。 |
| Price | number | 値段 |
| LeavesQty | number | 残数量（保有数量） |
| HoldQty | number | 拘束数量（返済のために拘束されている数量） |
| Side | string | 売買区分 |
| Expenses | number | 諸経費
※信用・先物・オプションの場合のみ |
| Commission | number | 手数料
※信用・先物・オプションの場合のみ |
| CommissionTax | number | 手数料消費税
※信用・先物・オプションの場合のみ |
| ExpireDay | integer | 返済期日
※信用・先物・オプションの場合のみ |
| MarginTradeType | integer | 信用取引区分
※信用の場合のみ |
| CurrentPrice | number | 現在値
追加情報出力フラグ：falseの場合、null |
| Valuation | number | 評価金額
追加情報出力フラグ：falseの場合、null |
| ProfitLoss | number | 評価損益額
追加情報出力フラグ：falseの場合、null |
| ProfitLossRate | number | 評価損益率
追加情報出力フラグ：falseの場合、null |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### PUT /register
**銘柄登録**
PUSH配信する銘柄を登録します。
 API登録銘柄リストを開くには、kabuステーションAPIインジケーターを右クリックし「API登録銘柄リスト」を選択してください。

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |

#### Request Body (application/json)

| Field | Type | Description |
|-------|------|-------------|
| Symbols | array | |
| Symbol | string | 銘柄コード |
| Exchange | integer | 市場コード |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| RegistList | array | 現在登録されている銘柄のリスト |
| Symbol | string | 銘柄コード |
| Exchange | integer | 市場コード |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### PUT /unregister
**銘柄登録解除**
API登録銘柄リストに登録されている銘柄を解除します

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |

#### Request Body (application/json)

| Field | Type | Description |
|-------|------|-------------|
| Symbols | array | ※為替銘柄を登録する場合、銘柄名は"通貨A" + "/" + "通貨B"、市場コードは"300"で指定してください。 例：'Symbol': 'EUR/USD', "Exchange": 300 |
| Symbol | string | 銘柄コード |
| Exchange | integer | 市場コード |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| RegistList | array | 現在登録されている銘柄のリスト |
| Symbol | string | 銘柄コード |
| Exchange | integer | 市場コード |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### PUT /unregister/all
**銘柄登録全解除**
API登録銘柄リストに登録されている銘柄をすべて解除します

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| RegistList | object | 現在登録されている銘柄のリスト
※銘柄登録解除が正常に行われれば、空リストを返します。
　登録解除でエラー等が発生した場合、現在登録されている銘柄のリストを返します |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /symbolname/future
**先物銘柄コード取得**
先物銘柄コード取得

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |
| FutureCode | query | | string | 先物コード
 ※大文字小文字は区別しません。 | |
| DerivMonth | query | ✓ | integer | 限月
 ※限月はyyyyMM形式で指定します。0を指定した場合、直近限月となります。
 ※取引最終日に「0」（直近限月）を指定した場合、日中・夜間の時間帯に関わらず、 取引最終日を迎える限月の銘柄コードを返します。取引最終日を迎える銘柄の取引は日中取引をもって終了となりますので、ご注意ください。
 | |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| Symbol | string | 銘柄コード |
| SymbolName | string | 銘柄名称 |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /symbolname/option
**オプション銘柄コード取得**
オプション銘柄コード取得

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |
| OptionCode | query | | string | オプションコード
 ※指定なしの場合は、日経225オプションを対象とする。
 | |
| DerivMonth | query | ✓ | integer | 限月
※限月はyyyyMM形式で指定します。0を指定した場合、直近限月となります。
※取引最終日に「0」（直近限月）を指定した場合、日中・夜間の時間帯に関わらず、取引最終日を迎える限月の銘柄コードを返します。取引最終日を迎える銘柄の取引は日中取引をもって終了となりますので、ご注意ください。 | |
| PutOrCall | query | ✓ | string | コール or プット
 ※大文字小文字は区別しません。 | |
| StrikePrice | query | ✓ | integer | 権利行使価格
※0を指定した場合、APIを実行した時点でのATMとなります。 | |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| Symbol | string | 銘柄コード |
| SymbolName | string | 銘柄名称 |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /symbolname/minioptionweekly
**ミニオプション（限週）銘柄コード取得**
ミニオプション（限週）銘柄コード取得

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |
| DerivMonth | query | ✓ | integer | 限月
※限月はyyyyMM形式で指定します。0を指定した場合、直近限月となります。
※取引最終日に「0」（直近限月）を指定した場合、日中・夜間の時間帯に関わらず、取引最終日を迎える限月の銘柄コードを返します。取引最終日を迎える銘柄の取引は日中取引をもって終了となりますので、ご注意ください。 | |
| DerivWeekly | query | ✓ | integer | 限週
※限週は0,1,3,4,5のいずれかを指定します。0を指定した場合、指定した限月の直近限週となります。
※取引最終日に「0」（直近限週）を指定した場合、日中・夜間の時間帯に関わらず、取引最終日を迎える限週の銘柄コードを返します。取引最終日を迎える銘柄の取引は日中取引をもって終了となりますので、ご注意ください。 | |
| PutOrCall | query | ✓ | string | コール or プット
 ※大文字小文字は区別しません。 | |
| StrikePrice | query | ✓ | integer | 権利行使価格
※0を指定した場合、APIを実行した時点でのATMとなります。 | |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| Symbol | string | 銘柄コード |
| SymbolName | string | 銘柄名称 |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /ranking
**詳細ランキング**
詳細ランキング画面と同様の各種ランキングを返します。 
ランキングの対象日はkabuステーションが保持している当日のデータとなります。 
※株価情報ランキング、業種別指数ランキングは、下記の時間帯でデータがクリアされるため、 
その間の詳細ランキングAPIは空レスポンスとなります。 
データクリア：平日7:53頃-9:00過ぎ頃 
※信用情報ランキングは毎週第３営業日の7:55頃にデータが更新されます。

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |
| Type | query | ✓ | string | 種別
 ※信用情報ランキングに「福証」「札証」を指定した場合は、空レスポンスになります | 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15 |
| ExchangeDivision | query | ✓ | string | 市場
 ※業種別値上がり率・値下がり率に市場を指定しても無視されます | ALL,T,TP,TS,TG,M,FK,S |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /exchange/{symbol}
**為替情報**
マネービューの情報を取得する

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |
| symbol | path | ✓ | string | 通貨 | usdjpy,eurjpy,gbpjpy,audjpy,chfjpy,cadjpy,nzdjpy,zarjpy,eurusd,gbpusd,audusd |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| Symbol | string | 通貨 |
| BidPrice | number | BID |
| Spread | number | SP |
| AskPrice | number | ASK |
| Change | number | 前日比 |
| Time | string | 時刻 
※HH:mm:ss形式 |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /regulations/{symbol}
**規制情報**
規制情報＋空売り規制情報を取得する

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |
| symbol | path | ✓ | string | 銘柄コード 
 ※次の形式で入力してください。
 [銘柄コード]@[市場コード]
 ※市場コードは下記の定義値から選択してください。 <b>市場コード</b> | |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| Symbol | string | 銘柄コード
 ※対象商品は、株式のみ |
| RegulationsInfo | array | 規制情報 |
| Exchange | integer | 規制市場 |
| Product | integer | 規制取引区分
 ※空売り規制の場合、「4：新規」 |
| Side | string | 規制売買
 ※空売り規制の場合、「1：売」 |
| Reason | string | 理由
※空売り規制の場合、「空売り規制」 |
| LimitStartDay | string | 制限開始日
yyyy/MM/dd HH:mm形式 
※空売り規制の場合、null |
| LimitEndDay | string | 制限終了日
yyyy/MM/dd HH:mm形式 
※空売り規制の場合、null |
| Level | integer | コンプライアンスレベル
 ※空売り規制の場合、null |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /primaryexchange/{symbol}
**優先市場**
株式の優先市場を取得する

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |
| symbol | path | ✓ | string | 銘柄コード | |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| Symbol | string | 銘柄コード
※対象商品は、株式のみ |
| PrimaryExchange | integer | 優先市場 |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /apisoftlimit
**ソフトリミット**
kabuステーションAPIのソフトリミット値を取得する

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| Stock | number | 現物のワンショット上限
※単位は万円 |
| Margin | number | 信用のワンショット上限
※単位は万円 |
| Future | number | 先物のワンショット上限
※単位は枚 |
| FutureMini | number | ミニ先物のワンショット上限
※単位は枚 |
| FutureMicro | number | マイクロ先物のワンショット上限
※単位は枚 |
| Option | number | オプションのワンショット上限
※単位は枚 |
| MiniOption | number | ミニオプションのワンショット上限
※単位は枚 |
| KabuSVersion | string | kabuステーションのバージョン |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |

---

### GET /margin/marginpremium/{symbol}
**プレミアム料取得**
指定した銘柄のプレミアム料を取得するAPI

#### パラメータ

| Name | In | Req | Type | Description | Enum |
|------|----|-----|------|-------------|------|
| X-API-KEY | header | ✓ | string | トークン発行メソッドで取得した文字列 | |
| symbol | path | ✓ | string | 銘柄コード | |

#### Responses

##### 200
OK
| Field | Type | Description |
|-------|------|-------------|
| Symbol | string | 銘柄コード |
| GeneralMargin | object | 一般信用（長期） |
| MarginPremiumType | integer | プレミアム料入力区分 |
| MarginPremium | number | 確定プレミアム料
 ※入札銘柄の場合、入札受付中は随時更新します。受付時間外は、確定したプレミアム料を返します。
 ※非入札銘柄の場合、常に固定値を返します。
 ※信用取引不可の場合、nullを返します。
 ※19:30~翌営業日のプレミアム料になります。 |
| UpperMarginPremium | number | 上限プレミアム料
 ※プレミアム料がない場合は、nullを返します。 |
| LowerMarginPremium | number | 下限プレミアム料
 ※プレミアム料がない場合は、nullを返します。 |
| TickMarginPremium | number | プレミアム料刻値
 ※入札可能銘柄以外は、nullを返します。 |
| DayTrade | object | 一般信用（デイトレ） |
##### 400
BadRequest
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 401
Unauthorized
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 403
Forbidden
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 404
NotFound
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 405
MethodNotAllowed
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 413
RequestEntityTooLarge
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 415
UnsupportedMediaType
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 429
TooManyRequests
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |
##### 500
InternalServerError
| Field | Type | Description |
|-------|------|-------------|
| Code | integer | エラーコード |
| Message | string | エラーメッセージ |