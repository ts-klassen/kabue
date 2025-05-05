# kabuステーションAPI
**Version:** 1.5

# 定義情報 
 REST APIのコード一覧、エンドポイントは下記リンク参照  
  - [REST APIコード一覧](#message)

## Servers
* `http://localhost:18080/kabusapi` – 本番用
* `http://localhost:18081/kabusapi` – 検証用

## auth
### `POST` /token
**Summary:** トークン発行
APIトークンを発行します。

発行したトークンは有効である限り使用することができ、リクエストごとに発行する必要はありません。

発行されたAPIトークンは以下のタイミングで無効となります。

・kabuステーションを終了した時

・kabuステーションからログアウトした時

・別のトークンが新たに発行された時

※kabuステーションは早朝、強制的にログアウトいたしますのでご留意ください。

**Request body**: (required)
* application/json
    | field | type | required | description |
    |-------|------|----------|-------------|
    | APIPassword | string | True | APIパスワード; Example: xxxxxx |


**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | ResultCode | integer(int32) | False | 結果コード 0が成功。それ以外はエラーコード。; Example: 0 |
        | Token | string | False | APIトークン; Example: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx |

* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



## order
### `POST` /sendorder
**Summary:** 注文発注（現物・信用）
注文を発注します。

同一の銘柄に対しての注文は同時に5件ほどを上限としてご利用ください。
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |

**Request body**: (required)
* application/json
    | field | type | required | description |
    |-------|------|----------|-------------|
    | Symbol | string | True | 銘柄コード |
    | Exchange | integer(int32) | True | 市場コード ; Enum: 1=東証, 3=名証, 5=福証, 6=札証, 9=SOR |
    | SecurityType | integer(int32) | True | 商品種別 ; Enum: 1=株式 |
    | Side | string | True | 売買区分 ; Enum: 1=売, 2=買 |
    | CashMargin | integer(int32) | True | 信用区分 ; Enum: 1=現物, 2=新規, 3=返済 |
    | MarginTradeType | integer(int32) | False | 信用取引区分 ※現物取引の場合は指定不要。 ※信用取引の場合、必須。 ; Enum: 1=制度信用, 2=一般信用（長期）, 3=一般信用（デイトレ） |
    | MarginPremiumUnit | number(double) | False | １株あたりのプレミアム料(円)   ※プレミアム料の刻値は、プレミアム料取得APIのレスポンスにある"TickMarginPremium"にてご確認ください。  ※入札受付中(19:30～20:30)プレミアム料入札可能銘柄の場合、「MarginPremiumUnit」は必須となります。  ※入札受付中(19:30～20:30)のプレミアム料入札可能銘柄以外の場合は、「MarginPremiumUnit」の記載は無視されます。  ※入札受付中以外の時間帯では、「MarginPremiumUnit」の記載は無視されます。 |
    | DelivType | integer(int32) | True | 受渡区分 ※現物買は指定必須。 ※現物売は「0(指定なし)」を設定 ※信用新規は「0(指定なし)」を設定 ※信用返済は指定必須  ※auマネーコネクトが有効の場合にのみ、「3」を設定可能 ; Enum: 0=指定なし, 2=お預り金, 3=auマネーコネクト |
    | FundType | string | False | 資産区分（預り区分） ※現物買は、指定必須。 ※現物売は、「'  '」 半角スペース2つを指定必須。 ※信用新規と信用返済は、「11」を指定するか、または指定なしでも可。指定しない場合は「11」が自動的にセットされます。 ; Enum: (半角スペース2つ)=現物売の場合, 02=保護, AA=信用代用, 11=信用取引 |
    | AccountType | integer(int32) | True | 口座種別 ; Enum: 2=一般, 4=特定, 12=法人 |
    | Qty | integer(int32) | True | 注文数量 ※信用一括返済の場合、返済したい合計数量を入力してください。 |
    | ClosePositionOrder | integer(int32) | False | 決済順序 ※信用返済の場合、必須。 ※ClosePositionOrderとClosePositionsはどちらか一方のみ指定可能。 ※ClosePositionOrderとClosePositionsを両方指定した場合、エラー。 ; Enum: 0=日付（古い順）、損益（高い順）, 1=日付（古い順）、損益（低い順）, 2=日付（新しい順）、損益（高い順）, 3=日付（新しい順）、損益（低い順）, 4=損益（高い順）、日付（古い順）, 5=損益（高い順）、日付（新しい順）, 6=損益（低い順）、日付（古い順）, 7=損益（低い順）、日付（新しい順） |
    | ClosePositions | array<object> | False | 返済建玉指定 ※信用返済の場合、必須。 ※ClosePositionOrderとClosePositionsはどちらか一方のみ指定可能。 ※ClosePositionOrderとClosePositionsを両方指定した場合、エラー。 ※信用一括返済の場合、各建玉IDと返済したい数量を入力してください。 ※建玉IDは「E」から始まる番号です。 |
    | ClosePositions..HoldID | string | False | 返済建玉ID |
    | ClosePositions..Qty | integer(int32) | False | 返済建玉数量 |
    | FrontOrderType | integer(int32) | True | 執行条件 ※SOR以外は以下、全て指定可能です。 ; Enum: 10=成行, 13=寄成（前場）, 14=寄成（後場）, 15=引成（前場）, 16=引成（後場）, 17=IOC成行, 20=指値, 21=寄指（前場）, 22=寄指（後場）, 23=引指（前場）, 24=引指（後場）, 25=不成（前場）, 26=不成（後場）, 27=IOC指値, 30=逆指値 |
    | Price | number(double) | True | 注文価格 ※FrontOrderTypeで成行を指定した場合、0を指定する。 ※詳細について、”FrontOrderType”をご確認ください。 |
    | ExpireDay | integer(int32) | True | 注文有効期限  yyyyMMdd形式。  「0」を指定すると、kabuステーション上の発注画面の「本日」に対応する日付として扱います。  「本日」は直近の注文可能日となり、以下のように設定されます。  引けまでの間 : 当日  引け後       : 翌取引所営業日  休前日       : 休日明けの取引所営業日  ※ 日替わりはkabuステーションが日付変更通知を受信したタイミングです。 |
    | ReverseLimitOrder | object | False | 逆指値条件  ※FrontOrderTypeで逆指値を指定した場合のみ必須。 |
    | ReverseLimitOrder..TriggerSec | integer(int32) | True | トリガ銘柄  ※未設定の場合はエラーになります。 ; Enum: 1=発注銘柄, 2=NK225指数, 3=TOPIX指数 |
    | ReverseLimitOrder..TriggerPrice | number(double) | True | トリガ価格  ※未設定の場合はエラーになります。  ※数字以外が設定された場合はエラーになります。 |
    | ReverseLimitOrder..UnderOver | integer(int32) | True | 以上／以下  ※未設定の場合はエラーになります。  ※1、2以外が指定された場合はエラーになります。 ; Enum: 1=以下, 2=以上 |
    | ReverseLimitOrder..AfterHitOrderType | integer(int32) | True | ヒット後執行条件  ※未設定の場合はエラーになります。  ※1、2、3以外が指定された場合はエラーになります。 ; Enum: 1=成行, 2=指値, 3=不成 |
    | ReverseLimitOrder..AfterHitPrice | number(double) | True | ヒット後注文価格  ※未設定の場合はエラーになります。  ※数字以外が設定された場合はエラーになります。   ヒット後執行条件に従い、下記のようにヒット後注文価格を設定してください。	 ; Enum: ヒット後執行条件=設定価格, 成行=0, 指値=指値の単価, 不成=不成の単価 |

    * Example:
    ```json
    {
      "Symbol": "9433",
      "Exchange": 1,
      "SecurityType": 1,
      "Side": "1",
      "CashMargin": 3,
      "MarginTradeType": 3,
      "MarginPremiumUnit": 12.34,
      "DelivType": 2,
      "AccountType": 4,
      "Qty": 500,
      "ClosePositions": [
        {
          "HoldID": "E20200702xxxxx",
          "Qty": 500
        }
      ],
      "FrontOrderType": 30,
      "ExpireDay": 20200903,
      "ReverseLimitOrder": {
        "TriggerSec": 1,
        "TriggerPrice": 40000,
        "UnderOver": 2,
        "AfterHitOrderType": 1,
        "AfterHitPrice": 0
      }
    }
    ```

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Result | integer(int32) | False | 結果コード 0が成功。それ以外はエラーコード。; Example: 0 |
        | OrderId | string | False | 受付注文番号; Example: 20200529A01N06848002 |

* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `POST` /sendorder/future
**Summary:** 注文発注（先物）
先物銘柄の注文を発注します。

同一の銘柄に対しての注文は同時に5件ほどを上限としてご利用ください。
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |

**Request body**: (required)
* application/json
    | field | type | required | description |
    |-------|------|----------|-------------|
    | Symbol | string | True | 銘柄コード ※取引最終日に「先物銘柄コード取得」でDerivMonthに0（直近限月）を指定した場合、日中・夜間の時間帯に関わらず、取引最終日を迎える限月の銘柄コードを返します。取引最終日を迎える銘柄の取引は日中取引をもって終了となりますので、ご注意ください。; Example: 165120019 |
    | Exchange | integer(int32) | True | 市場コード  SOR日中、SOR夜間、SOR日通しは一部銘柄のみ対象となります。  SOR対象銘柄は以下をご参照ください。 ; Enum: 2=日通し, 23=日中, 24=夜間, 32=SOR日通し, 33=SOR日中, 34=SOR夜間, 先物SOR取扱銘柄=有効限月, 日経225先物ラージ=直近2限月, 日経225先物ミニ=直近4限月, TOPIX先物ラージ=直近2限月, TOPIX先物ミニ=直近3限月, 東証マザーズ指数先物=直近2限月, JPX日経400先物=直近2限月, NYダウ先物=直近2限月 |
    | TradeType | integer(int32) | True | 取引区分 ; Enum: 1=新規, 2=返済 |
    | TimeInForce | integer(int32) | True | 有効期間条件   ※執行条件(FrontOrderType)、有効期限条件(TimeInForce)、市場コード(Exchange)で選択できる組み合わせは下表のようになります。    ■日中、夜間、日通し対応表              ■SOR日中、SOR夜間、SOR日通し対応表   ; Enum: 1=FAS, 2=FAK, 3=FOK, 執行条件=有効
    期間条件, 日中=夜間, 指値=FAS, 指値=FAK, 指値=FOK, 成行=FAK, 成行=FOK, 逆指値（指値）=FAK, 逆指値（成行）=FAK, 引成=FAK, 引指=FAS, 執行条件=有効
    期間条件, SOR日中=SOR夜間, 指値=FAS, 指値=FAK, 指値=FOK, 成行=FAK, 成行=FOK, 逆指値（指値）=FAK, 逆指値（成行）=FAK, 引成=FAK, 引指=FAS |
    | Side | string | True | 売買区分 ; Enum: 1=売, 2=買 |
    | Qty | integer(int32) | True | 注文数量 |
    | ClosePositionOrder | integer(int32) | False | 決済順序 ※ClosePositionOrderとClosePositionsはどちらか一方のみ指定可能。 ※ClosePositionOrderとClosePositionsを両方指定した場合、エラー。 ; Enum: 0=日付（古い順）、損益（高い順）, 1=日付（古い順）、損益（低い順）, 2=日付（新しい順）、損益（高い順）, 3=日付（新しい順）、損益（低い順）, 4=損益（高い順）、日付（古い順）, 5=損益（高い順）、日付（新しい順）, 6=損益（低い順）、日付（古い順）, 7=損益（低い順）、日付（新しい順） |
    | ClosePositions | array<object> | False | 返済建玉指定 ※ClosePositionOrderとClosePositionsはどちらか一方のみ指定可能。 ※ClosePositionOrderとClosePositionsを両方指定した場合、エラー。 |
    | ClosePositions..HoldID | string | False | 返済建玉ID |
    | ClosePositions..Qty | integer(int32) | False | 返済建玉数量 |
    | FrontOrderType | integer(int32) | True | 執行条件 ; Enum: 18=引成（派生）
    ※TimeInForceは、「FAK」のみ有効, 20=指値, 28=引指（派生）
    ※TimeInForceは、「FAS」のみ有効, 30=逆指値, 120=成行（マーケットオーダー） |
    | Price | number(double) | True | 注文価格 ※FrontOrderTypeで成行を指定した場合、0を指定する。 ※詳細について、”FrontOrderType”をご確認ください。 |
    | ExpireDay | integer(int32) | True | 注文有効期限  yyyyMMdd形式。  「0」を指定すると、kabuステーション上の発注画面の「本日」に対応する日付として扱います。  「本日」は直近の注文可能日となり、以下のように設定されます。  その市場の引けまでの間 : 当日  その市場の引け後       : 翌取引所営業日  その市場の休前日       : 休日明けの取引所営業日  ※ 日替わりはkabuステーションが日付変更通知を受信したタイミングです。  ※ 日通しの場合、夜間取引の引け後に日付が更新されます。 |
    | ReverseLimitOrder | object | False | 逆指値条件  ※FrontOrderTypeで逆指値を指定した場合のみ必須。 |
    | ReverseLimitOrder..TriggerPrice | number(double) | True | トリガ価格  ※未設定の場合はエラーになります。  ※数字以外が設定された場合はエラーになります。 |
    | ReverseLimitOrder..UnderOver | integer(int32) | True | 以上／以下  ※未設定の場合はエラーになります。  ※1、2以外が指定された場合はエラーになります。 ; Enum: 1=以下, 2=以上 |
    | ReverseLimitOrder..AfterHitOrderType | integer(int32) | True | ヒット後執行条件  ※未設定の場合はエラーになります。  ※日通の注文で2以外が指定された場合はエラーになります。  ※日中、夜間の注文で1、2以外が指定された場合はエラーになります。  ※逆指値（成行）で有効期間条件(TimeInForce)にFAK以外を指定された場合はエラーになります。  ※逆指値（指値）で有効期間条件(TimeInForce)にFAS以外を指定された場合はエラーになります。 ; Enum: 1=成行, 2=指値 |
    | ReverseLimitOrder..AfterHitPrice | number(double) | True | ヒット後注文価格  ※未設定の場合はエラーになります。  ※数字以外が設定された場合はエラーになります。   ヒット後執行条件に従い、下記のようにヒット後注文価格を設定してください。	 ; Enum: ヒット後執行条件=設定価格, 成行=0, 指値=指値の単価 |

    * Example:
    ```json
    {
      "Symbol": "165120019",
      "Exchange": 23,
      "TradeType": 2,
      "TimeInForce": 2,
      "Side": "1",
      "Qty": 1,
      "ClosePositions": [
        {
          "HoldID": "E20200903xxxxx",
          "Qty": 1
        }
      ],
      "FrontOrderType": 30,
      "ExpireDay": 20200903,
      "ReverseLimitOrder": {
        "TriggerPrice": 100,
        "UnderOver": 1,
        "AfterHitOrderType": 1,
        "AfterHitPrice": 0
      }
    }
    ```

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Result | integer(int32) | False | 結果コード 0が成功。それ以外はエラーコード。; Example: 0 |
        | OrderId | string | False | 受付注文番号; Example: 20200529A01N06848002 |

* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `POST` /sendorder/option
**Summary:** 注文発注（オプション）
オプション銘柄の注文を発注します。

同一の銘柄に対しての注文は同時に5件ほどを上限としてご利用ください。
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |

**Request body**: (required)
* application/json
    | field | type | required | description |
    |-------|------|----------|-------------|
    | Symbol | string | True | 銘柄コード ※取引最終日に「オプション銘柄コード取得」でDerivMonthに0（直近限月）を指定した場合、日中・夜間の時間帯に関わらず、取引最終日を迎える限月の銘柄コードを返します。取引最終日を迎える銘柄の取引は日中取引をもって終了となりますので、ご注意ください。 |
    | Exchange | integer(int32) | True | 市場コード ; Enum: 2=日通し, 23=日中, 24=夜間 |
    | TradeType | integer(int32) | True | 取引区分 ; Enum: 1=新規, 2=返済 |
    | TimeInForce | integer(int32) | True | 有効期間条件   ※執行条件(FrontOrderType)、有効期限条件(TimeInForce)、市場コード(Exchange)で選択できる組み合わせは下表のようになります。               ; Enum: 1=FAS, 2=FAK, 3=FOK, 執行条件=有効期間条件, 日中=夜間, 指値=FAS, 指値=FAK, 指値=FOK, 成行=FAK, 成行=FOK, 逆指値（指値）=FAK, 逆指値（成行）=FAK, 引成=FAK, 引指=FAS |
    | Side | string | True | 売買区分 ; Enum: 1=売, 2=買 |
    | Qty | integer(int32) | True | 注文数量 |
    | ClosePositionOrder | integer(int32) | False | 決済順序 ※ClosePositionOrderとClosePositionsはどちらか一方のみ指定可能。 ※ClosePositionOrderとClosePositionsを両方指定した場合、エラー。 ; Enum: 0=日付（古い順）、損益（高い順）, 1=日付（古い順）、損益（低い順）, 2=日付（新しい順）、損益（高い順）, 3=日付（新しい順）、損益（低い順）, 4=損益（高い順）、日付（古い順）, 5=損益（高い順）、日付（新しい順）, 6=損益（低い順）、日付（古い順）, 7=損益（低い順）、日付（新しい順） |
    | ClosePositions | array<object> | False | 返済建玉指定 ※ClosePositionOrderとClosePositionsはどちらか一方のみ指定可能。 ※ClosePositionOrderとClosePositionsを両方指定した場合、エラー。 |
    | ClosePositions..HoldID | string | False | 返済建玉ID |
    | ClosePositions..Qty | integer(int32) | False | 返済建玉数量 |
    | FrontOrderType | integer(int32) | True | 執行条件 ; Enum: 18=引成（派生）
    ※TimeInForceは、「FAK」のみ有効, 20=指値, 28=引指（派生）
    ※TimeInForceは、「FAS」のみ有効, 30=逆指値, 120=成行（マーケットオーダー） |
    | Price | number(double) | True | 注文価格 ※FrontOrderTypeで成行を指定した場合、0を指定する。 ※詳細について、”FrontOrderType”をご確認ください。 |
    | ExpireDay | integer(int32) | True | 注文有効期限  yyyyMMdd形式。  「0」を指定すると、kabuステーション上の発注画面の「本日」に対応する日付として扱います。  「本日」は直近の注文可能日となり、以下のように設定されます。  その市場の引けまでの間 : 当日  その市場の引け後       : 翌取引所営業日  その市場の休前日       : 休日明けの取引所営業日  ※ 日替わりはkabuステーションが日付変更通知を受信したタイミングです。  ※ 日通しの場合、夜間取引の引け後に日付が更新されます。 |
    | ReverseLimitOrder | object | False | 逆指値条件  ※FrontOrderTypeで逆指値を指定した場合のみ必須。 |
    | ReverseLimitOrder..TriggerPrice | number(double) | True | トリガ価格  ※未設定の場合はエラーになります。  ※数字以外が設定された場合はエラーになります。 |
    | ReverseLimitOrder..UnderOver | integer(int32) | True | 以上／以下  ※未設定の場合はエラーになります。  ※1、2以外が指定された場合はエラーになります。 ; Enum: 1=以下, 2=以上 |
    | ReverseLimitOrder..AfterHitOrderType | integer(int32) | True | ヒット後執行条件  ※未設定の場合はエラーになります。  ※日通の注文で2以外が指定された場合はエラーになります。  ※日中、夜間の注文で1、2以外が指定された場合はエラーになります。  ※逆指値（成行）で有効期間条件(TimeInForce)にFAK以外を指定された場合はエラーになります。  ※逆指値（指値）で有効期間条件(TimeInForce)にFAS以外を指定された場合はエラーになります。 ; Enum: 1=成行, 2=指値 |
    | ReverseLimitOrder..AfterHitPrice | number(double) | True | ヒット後注文価格  ※未設定の場合はエラーになります。  ※数字以外が設定された場合はエラーになります。   ヒット後執行条件に従い、下記のようにヒット後注文価格を設定してください。	 ; Enum: ヒット後執行条件=設定価格, 成行=0, 指値=指値の単価 |

    * Example:
    ```json
    {
      "Symbol": "165120019",
      "Exchange": 23,
      "TradeType": 2,
      "TimeInForce": 2,
      "Side": "1",
      "Qty": 1,
      "ClosePositions": [
        {
          "HoldID": "E20200903xxxxx",
          "Qty": 1
        }
      ],
      "FrontOrderType": 30,
      "ExpireDay": 20200903,
      "ReverseLimitOrder": {
        "TriggerPrice": 100,
        "UnderOver": 1,
        "AfterHitOrderType": 1,
        "AfterHitPrice": 0
      }
    }
    ```

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Result | integer(int32) | False | 結果コード 0が成功。それ以外はエラーコード。; Example: 0 |
        | OrderId | string | False | 受付注文番号; Example: 20200529A01N06848002 |

* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `PUT` /cancelorder
**Summary:** 注文取消
注文を取消します
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |

**Request body**: (required)
* application/json
    | field | type | required | description |
    |-------|------|----------|-------------|
    | OrderId | string | True | 注文番号 sendorderのレスポンスで受け取るOrderID。; Example: 20200529A01N06848002 |


**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Result | integer(int32) | False | 結果コード 0が成功。それ以外はエラーコード。; Example: 0 |
        | OrderId | string | False | 受付注文番号; Example: 20200529A01N06848002 |

* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



## wallet
### `GET` /wallet/cash
**Summary:** 取引余力（現物）
口座の取引余力（現物）を取得します
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | StockAccountWallet | number(double) | False | 現物買付可能額  ※auマネーコネクトが有効の場合、auじぶん銀行の残高を含めた合計可能額を表示する  ※auマネーコネクトが無効の場合、三菱UFJ eスマート証券の可能額のみを表示する; Example: None |
        | AuKCStockAccountWallet | number(double) | False | うち、三菱UFJ eスマート証券可能額; Example: None |
        | AuJbnStockAccountWallet | number(double) | False | うち、auじぶん銀行残高 ※auマネーコネクトが無効の場合、「0」を表示する; Example: None |

* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `GET` /wallet/cash/{symbol}
**Summary:** 取引余力（現物）（銘柄指定）
指定した銘柄の取引余力（現物）を取得します
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |
| symbol | path | string | True | 銘柄コード   ※次の形式で入力してください。  [銘柄コード]@[市場コード]  ※市場コードは下記の定義値から選択してください。 市場コード                定義値       説明                       1       東証                 3       名証                     5           福証                         6           札証                         9           SOR            |

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | StockAccountWallet | number(double) | False | 現物買付可能額  ※auマネーコネクトが有効の場合、auじぶん銀行の残高を含めた合計可能額を表示する  ※auマネーコネクトが無効の場合、三菱UFJ eスマート証券の可能額のみを表示する; Example: None |
        | AuKCStockAccountWallet | number(double) | False | うち、三菱UFJ eスマート証券可能額; Example: None |
        | AuJbnStockAccountWallet | number(double) | False | うち、auじぶん銀行残高 ※auマネーコネクトが無効の場合、「0」を表示する; Example: None |

* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `GET` /wallet/margin
**Summary:** 取引余力（信用）
口座の取引余力（信用）を取得します
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | MarginAccountWallet | number(double) | False | 信用新規可能額; Example: None |
        | DepositkeepRate | number(double) | False | 保証金維持率 ※銘柄指定の場合のみ ※銘柄が指定されなかった場合、0.0を返す。; Example: None |
        | ConsignmentDepositRate | number(double) | False | 委託保証金率 ※銘柄指定の場合のみ。 ※銘柄が指定されなかった場合、Noneを返す。; Example: None |
        | CashOfConsignmentDepositRate | number(double) | False | 現金委託保証金率 ※銘柄指定の場合のみ。 ※銘柄が指定されなかった場合、Noneを返す。; Example: None |

* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `GET` /wallet/margin/{symbol}
**Summary:** 取引余力（信用）（銘柄指定）
指定した銘柄の取引余力（信用）を取得します
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |
| symbol | path | string | True | 銘柄コード   ※次の形式で入力してください。  [銘柄コード]@[市場コード]  ※市場コードは下記の定義値から選択してください。 市場コード                定義値       説明                       1       東証                 3       名証                 9       SOR          |

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | MarginAccountWallet | number(double) | False | 信用新規可能額; Example: None |
        | DepositkeepRate | number(double) | False | 保証金維持率 ※銘柄指定の場合のみ ※銘柄が指定されなかった場合、0.0を返す。; Example: None |
        | ConsignmentDepositRate | number(double) | False | 委託保証金率 ※銘柄指定の場合のみ。 ※銘柄が指定されなかった場合、Noneを返す。; Example: None |
        | CashOfConsignmentDepositRate | number(double) | False | 現金委託保証金率 ※銘柄指定の場合のみ。 ※銘柄が指定されなかった場合、Noneを返す。; Example: None |

* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `GET` /wallet/future
**Summary:** 取引余力（先物）
口座の取引余力（先物）を取得します
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | FutureTradeLimit | number(double) | False | 新規建玉可能額; Example: None |
        | MarginRequirement | number(double) | False | 買い必要証拠金額 ※銘柄指定の場合のみ。 ※銘柄が指定されなかった場合、空を返す。; Example: None |
        | MarginRequirementSell | number(double) | False | 売り必要証拠金額 ※銘柄指定の場合のみ。 ※銘柄が指定されなかった場合、空を返す。; Example: None |

* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `GET` /wallet/future/{symbol}
**Summary:** 取引余力（先物）（銘柄指定）
指定した銘柄の取引余力（先物）を取得します
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |
| symbol | path | string | True | 銘柄コード   ※次の形式で入力してください。  [銘柄コード]@[市場コード]  ※市場コードは下記の定義値から選択してください。     ※SOR市場は取扱っておりませんのでご注意ください。市場コード                       定義値           説明                               2           日通し                         23           日中                         24           夜間            |

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | FutureTradeLimit | number(double) | False | 新規建玉可能額; Example: None |
        | MarginRequirement | number(double) | False | 買い必要証拠金額 ※銘柄指定の場合のみ。 ※銘柄が指定されなかった場合、空を返す。; Example: None |
        | MarginRequirementSell | number(double) | False | 売り必要証拠金額 ※銘柄指定の場合のみ。 ※銘柄が指定されなかった場合、空を返す。; Example: None |

* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `GET` /wallet/option
**Summary:** 取引余力（オプション）
口座の取引余力（オプション）を取得します
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | OptionBuyTradeLimit | number(double) | False | 買新規建玉可能額; Example: None |
        | OptionSellTradeLimit | number(double) | False | 売新規建玉可能額; Example: None |
        | MarginRequirement | number(double) | False | 必要証拠金額 ※銘柄指定の場合のみ。 ※銘柄が指定されなかった場合、空を返す。; Example: None |

* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `GET` /wallet/option/{symbol}
**Summary:** 取引余力（オプション）（銘柄指定）
指定した銘柄の取引余力（オプション）を取得します
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |
| symbol | path | string | True | 銘柄コード   ※次の形式で入力してください。  [銘柄コード]@[市場コード]  ※市場コードは下記の定義値から選択してください。 市場コード                      定義値           説明                               2           日通し                         23           日中                         24           夜間            |

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | OptionBuyTradeLimit | number(double) | False | 買新規建玉可能額; Example: None |
        | OptionSellTradeLimit | number(double) | False | 売新規建玉可能額; Example: None |
        | MarginRequirement | number(double) | False | 必要証拠金額 ※銘柄指定の場合のみ。 ※銘柄が指定されなかった場合、空を返す。; Example: None |

* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



## info
### `GET` /board/{symbol}
**Summary:** 時価情報・板情報
指定した銘柄の時価情報・板情報を取得します

レスポンスの一部にnullが発生した場合、該当銘柄を銘柄登録をしてから、

再度時価情報・板情報APIを実行してください。
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |
| symbol | path | string | True | 銘柄コード   ※次の形式で入力してください。  [銘柄コード]@[市場コード]  ※市場コードは下記の定義値から選択してください。  ※SOR市場は取扱っておりませんのでご注意ください。市場コード                       定義値           説明                               1           東証                         3           名証                         5           福証                         6           札証                         2           日通し                         23           日中                         24           夜間            |

**Responses**:
* **200** – ※①：レスポンスにある「Bid」と「Ask」は、本来の意味である「買気配」と「売気配」と逆になっております。実際に返却される値は日本語の説明に準じたものになりますので、ご注意いただきますようお願い申し上げます。ご迷惑をおかけしまして、誠に申し訳ございません。   影響するキー名：  BidQty, BidPrice, BidTime, BidSign  AskQty, AskPrice, AskTime, AskSign
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Symbol | string | False | 銘柄コード |
        | SymbolName | string | False | 銘柄名 |
        | Exchange | integer(int32) | False | 市場コード ※株式・先物・オプション銘柄の場合のみ ; Enum: 1=東証, 3=名証, 5=福証, 6=札証, 2=日通し, 23=日中, 24=夜間; Example: 1 |
        | ExchangeName | string | False | 市場名称 ※株式・先物・オプション銘柄の場合のみ |
        | CurrentPrice | number(double) | False | 現値 |
        | CurrentPriceTime | string(date-time) | False | 現値時刻 |
        | CurrentPriceChangeStatus | string | False | 現値前値比較 ; Enum: 0000=事象なし, 0056=変わらず, 0057=UP, 0058=DOWN, 0059=中断板寄り後の初値, 0060=ザラバ引け, 0061=板寄り引け, 0062=中断引け, 0063=ダウン引け, 0064=逆転終値, 0066=特別気配引け, 0067=一時留保引け, 0068=売買停止引け, 0069=サーキットブレーカ引け, 0431=ダイナミックサーキットブレーカ引け |
        | CurrentPriceStatus | integer(int32) | False | 現値ステータス ; Enum: 1=現値, 2=不連続歩み, 3=板寄せ, 4=システム障害, 5=中断, 6=売買停止, 7=売買停止・システム停止解除, 8=終値, 9=システム停止, 10=概算値, 11=参考値, 12=サーキットブレイク実施中, 13=システム障害解除, 14=サーキットブレイク解除, 15=中断解除, 16=一時留保中, 17=一時留保解除, 18=ファイル障害, 19=ファイル障害解除, 20=Spread/Strategy, 21=ダイナミックサーキットブレイク発動, 22=ダイナミックサーキットブレイク解除, 23=板寄せ約定 |
        | CalcPrice | number(double) | False | 計算用現値 |
        | PreviousClose | number(double) | False | 前日終値 |
        | PreviousCloseTime | string(date-time) | False | 前日終値日付 |
        | ChangePreviousClose | number(double) | False | 前日比 |
        | ChangePreviousClosePer | number(double) | False | 騰落率 |
        | OpeningPrice | number(double) | False | 始値 |
        | OpeningPriceTime | string(date-time) | False | 始値時刻 |
        | HighPrice | number(double) | False | 高値 |
        | HighPriceTime | string(date-time) | False | 高値時刻 |
        | LowPrice | number(double) | False | 安値 |
        | LowPriceTime | string(date-time) | False | 安値時刻 |
        | TradingVolume | number(double) | False | 売買高 ※株式・先物・オプション銘柄の場合のみ |
        | TradingVolumeTime | string(date-time) | False | 売買高時刻 ※株式・先物・オプション銘柄の場合のみ |
        | VWAP | number(double) | False | 売買高加重平均価格（VWAP） ※株式・先物・オプション銘柄の場合のみ |
        | TradingValue | number(double) | False | 売買代金 ※株式・先物・オプション銘柄の場合のみ |
        | BidQty | number(double) | False | 最良売気配数量 ※① ※株式・先物・オプション銘柄の場合のみ |
        | BidPrice | number(double) | False | 最良売気配値段 ※① ※株式・先物・オプション銘柄の場合のみ |
        | BidTime | string(date-time) | False | 最良売気配時刻 ※① ※株式銘柄の場合のみ |
        | BidSign | string | False | 最良売気配フラグ ※① ※株式・先物・オプション銘柄の場合のみ ; Enum: 0000=事象なし, 0101=一般気配, 0102=特別気配, 0103=注意気配, 0107=寄前気配, 0108=停止前特別気配, 0109=引け後気配, 0116=寄前気配約定成立ポイントなし, 0117=寄前気配約定成立ポイントあり, 0118=連続約定気配, 0119=停止前の連続約定気配, 0120=買い上がり売り下がり中 |
        | MarketOrderSellQty | number(double) | False | 売成行数量 ※株式銘柄の場合のみ |
        | Sell1 | object | False | 売気配数量1本目 |
        | Sell1..Time | string(date-time) | False | 時刻 ※株式銘柄の場合のみ |
        | Sell1..Sign | string | False | 気配フラグ ※株式・先物・オプション銘柄の場合のみ ; Enum: 0000=事象なし, 0101=一般気配, 0102=特別気配, 0103=注意気配, 0107=寄前気配, 0108=停止前特別気配, 0109=引け後気配, 0116=寄前気配約定成立ポイントなし, 0117=寄前気配約定成立ポイントあり, 0118=連続約定気配, 0119=停止前の連続約定気配, 0120=買い上がり売り下がり中 |
        | Sell1..Price | number(double) | False | 値段 ※株式・先物・オプション銘柄の場合のみ |
        | Sell1..Qty | number(double) | False | 数量 ※株式・先物・オプション銘柄の場合のみ |
        | Sell2 | object | False | 売気配数量2本目 |
        | Sell2..Price | number(double) | False | 値段 ※株式・先物・オプション銘柄の場合のみ |
        | Sell2..Qty | number(double) | False | 数量 ※株式・先物・オプション銘柄の場合のみ |
        | Sell3 | object | False | 売気配数量3本目 |
        | Sell3..Price | number(double) | False | 値段 ※株式・先物・オプション銘柄の場合のみ |
        | Sell3..Qty | number(double) | False | 数量 ※株式・先物・オプション銘柄の場合のみ |
        | Sell4 | object | False | 売気配数量4本目 |
        | Sell4..Price | number(double) | False | 値段 ※株式・先物・オプション銘柄の場合のみ |
        | Sell4..Qty | number(double) | False | 数量 ※株式・先物・オプション銘柄の場合のみ |
        | Sell5 | object | False | 売気配数量5本目 |
        | Sell5..Price | number(double) | False | 値段 ※株式・先物・オプション銘柄の場合のみ |
        | Sell5..Qty | number(double) | False | 数量 ※株式・先物・オプション銘柄の場合のみ |
        | Sell6 | object | False | 売気配数量6本目 |
        | Sell6..Price | number(double) | False | 値段 ※株式・先物・オプション銘柄の場合のみ |
        | Sell6..Qty | number(double) | False | 数量 ※株式・先物・オプション銘柄の場合のみ |
        | Sell7 | object | False | 売気配数量7本目 |
        | Sell7..Price | number(double) | False | 値段 ※株式・先物・オプション銘柄の場合のみ |
        | Sell7..Qty | number(double) | False | 数量 ※株式・先物・オプション銘柄の場合のみ |
        | Sell8 | object | False | 売気配数量8本目 |
        | Sell8..Price | number(double) | False | 値段 ※株式・先物・オプション銘柄の場合のみ |
        | Sell8..Qty | number(double) | False | 数量 ※株式・先物・オプション銘柄の場合のみ |
        | Sell9 | object | False | 売気配数量9本目 |
        | Sell9..Price | number(double) | False | 値段 ※株式・先物・オプション銘柄の場合のみ |
        | Sell9..Qty | number(double) | False | 数量 ※株式・先物・オプション銘柄の場合のみ |
        | Sell10 | object | False | 売気配数量10本目 |
        | Sell10..Price | number(double) | False | 値段 ※株式・先物・オプション銘柄の場合のみ |
        | Sell10..Qty | number(double) | False | 数量 ※株式・先物・オプション銘柄の場合のみ |
        | AskQty | number(double) | False | 最良買気配数量 ※① ※株式・先物・オプション銘柄の場合のみ |
        | AskPrice | number(double) | False | 最良買気配値段 ※① ※株式・先物・オプション銘柄の場合のみ |
        | AskTime | string(date-time) | False | 最良買気配時刻 ※① ※株式銘柄の場合のみ |
        | AskSign | string | False | 最良買気配フラグ ※① ※株式・先物・オプション銘柄の場合のみ ; Enum: 0000=事象なし, 0101=一般気配, 0102=特別気配, 0103=注意気配, 0107=寄前気配, 0108=停止前特別気配, 0109=引け後気配, 0116=寄前気配約定成立ポイントなし, 0117=寄前気配約定成立ポイントあり, 0118=連続約定気配, 0119=停止前の連続約定気配, 0120=買い上がり売り下がり中 |
        | MarketOrderBuyQty | number(double) | False | 買成行数量 ※株式銘柄の場合のみ |
        | Buy1 | object | False | 買気配数量1本目 |
        | Buy1..Time | string(date-time) | False | 時刻 ※株式銘柄の場合のみ |
        | Buy1..Sign | string | False | 気配フラグ ※株式・先物・オプション銘柄の場合のみ ; Enum: 0000=事象なし, 0101=一般気配, 0102=特別気配, 0103=注意気配, 0107=寄前気配, 0108=停止前特別気配, 0109=引け後気配, 0116=寄前気配約定成立ポイントなし, 0117=寄前気配約定成立ポイントあり, 0118=連続約定気配, 0119=停止前の連続約定気配, 0120=買い上がり売り下がり中 |
        | Buy1..Price | number(double) | False | 値段 ※株式・先物・オプション銘柄の場合のみ |
        | Buy1..Qty | number(double) | False | 数量 ※株式・先物・オプション銘柄の場合のみ |
        | Buy2 | object | False | 買気配数量2本目 |
        | Buy2..Price | number(double) | False | 値段 ※株式・先物・オプション銘柄の場合のみ |
        | Buy2..Qty | number(double) | False | 数量 ※株式・先物・オプション銘柄の場合のみ |
        | Buy3 | object | False | 買気配数量3本目 |
        | Buy3..Price | number(double) | False | 値段 ※株式・先物・オプション銘柄の場合のみ |
        | Buy3..Qty | number(double) | False | 数量 ※株式・先物・オプション銘柄の場合のみ |
        | Buy4 | object | False | 買気配数量4本目 |
        | Buy4..Price | number(double) | False | 値段 ※株式・先物・オプション銘柄の場合のみ |
        | Buy4..Qty | number(double) | False | 数量 ※株式・先物・オプション銘柄の場合のみ |
        | Buy5 | object | False | 買気配数量5本目 |
        | Buy5..Price | number(double) | False | 値段 ※株式・先物・オプション銘柄の場合のみ |
        | Buy5..Qty | number(double) | False | 数量 ※株式・先物・オプション銘柄の場合のみ |
        | Buy6 | object | False | 買気配数量6本目 |
        | Buy6..Price | number(double) | False | 値段 ※株式・先物・オプション銘柄の場合のみ |
        | Buy6..Qty | number(double) | False | 数量 ※株式・先物・オプション銘柄の場合のみ |
        | Buy7 | object | False | 買気配数量7本目 |
        | Buy7..Price | number(double) | False | 値段 ※株式・先物・オプション銘柄の場合のみ |
        | Buy7..Qty | number(double) | False | 数量 ※株式・先物・オプション銘柄の場合のみ |
        | Buy8 | object | False | 買気配数量8本目 |
        | Buy8..Price | number(double) | False | 値段 ※株式・先物・オプション銘柄の場合のみ |
        | Buy8..Qty | number(double) | False | 数量 ※株式・先物・オプション銘柄の場合のみ |
        | Buy9 | object | False | 買気配数量9本目 |
        | Buy9..Price | number(double) | False | 値段 ※株式・先物・オプション銘柄の場合のみ |
        | Buy9..Qty | number(double) | False | 数量 ※株式・先物・オプション銘柄の場合のみ |
        | Buy10 | object | False | 買気配数量10本目 |
        | Buy10..Price | number(double) | False | 値段 ※株式・先物・オプション銘柄の場合のみ |
        | Buy10..Qty | number(double) | False | 数量 ※株式・先物・オプション銘柄の場合のみ |
        | OverSellQty | number(double) | False | OVER気配数量 ※株式銘柄の場合のみ |
        | UnderBuyQty | number(double) | False | UNDER気配数量 ※株式銘柄の場合のみ |
        | TotalMarketValue | number(double) | False | 時価総額 ※株式銘柄の場合のみ |
        | ClearingPrice | number(double) | False | 清算値 ※先物銘柄の場合のみ |
        | IV | number(double) | False | インプライド・ボラティリティ ※オプション銘柄かつ日通しの場合のみ |
        | Gamma | number(double) | False | ガンマ ※オプション銘柄かつ日通しの場合のみ |
        | Theta | number(double) | False | セータ ※オプション銘柄かつ日通しの場合のみ |
        | Vega | number(double) | False | ベガ ※オプション銘柄かつ日通しの場合のみ |
        | Delta | number(double) | False | デルタ ※オプション銘柄かつ日通しの場合のみ |
        | SecurityType | integer(int32) | False | 銘柄種別 ; Enum: 0=指数, 1=現物, 101=日経225先物, 103=日経225OP, 107=TOPIX先物, 121=JPX400先物, 144=NYダウ, 145=日経平均VI, 154=グロース250先物, 155=TOPIX_REIT, 171=TOPIX CORE30, 901=日経平均225ミニ先物, 907=TOPIXミニ先物 |

        * Example:
        ```json
        {
          "Symbol": "5401",
          "SymbolName": "新日鐵住金",
          "Exchange": 1,
          "ExchangeName": "東証プ",
          "CurrentPrice": 2408.0,
          "CurrentPriceTime": "2022-04-04T15:00:00+09:00",
          "CurrentPriceChangeStatus": "0058",
          "CurrentPriceStatus": 1,
          "CalcPrice": 343.7,
          "PreviousClose": 1048.0,
          "PreviousCloseTime": "2022-04-01T00:00:00+09:00",
          "ChangePreviousClose": 1360.0,
          "ChangePreviousClosePer": 129.77,
          "OpeningPrice": 2380.0,
          "OpeningPriceTime": "2022-04-04T09:00:00+09:00",
          "HighPrice": 2418.0,
          "HighPriceTime": "2022-04-04T13:25:47+09:00",
          "LowPrice": 2370.0,
          "LowPriceTime": "2022-04-04T10:00:04+09:00",
          "TradingVolume": 4571500.0,
          "TradingVolumeTime": "2022-04-04T15:00:00+09:00",
          "VWAP": 2394.4262,
          "TradingValue": 10946119350.0,
          "BidQty": 100.0,
          "BidPrice": 2408.5,
          "BidTime": "2022-04-04T14:59:59+09:00",
          "BidSign": "0101",
          "MarketOrderSellQty": 0.0,
          "Sell1": {
            "Time": "2022-04-04T14:59:59+09:00",
            "Sign": "0101",
            "Price": 2408.5,
            "Qty": 100.0
          },
          "Sell2": {
            "Price": 2409.0,
            "Qty": 800.0
          },
          "Sell3": {
            "Price": 2409.5,
            "Qty": 2100.0
          },
          "Sell4": {
            "Price": 2410.0,
            "Qty": 800.0
          },
          "Sell5": {
            "Price": 2410.5,
            "Qty": 500
          },
          "Sell6": {
            "Price": 2411.0,
            "Qty": 8400.0
          },
          "Sell7": {
            "Price": 2411.5,
            "Qty": 1200.0
          },
          "Sell8": {
            "Price": 2412.0,
            "Qty": 27200.0
          },
          "Sell9": {
            "Price": 2412.5,
            "Qty": 400.0
          },
          "Sell10": {
            "Price": 2413.0,
            "Qty": 16400.0
          },
          "AskQty": 200.0,
          "AskPrice": 2407.5,
          "AskTime": "2022-04-04T14:59:59+09:00",
          "AskSign": "0101",
          "MarketOrderBuyQty": 0.0,
          "Buy1": {
            "Time": "2022-04-04T14:59:59+09:00",
            "Sign": "0101",
            "Price": 2407.5,
            "Qty": 200.0
          },
          "Buy2": {
            "Price": 2407.0,
            "Qty": 400.0
          },
          "Buy3": {
            "Price": 2406.5,
            "Qty": 1000.0
          },
          "Buy4": {
            "Price": 2406.0,
            "Qty": 5800.0
          },
          "Buy5": {
            "Price": 2405.5,
            "Qty": 7500
          },
          "Buy6": {
            "Price": 2405.0,
            "Qty": 2200.0
          },
          "Buy7": {
            "Price": 2404.5,
            "Qty": 16700.0
          },
          "Buy8": {
            "Price": 2403.0,
            "Qty": 1300.0
          },
          "Buy9": {
            "Price": 2403.5,
            "Qty": 1300.0
          },
          "Buy10": {
            "Price": 2403.0,
            "Qty": 3000.0
          },
          "OverSellQty": 974900,
          "UnderBuyQty": 756000,
          "TotalMarketValue": 3266254659361.4,
          "SecurityType": 1
        }
        ```
* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `GET` /symbol/{symbol}
**Summary:** 銘柄情報
指定した銘柄情報を取得します
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |
| symbol | path | string | True | 銘柄コード   ※次の形式で入力してください。  [銘柄コード]@[市場コード]  ※市場コードは下記の定義値から選択してください。  ※SOR市場は取扱っておりませんのでご注意ください。市場コード                       定義値           説明                               1           東証                         3           名証                         5           福証                         6           札証                         2           日通し                         23           日中                         24           夜間            |
| addinfo | query | string | False | 追加情報出力フラグ（未指定時：true）  ※追加情報は、「時価総額」、「発行済み株式数」、「決算期日」、「清算値」を意味します。                      定義値           説明                               true           追加情報を出力する                         false           追加情報を出力しない            |

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Symbol | string | False | 銘柄コード |
        | SymbolName | string | False | 銘柄名 |
        | DisplayName | string | False | 銘柄略称 ※株式・先物・オプション銘柄の場合のみ |
        | Exchange | integer(int32) | False | 市場コード ※株式・先物・オプション銘柄の場合のみ ; Enum: 1=東証, 3=名証, 5=福証, 6=札証, 2=日通し, 23=日中, 24=夜間 |
        | ExchangeName | string | False | 市場名称 ※株式・先物・オプション銘柄の場合のみ |
        | BisCategory | string | False | 業種コード名 ※株式銘柄の場合のみ ; Enum: 0050=水産・農林業, 1050=鉱業, 2050=建設業, 3050=食料品, 3100=繊維製品, 3150=パルプ・紙, 3200=化学, 3250=医薬品, 3300=石油・石炭製品, 3350=ゴム製品, 3400=ガラス・土石製品, 3450=鉄鋼, 3500=非鉄金属, 3550=金属製品, 3600=機械, 3650=電気機器, 3700=輸送用機器, 3750=精密機器, 3800=その他製品, 4050=電気・ガス業, 5050=陸運業, 5100=海運業, 5150=空運業, 5200=倉庫・運輸関連業, 5250=情報・通信業, 6050=卸売業, 6100=小売業, 7050=銀行業, 7100=証券、商品先物取引業, 7150=保険業, 7200=その他金融業, 8050=不動産業, 9050=サービス業, 9999=その他 |
        | TotalMarketValue | number(double) | False | 時価総額 ※株式銘柄の場合のみ 追加情報出力フラグ：falseの場合、null |
        | TotalStocks | number(double) | False | 発行済み株式数（千株） ※株式銘柄の場合のみ 追加情報出力フラグ：falseの場合、null |
        | TradingUnit | number(double) | False | 売買単位 ※株式・先物・オプション銘柄の場合のみ |
        | FiscalYearEndBasic | integer(int32) | False | 決算期日 ※株式銘柄の場合のみ 追加情報出力フラグ：falseの場合、null |
        | PriceRangeGroup | string | False | 呼値グループ  ※株式・先物・オプション銘柄の場合のみ  ※各呼値コードが対応する商品は以下となります。  　株式の呼値の単位の詳細は [JPXページ](https://www.jpx.co.jp/equities/trading/domestic/07.html) をご覧ください。  　10000：株式(TOPIX500採用銘柄以外)　  　10003：株式(TOPIX500採用銘柄)  　10118 : 日経平均先物  　10119 : 日経225mini  　10318 : 日経平均オプション  　10706 : ﾐﾆTOPIX先物  　10718 : TOPIX先物  　12122 : JPX日経400指数先物  　14473 : NYダウ先物  　14515 : 日経平均VI先物  　15411 : グロース250先物  　15569 : 東証REIT指数先物  　17163 : TOPIXCore30指数先物  ; Enum: 呼値コード=値段の水準, 10000=3000円以下, 10000=5000円以下, 10000=30000円以下, 10000=50000円以下, 10000=300000円以下, 10000=500000円以下, 10000=3000000円以下, 10000=5000000円以下, 10000=30000000円以下, 10000=50000000円以下, 10000=50000000円超, 10003=1000円以下, 10003=3000円以下, 10003=10000円以下, 10003=30000円以下, 10003=100000円以下, 10003=300000円以下, 10003=1000000円以下, 10003=3000000円以下, 10003=10000000円以下, 10003=30000000円以下, 10003=30000000円超, 10118=-, 10119=-, 10318=100円以下, 10318=1000円以下, 10318=1000円超, 10706=-, 10718=-, 12122=-, 14473=-, 14515=-, 15411=-, 15569=-, 17163=- |
        | KCMarginBuy | boolean | False | 一般信用買建フラグ ※trueのとき、一般信用(長期)または一般信用(デイトレ)が買建可能 ※株式銘柄の場合のみ |
        | KCMarginSell | boolean | False | 一般信用売建フラグ ※trueのとき、一般信用(長期)または一般信用(デイトレ)が売建可能 ※株式銘柄の場合のみ |
        | MarginBuy | boolean | False | 制度信用買建フラグ ※trueのとき制度信用買建可能 ※株式銘柄の場合のみ |
        | MarginSell | boolean | False | 制度信用売建フラグ ※trueのとき制度信用売建可能 ※株式銘柄の場合のみ |
        | UpperLimit | number(double) | False | 値幅上限 ※株式・先物・オプション銘柄の場合のみ |
        | LowerLimit | number(double) | False | 値幅下限 ※株式・先物・オプション銘柄の場合のみ |
        | Underlyer | string | False | 原資産コード ※先物・オプション銘柄の場合のみ ; Enum: NK225=日経225, NK300=日経300, GROWTH=グロース250先物, JPX400=JPX日経400, TOPIX=TOPIX, NKVI=日経平均VI, DJIA=NYダウ, TSEREITINDEX=東証REIT指数, TOPIXCORE30=TOPIX Core30 |
        | DerivMonth | string | False | 限月-年月 ※「限月-年月」は「年(yyyy)/月(MM)」で表示します。 ※先物・オプション銘柄の場合のみ |
        | TradeStart | integer(int32) | False | 取引開始日 ※先物・オプション銘柄の場合のみ |
        | TradeEnd | integer(int32) | False | 取引終了日 ※先物・オプション銘柄の場合のみ |
        | StrikePrice | number(double) | False | 権利行使価格 ※オプション銘柄の場合のみ |
        | PutOrCall | integer(int32) | False | プット/コール区分 ※オプション銘柄の場合のみ ; Enum: 1=プット, 2=コール |
        | ClearingPrice | number(double) | False | 清算値 ※先物銘柄の場合のみ 追加情報出力フラグ：falseの場合、null |

        * Example:
        ```json
        {
          "Symbol": "166090018",
          "SymbolName": "日経平均先物 21/09",
          "DisplayName": "日経平均先物 09",
          "Exchange": 23,
          "ExchangeName": "大阪日中",
          "TradingUnit": 1.0,
          "PriceRangeGroup": "10118",
          "UpperLimit": 29870.0,
          "LowerLimit": 25290.0,
          "Underlyer": "NK225",
          "DerivMonth": "2021/09",
          "TradeEnd": 20210909,
          "TradeStart": 20200313,
          "ClearingPrice": null
        }
        ```
* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `GET` /orders
**Summary:** 注文約定照会
注文一覧を取得します。

※下記Queryパラメータは任意設定となります。
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |
| product | query | string | False | 取得する商品                      定義値           説明                               0           すべて                          1           現物                         2           信用                         3           先物                         4           OP            |
| id | query | string | False | 注文番号  ※指定された注文番号と一致する注文のみレスポンスします。  ※指定された注文番号との比較では大文字小文字を区別しません。  ※複数の注文番号を指定することはできません。 |
| updtime | query | string | False | 更新日時  ※形式：yyyyMMddHHmmss （例：20201201123456）  ※指定された更新日時以降（指定日時含む）に更新された注文のみレスポンスします。  ※複数の更新日時を指定することはできません。 |
| details | query | string | False | 注文詳細抑止                      定義値           説明                               true           注文詳細を出力する（デフォルト）                         false           注文詳細の出力しない            |
| symbol | query | string | False | 銘柄コード ※指定された銘柄コードと一致する注文のみレスポンスします。 ※複数の銘柄コードを指定することができません。 |
| state | query | string | False | 状態  ※指定された状態と一致する注文のみレスポンスします。  ※フィルタには数字の入力のみ受け付けます。  ※複数の状態を指定することはできません。                      定義値           説明                               1           待機（発注待機）                         2           処理中（発注送信中）                         3           処理済（発注済・訂正済）                         4           訂正取消送信中                         5           終了（発注エラー・取消済・全約定・失効・期限切れ）            |
| side | query | string | False | 売買区分  ※指定された売買区分と一致する注文のみレスポンスします。  ※フィルタには数字の入力のみ受け付けます。  ※複数の売買区分を指定することができません。                      定義値           説明                               1           売                         2           買            |
| cashmargin | query | string | False | 取引区分  ※指定された取引区分と一致する注文のみレスポンスします。  ※フィルタには数字の入力のみ受け付けます。  ※複数の取引区分を指定することができません。                      定義値           説明                               2           新規                         3           返済            |

**Responses**:
* **200** – OK
    * application/json
* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `GET` /positions
**Summary:** 残高照会
残高一覧を取得します。
※下記Queryパラメータは任意設定となります。
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |
| product | query | string | False | 取得する商品                      定義値           説明                               0           すべて                         1           現物                         2           信用                         3           先物                         4           OP            |
| symbol | query | string | False | 銘柄コード ※指定された銘柄コードと一致するポジションのみレスポンスします。 ※複数の銘柄コードを指定することはできません。 |
| side | query | string | False | 売買区分フィルタ  指定された売買区分と一致する注文を返す                      定義値           説明                               1           売                         2           買            |
| addinfo | query | string | False | 追加情報出力フラグ（未指定時：true）  ※追加情報は、「現在値」、「評価金額」、「評価損益額」、「評価損益率」を意味します。                      定義値           説明                               true           追加情報を出力する                         false           追加情報を出力しない            |

**Responses**:
* **200** – OK
    * application/json
* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `GET` /symbolname/future
**Summary:** 先物銘柄コード取得
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |
| FutureCode | query | string | False | 先物コード  ※大文字小文字は区別しません。                      定義値           説明                               NK225           日経平均先物                         NK225mini           日経225mini先物                         TOPIX           TOPIX先物                         TOPIXmini           ミニTOPIX先物                         GROWTH           グロース250先物                         JPX400           JPX日経400先物                         DOW           NYダウ先物                         VI           日経平均VI先物                         Core30           TOPIX Core30先物                         REIT           東証REIT指数先物                         NK225micro           日経225マイクロ先物            |
| DerivMonth | query | integer | True | 限月  ※限月はyyyyMM形式で指定します。0を指定した場合、直近限月となります。  ※取引最終日に「0」（直近限月）を指定した場合、日中・夜間の時間帯に関わらず、 取引最終日を迎える限月の銘柄コードを返します。取引最終日を迎える銘柄の取引は日中取引をもって終了となりますので、ご注意ください。  |

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Symbol | string | False | 銘柄コード; Example: 136091318 |
        | SymbolName | string | False | 銘柄名称; Example: 日経平均オプション 21/09 プット 31375 |

* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `GET` /symbolname/option
**Summary:** オプション銘柄コード取得
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |
| OptionCode | query | string | False | オプションコード  ※指定なしの場合は、日経225オプションを対象とする。                       定義値           説明                               NK225op           日経225オプション                         NK225miniop           日経225ミニオプション            |
| DerivMonth | query | integer | True | 限月 ※限月はyyyyMM形式で指定します。0を指定した場合、直近限月となります。 ※取引最終日に「0」（直近限月）を指定した場合、日中・夜間の時間帯に関わらず、取引最終日を迎える限月の銘柄コードを返します。取引最終日を迎える銘柄の取引は日中取引をもって終了となりますので、ご注意ください。 |
| PutOrCall | query | string | True | コール or プット  ※大文字小文字は区別しません。                      定義値           説明                               P           PUT                         C           CALL            |
| StrikePrice | query | integer | True | 権利行使価格 ※0を指定した場合、APIを実行した時点でのATMとなります。 |

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Symbol | string | False | 銘柄コード; Example: 136091318 |
        | SymbolName | string | False | 銘柄名称; Example: 日経平均オプション 21/09 プット 31375 |

* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `GET` /symbolname/minioptionweekly
**Summary:** ミニオプション（限週）銘柄コード取得
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |
| DerivMonth | query | integer | True | 限月 ※限月はyyyyMM形式で指定します。0を指定した場合、直近限月となります。 ※取引最終日に「0」（直近限月）を指定した場合、日中・夜間の時間帯に関わらず、取引最終日を迎える限月の銘柄コードを返します。取引最終日を迎える銘柄の取引は日中取引をもって終了となりますので、ご注意ください。 |
| DerivWeekly | query | integer | True | 限週 ※限週は0,1,3,4,5のいずれかを指定します。0を指定した場合、指定した限月の直近限週となります。 ※取引最終日に「0」（直近限週）を指定した場合、日中・夜間の時間帯に関わらず、取引最終日を迎える限週の銘柄コードを返します。取引最終日を迎える銘柄の取引は日中取引をもって終了となりますので、ご注意ください。 |
| PutOrCall | query | string | True | コール or プット  ※大文字小文字は区別しません。                      定義値           説明                               P           PUT                         C           CALL            |
| StrikePrice | query | integer | True | 権利行使価格 ※0を指定した場合、APIを実行した時点でのATMとなります。 |

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Symbol | string | False | 銘柄コード; Example: 136091318 |
        | SymbolName | string | False | 銘柄名称; Example: 日経平均オプション 21/09 プット 31375 |

* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `GET` /ranking
**Summary:** 詳細ランキング
詳細ランキング画面と同様の各種ランキングを返します。

ランキングの対象日はkabuステーションが保持している当日のデータとなります。

※株価情報ランキング、業種別指数ランキングは、下記の時間帯でデータがクリアされるため、

その間の詳細ランキングAPIは空レスポンスとなります。

データクリア：平日7:53頃-9:00過ぎ頃

※信用情報ランキングは毎週第３営業日の7:55頃にデータが更新されます。
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |
| Type | query | string | True | 種別  ※信用情報ランキングに「福証」「札証」を指定した場合は、空レスポンスになります                      定義値           説明                               1           値上がり率（デフォルト）                         2           値下がり率                         3           売買高上位                         4           売買代金                         5           TICK回数                         6           売買高急増                         7           売買代金急増                         8           信用売残増                         9           信用売残減                         10           信用買残増                         11           信用買残減                         12           信用高倍率                         13           信用低倍率                         14           業種別値上がり率                         15           業種別値下がり率            |
| ExchangeDivision | query | string | True | 市場  ※業種別値上がり率・値下がり率に市場を指定しても無視されます                      定義値           説明                               ALL           全市場（デフォルト）                         T           東証全体                         TP           東証プライム                         TS           東証スタンダード                         TG           グロース250                         M           名証                         FK           福証                         S           札証            |

**Responses**:
* **200** – OK
    * application/json
* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `GET` /exchange/{symbol}
**Summary:** 為替情報
マネービューの情報を取得する
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |
| symbol | path | string | True | 通貨                定義値       内容                       usdjpy       USD/JPY                 eurjpy       EUR/JPY                 gbpjpy       GBP/JPY                 audjpy       AUD/JPY                 chfjpy       CHF/JPY                 cadjpy       CAD/JPY                 nzdjpy       NZD/JPY                 zarjpy       ZAR/JPY                 eurusd       EUR/USD                 gbpusd       GBP/USD                 audusd       AUD/USD          |

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Symbol | string | False | 通貨 |
        | BidPrice | number(double) | False | BID |
        | Spread | number(double) | False | SP |
        | AskPrice | number(double) | False | ASK |
        | Change | number(double) | False | 前日比 |
        | Time | string | False | 時刻  ※HH:mm:ss形式 |

        * Example:
        ```json
        {
          "Symbol": "USD/JPY",
          "BidPrice": 105.502,
          "Spread": 0.2,
          "AskPrice": 105.504,
          "Change": -0.055,
          "Time": "16:10:45"
        }
        ```
* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `GET` /regulations/{symbol}
**Summary:** 規制情報
規制情報＋空売り規制情報を取得する
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |
| symbol | path | string | True | 銘柄コード   ※次の形式で入力してください。  [銘柄コード]@[市場コード]  ※市場コードは下記の定義値から選択してください。 市場コード                      定義値           説明                               1           東証                         3           名証                         5           福証                         6           札証            |

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Symbol | string | False | 銘柄コード  ※対象商品は、株式のみ |
        | RegulationsInfo | array<object> | False | 規制情報 |
        | RegulationsInfo..Exchange | integer(int32) | False | 規制市場 ; Enum: 0=全対象, 1=東証, 3=名証, 5=福証, 6=札証, 9=SOR, 10=CXJ, 21=JNX |
        | RegulationsInfo..Product | integer(int32) | False | 規制取引区分  ※空売り規制の場合、「4：新規」 ; Enum: 0=全対象, 1=現物, 2=信用新規（制度）, 3=信用新規（一般）, 4=新規, 5=信用返済（制度）, 6=信用返済（一般）, 7=返済, 8=品受, 9=品渡 |
        | RegulationsInfo..Side | string | False | 規制売買  ※空売り規制の場合、「1：売」 ; Enum: 0=全対象, 1=売, 2=買 |
        | RegulationsInfo..Reason | string | False | 理由 ※空売り規制の場合、「空売り規制」 |
        | RegulationsInfo..LimitStartDay | string | False | 制限開始日 yyyy/MM/dd HH:mm形式   ※空売り規制の場合、null |
        | RegulationsInfo..LimitEndDay | string | False | 制限終了日 yyyy/MM/dd HH:mm形式   ※空売り規制の場合、null |
        | RegulationsInfo..Level | integer(int32) | False | コンプライアンスレベル  ※空売り規制の場合、null ; Enum: ０=規制無し, １=ワーニング, ２=エラー |

        * Example:
        ```json
        {
          "Symbol": "5614",
          "RegulationsInfo": [
            {
              "Exchange": 1,
              "Product": 8,
              "Side": "2",
              "Reason": "品受停止（貸借申込停止銘柄（日証金規制））",
              "LimitStartDay": "2020/10/01 00:00",
              "LimitEndDay": "2999/12/31 00:00",
              "Level": 2
            },
            {
              "Exchange": 0,
              "Product": 1,
              "Side": "2",
              "Reason": "その他（代用不適格銘柄）",
              "LimitStartDay": "2021/01/27 00:00",
              "LimitEndDay": "2021/02/17 00:00",
              "Level": 2
            }
          ]
        }
        ```
* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `GET` /primaryexchange/{symbol}
**Summary:** 優先市場
株式の優先市場を取得する
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |
| symbol | path | string | True | 銘柄コード |

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Symbol | string | False | 銘柄コード ※対象商品は、株式のみ |
        | PrimaryExchange | integer(int32) | False | 優先市場 ; Enum: 1=東証, 3=名証, 5=福証, 6=札証 |

        * Example:
        ```json
        {
          "Symbol": "2928",
          "Exchange": 6
        }
        ```
* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `GET` /apisoftlimit
**Summary:** ソフトリミット
kabuステーションAPIのソフトリミット値を取得する
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Stock | number(double) | False | 現物のワンショット上限 ※単位は万円 |
        | Margin | number(double) | False | 信用のワンショット上限 ※単位は万円 |
        | Future | number(double) | False | 先物のワンショット上限 ※単位は枚 |
        | FutureMini | number(double) | False | ミニ先物のワンショット上限 ※単位は枚 |
        | FutureMicro | number(double) | False | マイクロ先物のワンショット上限 ※単位は枚 |
        | Option | number(double) | False | オプションのワンショット上限 ※単位は枚 |
        | MiniOption | number(double) | False | ミニオプションのワンショット上限 ※単位は枚 |
        | KabuSVersion | string | False | kabuステーションのバージョン |

        * Example:
        ```json
        {
          "Stock": 200,
          "Margin": 200,
          "Future": 10,
          "FutureMini": 100,
          "FutureMicro": 1000,
          "Option": 20,
          "MiniOption": 200,
          "KabuSVersion": "5.13.1.0"
        }
        ```
* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `GET` /margin/marginpremium/{symbol}
**Summary:** プレミアム料取得
指定した銘柄のプレミアム料を取得するAPI
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |
| symbol | path | string | True | 銘柄コード |

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Symbol | string | False | 銘柄コード |
        | GeneralMargin | object | False | 一般信用（長期） |
        | GeneralMargin..MarginPremiumType | integer(int32) | False | プレミアム料入力区分 ; Enum: null=一般信用（長期）非対応銘柄, 0=プレミアム料がない銘柄, 1=プレミアム料が固定の銘柄, 2=プレミアム料が入札で決定する銘柄 |
        | GeneralMargin..MarginPremium | number(double) | False | 確定プレミアム料  ※入札銘柄の場合、入札受付中は随時更新します。受付時間外は、確定したプレミアム料を返します。  ※非入札銘柄の場合、常に固定値を返します。  ※信用取引不可の場合、nullを返します。  ※19:30~翌営業日のプレミアム料になります。 |
        | GeneralMargin..UpperMarginPremium | number(double) | False | 上限プレミアム料  ※プレミアム料がない場合は、nullを返します。 |
        | GeneralMargin..LowerMarginPremium | number(double) | False | 下限プレミアム料  ※プレミアム料がない場合は、nullを返します。 |
        | GeneralMargin..TickMarginPremium | number(double) | False | プレミアム料刻値  ※入札可能銘柄以外は、nullを返します。 |
        | DayTrade | object | False | 一般信用（デイトレ） |
        | DayTrade..MarginPremiumType | integer(int32) | False | プレミアム料入力区分 ; Enum: null=一般信用（デイトレ）非対応銘柄, 0=プレミアム料がない銘柄, 1=プレミアム料が固定の銘柄, 2=プレミアム料が入札で決定する銘柄 |
        | DayTrade..MarginPremium | number(double) | False | 確定プレミアム料  ※入札銘柄の場合、入札受付中は随時更新します。受付時間外は、確定したプレミアム料を返します。  ※非入札銘柄の場合、常に固定値を返します。  ※信用取引不可の場合、nullを返します。  ※19:30~翌営業日のプレミアム料になります。 |
        | DayTrade..UpperMarginPremium | number(double) | False | 上限プレミアム料  ※プレミアム料がない場合は、nullを返します。 |
        | DayTrade..LowerMarginPremium | number(double) | False | 下限プレミアム料  ※プレミアム料がない場合は、nullを返します。 |
        | DayTrade..TickMarginPremium | number(double) | False | プレミアム料刻値  ※入札可能銘柄以外は、nullを返します。 |

        * Example:
        ```json
        {
          "Symbol": "9433",
          "GeneralMargin": {
            "MarginPremiumType": null,
            "MarginPremium": null,
            "UpperMarginPremium": null,
            "LowerMarginPremium": null,
            "TickMarginPremium": null
          },
          "DayTrade": {
            "MarginPremiumType": 2,
            "MarginPremium": 0.55,
            "UpperMarginPremium": 1,
            "LowerMarginPremium": 0.3,
            "TickMarginPremium": 0.01
          }
        }
        ```
* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



## register
### `PUT` /register
**Summary:** 銘柄登録
PUSH配信する銘柄を登録します。

API登録銘柄リストを開くには、kabuステーションAPIインジケーターを右クリックし「API登録銘柄リスト」を選択してください。
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |

**Request body**: (required)
* application/json
    | field | type | required | description |
    |-------|------|----------|-------------|
    | Symbols | array<object> | False |  |
    | Symbols..Symbol | string | False | 銘柄コード; Example: 9433 |
    | Symbols..Exchange | integer(int32) | False | 市場コード ; Enum: 1=東証, 3=名証, 5=福証, 6=札証, 2=日通し, 23=日中, 24=夜間; Example: 1 |


**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | RegistList | array<object> | False | 現在登録されている銘柄のリスト |
        | RegistList..Symbol | string | False | 銘柄コード; Example: 9433 |
        | RegistList..Exchange | integer(int32) | False | 市場コード ; Enum: 1=東証, 3=名証, 5=福証, 6=札証, 2=日通し, 23=日中, 24=夜間; Example: 1 |

* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `PUT` /unregister
**Summary:** 銘柄登録解除
API登録銘柄リストに登録されている銘柄を解除します
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |

**Request body**: (required)
* application/json
    | field | type | required | description |
    |-------|------|----------|-------------|
    | Symbols | array<object> | False | ※為替銘柄を登録する場合、銘柄名は"通貨A" + "/" + "通貨B"、市場コードは"300"で指定してください。 例：'Symbol': 'EUR/USD', "Exchange": 300 |
    | Symbols..Symbol | string | False | 銘柄コード; Example: 9433 |
    | Symbols..Exchange | integer(int32) | False | 市場コード ; Enum: 1=東証, 3=名証, 5=福証, 6=札証, 2=日通し, 23=日中, 24=夜間; Example: 1 |


**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | RegistList | array<object> | False | 現在登録されている銘柄のリスト |
        | RegistList..Symbol | string | False | 銘柄コード; Example: 9433 |
        | RegistList..Exchange | integer(int32) | False | 市場コード ; Enum: 1=東証, 3=名証, 5=福証, 6=札証, 2=日通し, 23=日中, 24=夜間; Example: 1 |

* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



### `PUT` /unregister/all
**Summary:** 銘柄登録全解除
API登録銘柄リストに登録されている銘柄をすべて解除します
| name | in | type | required | description |
|------|----|------|----------|-------------|
| X-API-KEY | header | string | True | トークン発行メソッドで取得した文字列 |

**Responses**:
* **200** – OK
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | RegistList | object | False | 現在登録されている銘柄のリスト ※銘柄登録解除が正常に行われれば、空リストを返します。 　登録解除でエラー等が発生した場合、現在登録されている銘柄のリストを返します; Example: [] |

* **400** – BadRequest
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **401** – Unauthorized
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **403** – Forbidden
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **404** – NotFound
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **405** – MethodNotAllowed
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **413** – RequestEntityTooLarge
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **415** – UnsupportedMediaType
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **429** – TooManyRequests
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |

* **500** – InternalServerError
    * application/json
        | field | type | required | description |
        |-------|------|----------|-------------|
        | Code | integer(int32) | False | エラーコード; Example: 4001001 |
        | Message | string | False | [エラーメッセージ](#message); Example: 内部エラー |



## message
The detailed list of error codes / messages is available in the official documentation.