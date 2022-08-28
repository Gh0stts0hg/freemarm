(namespace "free")

(module kadcars-nft-policy GOVERNANCE
  @doc "Policy for fixed issuance with royalty and quoted sale in specified fungible."


  (defconst ADMIN_ADDRESS "k:f157854c15e9bb8fb55aafdecc1ce27a3d60973cbe6870045f4415dc06be06f5"
    @doc "admin address which also recieves mint payouts")

  (defconst ROYALTY_ADDRESS "royalty_dao"
    @doc "address for royalty payouts, which ideally will feed into splitter with business rules")

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))
  (defcap BUY (id:string receiver:string)
   (compose-capability (UPDATE-OWNER id receiver)))
  (defcap UPDATE-OWNER (token-id:string new-owner:string)
    true)

  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])

  (defschema collections-schema
    collection-id:string
    collection-guard:guard
    token-list:[string]
    max-unique-supply:integer
  )

  (defschema token-policy-schema
    fungible:module{fungible-v2}
    creator:string
    creator-guard:guard
    mint-guard:guard
    royalty-rate:decimal
    collection-id:string
    owner:string
  )

  (defschema effecient-token-policy-schema
    token-id:string
    fungible:module{fungible-v2}
    creator:string
    creator-guard:guard
    mint-guard:guard
    royalty-rate:decimal
    collection-id:string
    owner:string
  )

  (deftable tokens:{effecient-token-policy-schema})
  (deftable collections:{collections-schema})


  (defconst TOKEN_SPEC "token_spec"
    @doc "Payload field for token spec")

    (defconst COLLECTION_SUPPLY "collection-unique-supply"
      @doc "Max Unique Tokens in a collection")

  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")

  (defschema quote-spec
    @doc "Quote data to include in payload"
    price:decimal
    recipient:string
    recipient-guard:guard
    )

  (defschema quote-schema
    id:string
    spec:object{quote-spec})

  (deftable quotes:{quote-schema})

  (defun get-policy:object{token-policy-schema} (token:object{token-info})
    (read tokens (at 'id token))
  )

  (defun get-collection:object{collections-schema} (collection-id:string)
    (read collections collection-id)
  )

  (defcap QUOTE:bool
    ( sale-id:string
      token-id:string
      amount:decimal
      price:decimal
      sale-price:decimal
      royalty-payout:decimal
      creator:string
      spec:object{quote-spec}
    )
    @doc "For event emission purposes"
    @event
    true
  )

  (defun enforce-ledger:bool ()
     (enforce-guard (free.universal-ledger.ledger-guard))
   )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
    (bind (get-policy token)
      { 'mint-guard:=mint-guard:guard
      }
      (enforce-guard mint-guard)
        (coin.transfer account ADMIN_ADDRESS 1.1)
  ))

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    (enforce false "Burn prohibited")
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (enforce-ledger)
    (let* ( (spec:object{token-policy-schema} (read-msg TOKEN_SPEC))
            (fungible:module{fungible-v2} (at 'fungible spec))
            (creator:string (at 'creator spec))
            (creator-guard:guard (at 'creator-guard spec))
            (mint-guard:guard (at 'mint-guard spec))
            (royalty-rate:decimal (at 'royalty-rate spec))
            (collection-identifier:string (at 'collection-id spec))
            (creator-details:object (fungible::details creator ))
            )

      (enforce (=
        (at 'guard creator-details) creator-guard)
        "Creator guard does not match")

      ;;TODO : COLLECTION ENFORCEMENT
      (enforce-collection collection-identifier creator-guard)
      (enforce (and
        (>= royalty-rate 0.0) (<= royalty-rate 1.0))
        "Invalid royalty rate")
      (insert tokens (at 'id token)
        {'token-id: (at 'id token)
        ,'fungible: fungible
        , 'creator: creator
        , 'owner: creator
        , 'creator-guard: creator-guard
        , 'mint-guard: mint-guard
        , 'collection-id:collection-identifier
        , 'royalty-rate: royalty-rate }
        )
        (bind (get-collection collection-identifier)
          {'token-list:=tokenList}
          (update collections collection-identifier
            {'token-list: (+ [(at 'id token)] tokenList)}
          )
        )
      )


    true
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
    @doc "Capture quote spec for SALE of TOKEN from message"
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (bind (get-policy token)
      { 'fungible := fungible:module{fungible-v2}
       ,'royalty-rate:= royalty-rate:decimal
       ,'creator:= creator:string
      }
    (let* ( (spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
            (price:decimal (at 'price spec))
            (recipient:string (at 'recipient spec))
            (recipient-guard:guard (at 'recipient-guard spec))
            (recipient-details:object (fungible::details recipient))
            (sale-price:decimal (* amount price))
            (royalty-payout:decimal
               (floor (* sale-price royalty-rate) (fungible::precision))) )
      (fungible::enforce-unit sale-price)
      (enforce (< 0.0 price) "Offer price must be positive")
      (enforce (=
        (at 'guard recipient-details) recipient-guard)
        "Recipient guard does not match")
      (insert quotes sale-id { 'id: (at 'id token), 'spec: spec })
      (emit-event (QUOTE sale-id (at 'id token) amount price sale-price royalty-payout creator spec)))
      true
  )
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (with-capability (BUY (at 'id token) buyer)
      (enforce-sale-pact sale-id)
      (bind (get-policy token)
        { 'fungible := fungible:module{fungible-v2}
        , 'creator:= creator:string
        , 'royalty-rate:= royalty-rate:decimal
        }
        (with-read quotes sale-id { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
          (enforce (= qtoken (at 'id token)) "incorrect sale token")
          (bind spec
            { 'price := price:decimal
            , 'recipient := recipient:string
            }
            (let* ((sale-price:decimal (* amount price))
                   (royalty-payout:decimal
                      (floor (* sale-price royalty-rate) (fungible::precision)))
                   (payout:decimal (- sale-price royalty-payout)) )
              (if
                (> royalty-payout 0.0)
                (fungible::transfer buyer creator royalty-payout)
                "No royalty")
              (fungible::transfer buyer recipient payout)))
              true
              (update-owner qtoken buyer)
        )))
  )

  (defun enforce-sale-pact:bool (sale:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale (pact-id)) "Invalid pact/sale id")
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce-ledger)
    (enforce false "Transfer prohibited")
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce-ledger)
    (enforce false "Transfer prohibited")
  )

  (defun update-owner (token-id:string new-owner:string)
      (require-capability (UPDATE-OWNER token-id new-owner))
      (update tokens token-id
        {'owner: new-owner}
      )
  )

  (defun create-collection ()
    (with-capability (GOVERNANCE)
    (let* (
            (creator-guard:guard (read-msg "creator-guard"))
            (collection-identifier:string (read-msg 'collection-id))
            (collection-max-unique-supply:integer (read-integer COLLECTION_SUPPLY))
            )
            ;;TODO : Move collection-id to pull from make of manifest
          (insert collections collection-identifier
              {'collection-id:collection-identifier
              ,'collection-guard:creator-guard
              ,'token-list:[]
              ,'max-unique-supply:collection-max-unique-supply})
          )
          true
    )
  )

    (defun enforce-collection (collection-id:string guard:guard)

      (bind (get-collection collection-id)
        {'collection-guard:=collection-guard}
        (enforce-guard collection-guard)
      )
    )


    ;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;

    (defun get-tokens-for-collection (collection-id:string)

       (at "token-list" (read collections collection-id ["token-list"]))
    )

    (defun get-cars-in-collection-by-owner (collection-id:string owner:string)
      @doc "All cars under collections and owner"
      (select tokens ["token-id"](where 'owner (= owner)))
    )

)

(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table quotes)
    (create-table collections)
    (create-table tokens) ])
