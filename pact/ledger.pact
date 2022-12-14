(namespace (read-msg 'ns))
;;
;; Create floating tokens as you would with marm (Except ID prefix added by ledger)
;; Or create tokens as part of explicit collection (1/1nfts only) to facilitate easier creation and managing of collections
;;
;; Minimal changes to ledger to support compat. from KC and friends
;;
;;
(module ledger GOVERNANCE

  @model
    [
      (defproperty valid-account (account:string)
          (> (length account) 2))
    ]

  (use util.fungible-util)
  (use kip.token-manifest)
  (use marmalade.change-ledger)

  (implements kip.poly-fungible-v2)
  (use kip.poly-fungible-v2 [account-details sender-balance-change receiver-balance-change])

  ;;
  ;; Tables/Schemas
  ;;

  (deftable ledger:{account-details})



;; indexed by effective id : {collection.tokenid}
  (defschema token-schema
    id:string
    manifest:object{manifest}
    precision:integer
    supply:decimal
    policy:module{kip.token-policy-v1}
  )


;; TODO : Move to policy based on indexing approach we discussed on git
  (defschema nft-collectioned-token-schema
    collection-id:string
    max-unique-token-supply:integer
    current-unique-token-supply:integer

    available-token-ids:list
    collection-guard:guard
    collection-wide-policy:module{kip.token-policy-v1}
  )


  (deftable tokens:{token-schema})
  (deftable collections:{nft-collectioned-token-schema})

  ;;
  ;; Capabilities
  ;;

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin)))

  ;;
  ;; poly-fungible-v2 caps
  ;;

  (defcap TRANSFER:bool
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    @managed amount TRANSFER-mgr
    (enforce-unit id amount)
    (enforce (> amount 0.0) "Amount must be positive")
    (compose-capability (DEBIT id sender))
    (compose-capability (CREDIT id receiver))
  )

  (defcap XTRANSFER:bool
    ( id:string
      sender:string
      receiver:string
      target-chain:string
      amount:decimal
    )
    @managed amount TRANSFER-mgr
    (enforce false "cross chain not supported")
  )

  (defun TRANSFER-mgr:decimal
    ( managed:decimal
      requested:decimal
    )
    (let ((newbal (- managed requested)))
      (enforce (>= newbal 0.0)
        (format "TRANSFER exceeded for balance {}" [managed]))
      newbal)
  )

  (defcap SUPPLY:bool (id:string supply:decimal)
    @doc " Emitted when supply is updated, if supported."
    @event true
  )

  (defcap TOKEN:bool (id:string precision:integer supply:decimal policy:module{kip.token-policy-v1})
    @event
    true
  )

  (defcap RECONCILE:bool
    ( token-id:string
      amount:decimal
      sender:object{sender-balance-change}
      receiver:object{receiver-balance-change}
    )
    @doc " For accounting via events. \
         \ sender = {account: '', previous: 0.0, current: 0.0} for mint \
         \ receiver = {account: '', previous: 0.0, current: 0.0} for burn"
    @event
    true
  )

  (defcap MANIFEST_UPDATE
    ( token-id:string
      new-manifest:object{manifest}
    )
    @doc "Event emitted when manifest for a token is changed"
    @event
    true
  )

  (defcap ACCOUNT_GUARD:bool (id:string account:string guard:guard)
    @doc " Emitted when ACCOUNT guard is updated."
    @event
    true
  )

  ;;
  ;; Implementation caps
  ;;

  (defcap ROTATE (id:string account:string)
    @doc "Autonomously managed capability for guard rotation"
    @managed
    true)

  (defcap DEBIT (id:string sender:string)
    (enforce-guard (account-guard id sender))
  )

  (defun account-guard:guard (id:string account:string)
    (with-read ledger (key id account) { 'guard := g } g)
  )

  (defcap CREDIT (id:string receiver:string) true)

  (defcap UPDATE_SUPPLY ()
    "private cap for update-supply"
    true)

  (defcap MINT (id:string account:string amount:decimal)
    @managed ;; one-shot for a given amount
    (enforce (< 0.0 amount) "Amount must be positive")
    (compose-capability (CREDIT id account))
    (compose-capability (UPDATE_SUPPLY))
  )

  (defcap BURN (id:string account:string amount:decimal)
    @managed ;; one-shot for a given amount
    (enforce (< 0.0 amount) "Amount must be positive")
    (compose-capability (DEBIT id account))
    (compose-capability (UPDATE_SUPPLY))
  )

  (defun ledger-guard:guard ()
    @doc "Ledger module guard for policies to be able to validate access to policy operations."
    (create-module-guard "ledger-guard")
  )

  (defschema policy-info
    policy:module{kip.token-policy-v1}
    token:object{kip.token-policy-v1.token-info}
  )

  (defun get-policy-info:object{policy-info} (id:string)
    (with-read tokens id
      { 'policy := policy:module{kip.token-policy-v1}
      , 'supply := supply
      , 'precision := precision
      , 'manifest := manifest
      }
      { 'policy: policy
      , 'token:
        { 'id: id
        , 'supply: supply
        , 'precision: precision
        , 'manifest: manifest
        } } )
  )

  (defun get-collection-info:object{nft-collectioned-token-schema} (id:string)
    (with-read collections id
      { 'collection-wide-policy := collection-wide-policy
      , 'collection-id := collection-id
      , 'max-unique-token-supply := max-supply
      , 'current-unique-token-supply:= curr-supply
      , 'available-token-ids:=available-tokens
      , 'collection-guard:=collection-guard
      }
        { 'collection-wide-policy : collection-wide-policy
        , 'collection-id : collection-id
        , 'max-unique-token-supply : max-supply
        , 'current-unique-token-supply: curr-supply
        , 'available-token-ids:available-tokens
        , 'collection-guard:collection-guard
        } )
  )


  (defun create-account:bool
    ( id:string
      account:string
      guard:guard
    )
    (enforce-valid-account account)
    (enforce-reserved account guard)
    (insert ledger (key id account)
      { "balance" : 0.0
      , "guard"   : guard
      , "id" : id
      , "account" : account
      })
    (emit-event (ACCOUNT_GUARD id account guard))
  )

  (defun create-collection:bool
    (
      collection-id:string
      collection-guard:guard
      max-unique-supply:integer
      collection-policy:module{kip.token-policy-v1}

    )
    (enforce-valid-collection collection-id)
    (insert collections collection-id
      { "collection-id" : collection-id
      , "max-unique-token-supply"   : max-unique-supply
      , "current-unique-token-supply" : 0
      , "available-token-ids" : []
      , "collection-guard": collection-guard
      , "collection-wide-policy": collection-policy
      })
      (format "collection created for {}" [collection-id])
    ;(emit-event (COLLECTION_CREATED id account guard))
  )


  (defun enforce-valid-collection (collection-id:string)

    ;;(enforce (= false (row-exists 'collections collection-id "before")) "Collection already exists")
    (enforce (!= collection-id "global") "Reserved collection-name for floating tokens")

  )



  (defun total-supply:decimal (id:string)
    (with-default-read tokens id
      { 'supply : 0.0 }
      { 'supply := s }
      s)
  )

  (defun create-token:bool
    ( id:string
      precision:integer
      manifest:object{manifest}
      policy:module{kip.token-policy-v1}
    )
    ;;TODO: remove indexed IDs, move organization of collections to policy for now
    (let*
        (
          (indexed-id (get-token-id-indexed-by-collection "global" id))
        )
        (enforce-verify-manifest manifest)
        (policy::enforce-init
          { 'id: indexed-id, 'supply: 0.0, 'precision: precision, 'manifest: manifest })
        (insert tokens indexed-id {
          "id": indexed-id,
          "precision": precision,
          "manifest": manifest,
          "supply": 0.0,
          "policy": policy
          })
          (emit-event (TOKEN indexed-id precision 0.0 policy))

          (change-ledger.ingest-document indexed-id manifest)
          ;;TODO emit event
          indexed-id
    )

  )


  (defun create-token-for-collection:bool
    ( collection-id:string
      token-id:string
      manifest:object{manifest}
      policy:module{kip.token-policy-v1}
    )
    (enforce-verify-manifest manifest)
    ;; enforce collection guard
    ;; enforce max unique SUPPLY
    (enforce-collection collection-id)

    (let*
      (
        (indexed-id (get-token-id-indexed-by-collection collection-id token-id))
      )
      (bind (get-collection collection-id)
        { 'collection-guard:=mint-guard:guard
        , 'collection-id:=collection-id:string
        , 'max-unique-token-supply:=max-amount:integer
        , 'current-unique-token-supply:=curr-amount:integer
        , 'available-token-ids:=available-token-list:list
        }

        (policy::enforce-init
          { 'id: indexed-id, 'supply: 0.0, 'precision: 0, 'manifest: manifest })
        (insert tokens indexed-id {
          "id": indexed-id,
          "precision": 0,
          "manifest": manifest,
          "supply": 0.0,
          "policy": policy
          }
        )

;;;;;;;; NOTE: Here we actually should be updating unique supply, not at mint, because collections
;;;;;;;;       assume that we are at 0 precision 1 supply creation is better place to increment.


;;;;;;;; If we don't then we can create tokens to exceed max supply and have floating available tokens list
;;;;;;;; not the end of the world but meh
        (update collections collection-id {
          "available-token-ids": (+ available-token-list [indexed-id])
          }
        )
      )
      (emit-event (TOKEN indexed-id 0 0.0 policy))
      (format "created token {} for collection {}" [indexed-id collection-id])
    )
  )


;; VERYYYYYYY TESTY code just to plug in an arbitrary modification to manifest and return new manifest
;; LOOOL long term looks like for an "add value" transformation for example:

;; unbox untill path

;; box and add back in new val
  (defun change-manifest:object{manifest} (manifest:object{manifest} stat-key:string new-val:string)

    (let*
      (

        (manifestdata (at 'data manifest))
        (manifesttofill (filter (compose (filterobj) (!= 'mutable-state-data)) manifestdata) )
        (manifestmutableoriginal (filter (compose (filterobj) (= 'mutable-state-data)) manifestdata) )
        (manifestOriginal-nodatum (map (filterdatumfromobj) manifestmutableoriginal) )

        (datum  (at 0 (at 'data manifest)))
        (datumfiltered (remove "components" (at 'datum datum)))


        (complist (at 'components (at 'datum (at 0 (at 'data manifest)))))

        (complistfiltered (filter (where 'name (!= "body")) complist))


        (body (at 0(filter (where 'name (= "body")) complist)))
        (bodyfiltered (remove "stats" body))

        (stats (at 'stats body))


        (aero (filter (where 'key (!= "aerodynamic-factor")) stats))
        (test  {"aerodynamic-factor":new-val})
        (aeronew (+ [test] aero))
        (bodyfinal (+ bodyfiltered {"stats":aeronew}))
        (finalcomp (+ [bodyfinal] complistfiltered))
        (datumfinal (+ {"components":finalcomp} datumfiltered))
        (datumbackfilled (+ {"datum":datumfinal} (at 0 manifestOriginal-nodatum)))
        (backfilledmanifestithink (+ manifesttofill [datumbackfilled]))


      )
      backfilledmanifestithink
      ;aero

    )
;  (filter (at 'components (at 'datum (at 0 (at 'data manifest)))) (where 'name (= "body")))
  )

  (defun filterobj (object:object)

    (at 'data (at 'uri object))
  )

  (defun filterdatumfromobj (object:object)

    (remove "datum" object)
  )

  (defun get-token-id-indexed-by-collection:string (collection-id:string original-token-id:string)
    (format "{}.{}" [collection-id original-token-id])
  )

  (defun enforce-collection
    (
      collection-id:string
    )
    (bind (get-collection collection-id)
      { 'collection-guard:=collection-guard:guard
      , 'max-unique-token-supply:=max-amount:integer
      , 'current-unique-token-supply:=curr-amount:integer
      }
      (enforce-guard collection-guard)
      (enforce (>= max-amount (+ curr-amount 1)) "Exceeds max collection nft supply")
    )
  )

  (defun get-collection:object{nft-collectioned-token-schema}
    (
      collection-id:string
    )

    (read collections collection-id)
  )
  (defun truncate:decimal (id:string amount:decimal)
    (floor amount (precision id))
  )

  (defun get-balance:decimal (id:string account:string)
    (at 'balance (read ledger (key id account)))
  )

  (defun details:object{account-details}
    ( id:string account:string )
    (read ledger (key id account))
  )

  (defun rotate:bool (id:string account:string new-guard:guard)
    (with-capability (ROTATE id account)
      (enforce-transfer-policy id account account 0.0)
      (with-read ledger (key id account)
        { "guard" := old-guard }

        (enforce-guard old-guard)
        (update ledger (key id account)
          { "guard" : new-guard })
        (emit-event (ACCOUNT_GUARD id account new-guard)))))

  (defun transfer:bool
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")
    (enforce-valid-transfer sender receiver (precision id) amount)
    (with-capability (TRANSFER id sender receiver amount)
      (enforce-transfer-policy id sender receiver amount)
      (with-read ledger (key id receiver)
        { "guard" := g }
        (let
          ( (sender (debit id sender amount))
            (receiver (credit id receiver g amount))
          )
          (emit-event (RECONCILE id amount sender receiver))
        )
      )
    )
  )

  (defun enforce-transfer-policy
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    (bind (get-policy-info id)
      { 'policy := policy:module{kip.token-policy-v1}
      , 'token := token }
      (policy::enforce-transfer token sender (account-guard id sender) receiver amount))
  )

  (defun transfer-create:bool
    ( id:string
      sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal
    )
    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")
    (enforce-valid-transfer sender receiver (precision id) amount)

    (with-capability (TRANSFER id sender receiver amount)
      (enforce-transfer-policy id sender receiver amount)
      (let
        (
          (sender (debit id sender amount))
          (receiver (credit id receiver receiver-guard amount))
        )
        (emit-event (RECONCILE id amount sender receiver))
      ))
  )

  (defun mint:bool
    ( id:string
      account:string
      guard:guard
      amount:decimal
    )
    ;;;;;;; CHECK If collection based because you should update collections

    (with-capability (MINT id account amount)
      (bind (get-policy-info id)
        { 'policy := policy:module{kip.token-policy-v1}
        , 'token := token }
        (policy::enforce-mint token account guard amount))
      (let
        (
          (receiver (credit id account guard amount))
          (sender:object{sender-balance-change}
            {'account: "", 'previous: 0.0, 'current: 0.0})
        )
        (emit-event (RECONCILE id amount sender receiver))
        (update-supply id amount)
      ))
  )

  (defun mint-token-from-collection:bool
    ( collection-id:string
      account:string
      account-guard:guard
      amount:integer
    )
      (bind (get-collection-info collection-id)
        { 'available-token-ids := available-token-ids
          ,'current-unique-token-supply := current-unique-token-supply
        }

        (let*
          (
            (token-id (get-token-id-from-collection-available-tokens collection-id))
            (final-available (filter (!= token-id) available-token-ids))
            (minted (mint token-id account account-guard 1.0 ))
          )
          (update collections collection-id {
                          'available-token-ids: (filter (!= token-id) available-token-ids),
                          'current-unique-token-supply: (+ current-unique-token-supply 1)
          })


          (format "mint result {} for token-id {}" [minted token-id])
        )
      )

  )

  (defun get-token-id-from-collection-available-tokens (collection-id:string)

    (bind (get-collection-info collection-id)
      { 'available-token-ids := available-token-ids
      }
      (let* (
              (random:integer (get-rand))
              (current-length:integer (length available-token-ids))
              (index:integer (mod random current-length))
              (token-id:string (at index available-token-ids))
            )
            token-id
       )
    )
  )

;; credit to babena folks
  (defun get-rand:integer ()
    (let* (
      (prev-block-hash (at "prev-block-hash" (chain-data)))
      (random (str-to-int 64 (hash (+ prev-block-hash (hash (at 'block-time (chain-data)))))))
    )
    random
    )
  )

  (defun upgrade-manifest:bool
    ( token-id:string
      account-id:string
      collection-id:string
      new-manifest:object{manifest}
    )
    ;;enforce ownership & for now locked precision & supply nft
    (with-read ledger (key token-id account-id)
      { "balance" := old-bal
      ,"account" := account
      ,"guard" := account-guard }

        (bind (get-policy-info token-id)
          { 'policy := policy:module{kip.token-policy-v1}
          , 'token := token
          }

          ;;;;;; for now lock to single supply 0 precision nfts
          (enforce (= (at 'supply token) 1.0) "CANT UPGRADE THIS")
          (enforce (= (at 'precision token) 0) "CANT UPGRADE THIS")
          (enforce (= account account-id) "Lack of dom ownership over NFT")
          (enforce (= old-bal 1.0) "Lack of dom ownership over NFT")
          ;;;;;; enforce that nft owner signed tx
          (enforce-guard account-guard)
          (bind (get-collection-info collection-id)
          { 'collection-wide-policy := collection-wide-policy
          , 'collection-id := collection-id
          , 'max-unique-token-supply := max-supply
          , 'current-unique-token-supply:= curr-supply
          , 'available-token-ids:=available-tokens
          , 'collection-guard:=collection-guard
          }
          (enforce-guard collection-guard)

          )
        )
        (update tokens token-id
          { "manifest" : new-manifest }
        )
      )
    ;;(time-stamping-auth::broadcast-change token-id old-manifest new-manifest block-time)

     (emit-event (MANIFEST_UPDATE token-id new-manifest))
  )


  (defun get-collection-from-id (token-id:string)

    (format "todo" [])
  )

  (defun burn:bool
    ( id:string
      account:string
      amount:decimal
    )
    (with-capability (BURN id account amount)
      (bind (get-policy-info id)
        { 'policy := policy:module{kip.token-policy-v1}
        , 'token := token }
        (policy::enforce-burn token account amount))
      (let
        (
          (sender (debit id account amount))
          (receiver:object{receiver-balance-change}
            {'account: "", 'previous: 0.0, 'current: 0.0})
        )
        (emit-event (RECONCILE id amount sender receiver))
        (update-supply id (- amount))
      ))
  )

  (defun debit:object{sender-balance-change}
    ( id:string
      account:string
      amount:decimal
    )

    (require-capability (DEBIT id account))

    (enforce-unit id amount)

    (with-read ledger (key id account)
      { "balance" := old-bal }

      (enforce (<= amount old-bal) "Insufficient funds")

      (let ((new-bal (- old-bal amount)))
        (update ledger (key id account)
          { "balance" : new-bal }
          )
        {'account: account, 'previous: old-bal, 'current: new-bal}
      ))
  )

  (defun credit:object{receiver-balance-change}
    ( id:string
      account:string
      guard:guard
      amount:decimal
    )
    @doc "Credit AMOUNT to ACCOUNT balance"

    @model [ (property (> amount 0.0))
             (property (valid-account account))
           ]
    (enforce-valid-account account)
    (enforce-unit id amount)

    (require-capability (CREDIT id account))

    (with-default-read ledger (key id account)
      { "balance" : -1.0, "guard" : guard }
      { "balance" := old-bal, "guard" := retg }
      (enforce (= retg guard)
        "account guards do not match")

      (let* ((is-new
               (if (= old-bal -1.0)
                   (enforce-reserved account guard)
                 false))
              (new-bal (if is-new amount (+ old-bal amount)))
            )

      (write ledger (key id account)
        { "balance" : new-bal
        , "guard"   : retg
        , "id"   : id
        , "account" : account
        })
        (if is-new (emit-event (ACCOUNT_GUARD id account retg)) true)
        {'account: account, 'previous: (if is-new 0.0 old-bal), 'current: new-bal}
      ))
  )

  (defun credit-account:object{receiver-balance-change}
    ( id:string
      account:string
      amount:decimal
    )
    @doc "Credit AMOUNT to ACCOUNT"
    (credit id account (account-guard id account) amount)
  )

  (defun update-supply:bool (id:string amount:decimal)
    (require-capability (UPDATE_SUPPLY))
    (with-default-read tokens id
      { 'supply: 0.0 }
      { 'supply := s }
      (let ((new-supply (+ s amount)))
        (update tokens id {'supply: new-supply })
        (emit-event (SUPPLY id new-supply))))
  )

  (defun enforce-unit:bool (id:string amount:decimal)
    (let ((p (precision id)))
    (enforce
      (= (floor amount p)
         amount)
      "precision violation"))
  )

  (defun precision:integer (id:string)
    (at 'precision (read tokens id))
  )

  (defpact transfer-crosschain:bool
    ( id:string
      sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )
    (step (format "{}" [(enforce false "cross chain not supported")]) false))

  ;;
  ;; ACCESSORS
  ;;

  (defun key:string ( id:string account:string )
    @doc "DB key for ledger account"
    (format "{}:{}" [id account])
  )

  (defun get-manifest:object{manifest} (id:string)
    (at 'manifest (read tokens id)))

  ;;
  ;; sale
  ;;

  (defcap SALE:bool
    (id:string seller:string amount:decimal timeout:integer sale-id:string)
    @doc "Wrapper cap/event of SALE of token ID by SELLER of AMOUNT until TIMEOUT block height."
    @event
    (enforce (> amount 0.0) "Amount must be positive")
    (compose-capability (OFFER id seller amount timeout))
    (compose-capability (SALE_PRIVATE sale-id))
  )

  (defcap OFFER:bool
    (id:string seller:string amount:decimal timeout:integer)
    @doc "Managed cap for SELLER offering AMOUNT of token ID until TIMEOUT."
    @managed
    (enforce (sale-active timeout) "SALE: invalid timeout")
    (compose-capability (DEBIT id seller))
    (compose-capability (CREDIT id (sale-account)))
  )

  (defcap WITHDRAW:bool
    (id:string seller:string amount:decimal timeout:integer sale-id:string)
    @doc "Withdraws offer SALE from SELLER of AMOUNT of token ID after timeout."
    @event
    (enforce (not (sale-active timeout)) "WITHDRAW: still active")
    (compose-capability (DEBIT id (sale-account)))
    (compose-capability (CREDIT id seller))
    (compose-capability (SALE_PRIVATE sale-id))
  )

  (defcap BUY:bool
    (id:string seller:string buyer:string amount:decimal timeout:integer sale-id:string)
    @doc "Completes sale OFFER to BUYER."
    @managed
    (enforce (sale-active timeout) "BUY: expired")
    (compose-capability (DEBIT id (sale-account)))
    (compose-capability (CREDIT id buyer))
    (compose-capability (SALE_PRIVATE sale-id))
  )

  (defcap SALE_PRIVATE:bool (sale-id:string) true)

  (defpact sale:bool
    ( id:string
      seller:string
      amount:decimal
      timeout:integer
    )
    (step-with-rollback
      (with-capability (SALE id seller amount timeout (pact-id))
        (offer id seller amount))
      (with-capability (WITHDRAW id seller amount timeout (pact-id))
        (withdraw id seller amount))
    )
    (step
      (let ( (buyer:string (read-msg "buyer"))
             (buyer-guard:guard (read-msg "buyer-guard")) )
        (with-capability (BUY id seller buyer amount timeout (pact-id))
          (buy id seller buyer buyer-guard amount (pact-id)))))
  )

  (defun offer:bool
    ( id:string
      seller:string
      amount:decimal
    )
    @doc "Initiate sale with by SELLER by escrowing AMOUNT of TOKEN until TIMEOUT."
    (require-capability (SALE_PRIVATE (pact-id)))
    (bind (get-policy-info id)
      { 'policy := policy:module{kip.token-policy-v1}
      , 'token := token }
      (policy::enforce-offer token seller amount (pact-id)))
    (let
      (
        (sender (debit id seller amount))
        (receiver (credit id (sale-account) (create-pact-guard "SALE") amount))
      )
      (emit-event (TRANSFER id seller (sale-account) amount))
      (emit-event (RECONCILE id amount sender receiver)))
  )

  (defun withdraw:bool
    ( id:string
      seller:string
      amount:decimal
    )
    @doc "Withdraw offer by SELLER of AMOUNT of TOKEN before TIMEOUT"
    (require-capability (SALE_PRIVATE (pact-id)))
    (let
      (
        (sender (debit id (sale-account) amount))
        (receiver (credit-account id seller amount))
      )
      (emit-event (TRANSFER id (sale-account) seller amount))
      (emit-event (RECONCILE id amount sender receiver)))
  )


  (defun buy:bool
    ( id:string
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string
    )
    @doc "Complete sale with transfer."
    (require-capability (SALE_PRIVATE (pact-id)))
    (bind (get-policy-info id)
      { 'policy := policy:module{kip.token-policy-v1}
      , 'token := token }
      (policy::enforce-buy token seller buyer buyer-guard amount sale-id))
    (let
      (
        (sender (debit id (sale-account) amount))
        (receiver (credit id buyer buyer-guard amount))
      )
      (emit-event (TRANSFER id (sale-account) buyer amount))
      (emit-event (RECONCILE id amount sender receiver)))
  )

  (defun sale-active:bool (timeout:integer)
    @doc "Sale is active until TIMEOUT block height."
    (< (at 'block-height (chain-data)) timeout)
  )

  (defun sale-account:string ()
    (create-principal (create-pact-guard "SALE"))
  )
  (defun get-account-minted:integer (account:string)
      (with-default-read ledger account
        {"balance": 0}
        {"balance":= balance}
      balance
      )
    )




)

(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table ledger)
    (create-table tokens)
    (create-table collections) ])
