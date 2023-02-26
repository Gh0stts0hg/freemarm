(namespace "free")
(module universal-ledger GOVERNANCE


    (defcap GOVERNANCE ()
      (enforce-guard (keyset-ref-guard "free.universal-admin")))

  (defschema race-schema
    pact-id:string
    min-participants:integer
    max-participants:integer
    participants:[object:{participants-schema}]
    created-time:time
    expiration:integer
    race-state:string;;enum : { CREATED, STARTED, FAULT/EXPIRED?, FINISHED }
    reward-policy:module{free.reward-policy-v1}
    race-account:string
  )

  (defschema reward-input-params
    reward-policy:module{free.reward-policy-v1}
    min-participants:integer
    max-participants:integer
    expiration:integer
  )
  (deftable races:{race-schema})

  (defschema participants-schema
    account-id:string
    account-guard:guard
  )
      (defpact deftest:bool
        (
        )
        (step-with-rollback
          (create-race-state)
          (rollback-from-create)
        )
        (step-with-rollback
          (start-race)
          (rollback-from-start)
        )
        (step
          (finish-race)
          )
      )
  (defcap RACE_CREATED:bool
    ( pact-id:string
      expiration:integer
      state:string
    )
    @doc " For accounting via events."
    @event
    true
  )

  (defun create-race-state (
    pact-id:string)
    ;;1. create race params : race entry, min/max-participants,
    ;;2. set state to WAITING_PARTICIPANTS
    ;;3. create-pact escrow
    (let*
      (
        (spec:object{reward-input-params} (read-msg "reward-params"))
        (reward-policy:module{free.reward-policy-v1} (at 'reward-policy spec))
        (min-participants:integer (at 'min-participants spec))
        (max-participants:integer (at 'max-participants spec))
        (expiration:integer (at 'expiration spec))
        )
        (insert races pact-id
          { "pact-id" : pact-id,
            "reward-policy": reward-policy,
            "min-participants" : min-participants,
            "max-participants": max-participants,
            "expiration" : expiration,
            "state":"CREATED",
            "race-account": (race-account)
          })
          (emit-event RACE_CREATED pact-id expiration state)
    )
  )

  (defun get-race (id:string)
    (with-read tokens races
      { 'reward-policy := policy:module{free.reward-policy-v1},
        'expiration := expiration
        'state := state
      }
      { 'reward-policy: policy,
        'expiration: expiration,
        'state:state})
  )
    (defun partipate-in-race (
      race-id:string
      account-id:string
      account-guard:guard
      )
      (bind (get-race race-id)
        { 'reward-policy := reward-policy:module{free.reward-policy-v1},
          'expiration := expiration,
          'state := state
        }
        (enforce (= state "CREATED"))
        (enforce (can-join-race expiration))


        (policy::get-race-entry token account guard amount))

        ;;1. get race entry
        ;;2. get reward policy from entry
        ;;3. call get-entry from policy
        ;;4. charge entry to user
        ;;5. lock NFT
        ;;6. set in participants-list
        (format "" [])
      )

  (defun can-join-race (expiration:integer
    state:string
    max-participants:integer
    participants:[object:{participants}])
    (enforce (= "CREATED" state))
    ;;enforce hasnt reached max participants
    ;; enforce not expired

  )

  (defun race-account:string ()
    (create-principal (create-pact-guard "REWARDS"))
  )

  (defun rollback-from-create ()
  (update test "test"
    { "isTrue" : "false" })
  )

  (defun set-to-mid ()
  (update test "test"
    { "isTrue" : "true1" })
  )


  (defun get-test:bool ()
    (read test "test")
  )


)
