(namespace "free")


(interface reward-policy-v1

  (defschema reward-schema
    id:string
    race-entry-fungible:module{fungible-v2}
    entry-amount:decimal
    )

  (defun get-reward-distribution:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    @doc "Minting policy for TOKEN to ACCOUNT for AMOUNT."
    @model [
      (property (!= account ""))
      (property (> amount 0.0))
    ]
  )

  (defun get-race-entry:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    @doc "Policy to get entry amount for a reward-schema."
    @model [
      (property (!= account ""))
      (property (> amount 0.0))
    ]
  )
)
