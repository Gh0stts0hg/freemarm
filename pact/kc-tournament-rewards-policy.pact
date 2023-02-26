(namespace "free")

(module kc-tournament-rewards-policy GOVERNANCE

(implements free.reward-policy-v1)


    (defcap GOVERNANCE ()
      (enforce-guard (keyset-ref-guard "free.kc-admin")))

    (deftable rewards:{reward-schema})


        (defun get-reward-distribution:bool
          ( token:object{token-info}
            account:string
            guard:guard
            amount:decimal
          )
          @doc "Minting policy for TOKEN to ACCOUNT for AMOUNT."

            (format "{}#{}:{}" [make model vin])
        )

        (defun get-race-entry:bool
          ( token:object{token-info}
            account:string
            amount:decimal
          )
          @doc "Policy to get entry amount for a reward-schema."
            (format "{}#{}:{}" [make model vin])
        )




)
