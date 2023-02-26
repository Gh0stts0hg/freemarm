(namespace "free")

(module upgrade-utils GOVERNANCE


  (use kip.token-manifest)
  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

    (defschema change-ledger-schema
        max-version:integer
        ingestion-document:object{manifest}
        ingestion-hash:string
        ingestion-height:integer
    )

    (defschema certificate-schema
        token-id:string
        manifest-hash:string
        transformation-list:list
        transformation-hash:string
        block-height:integer
    )

    (defschema versioned-certificate-schema
        certificate:object{certificate-schema}
        certificate-hash:string
        previous-version-hash:string
    )

    (deftable change-ledger:{change-ledger-schema})
    (deftable versioned-certificates:{versioned-certificate-schema})


    (defun ingest-document(token-id:string doc:object{manifest})
      (let*
        (
            (certificate (create-init-certificate doc token-id))
        )
        (insert change-ledger token-id {
          "max-version": 0,
          "ingestion-document": doc,
          "ingestion-hash": (hash doc),
          "ingestion-height": (at "block-height" (chain-data))
          })

        (insert versioned-certificates (+ token-id ".0") {
          "certificate": certificate,
          "certificate-hash": (hash certificate),
          "previous-version-hash": ""
          })
      )
    )

    (defun create-init-certificate:object{certificate-schema} (manifest:object{manifest} token-id:string)
      {
        'token-id:token-id,
        'manifest-hash:(hash manifest),
        'transformation-list:[],
        'transformation-hash: "",
        'block-height:(at "block-height" (chain-data))
      }
    )

    (defun create-certificate:object{certificate-schema} (manifest:object{manifest} transformation-list:list token-id:string)
      {
        'token-id:token-id,
        'manifest-hash:(hash manifest),
        'transformation-list:transformation-list,
        'transformation-hash: (hash transformation-list),
        'block-height:(at "block-height" (chain-data))
      }
    )

    (defun upgrade-and-version:object{manifest} (curr-manifest:object{manifest} transformation-list:list token-id:string)

      (let* (
          (change-ledger:object{change-ledger-schema} (get-content-or-ingest token-id curr-manifest))
          (version:integer (at 'max-version change-ledger))
          (new-certificate-id (get-version-id (+ 1 version) token-id))
          (last-certification (get-certificate version token-id))
          (previous-hash (at 'certificate-hash last-certification ))
          (new-manifest:object{manifest} (upgrade-provider.provide curr-manifest transformation-list))
          (new-certificate (create-certificate new-manifest transformation-list token-id))
        )

        (increment-max-version token-id)

        (insert versioned-certificates new-certificate-id {
          "certificate": new-certificate,
          "certificate-hash": (hash new-certificate),
          "previous-version-hash": previous-hash
          })
          (get-certificate 1 token-id)

        new-manifest
      )
    )


    (defun get-content-or-ingest:object{change-ledger-schema} (token-id:string manifest:object{manifest})

      (with-default-read change-ledger token-id
        {
          "max-version": -1,
          "ingestion-document": manifest,
          "ingestion-hash": (hash manifest),
          "ingestion-height": (at "block-height" (chain-data))
        }
        {
        "max-version":= max-version,
        "ingestion-document":= ing-doc,
        "ingestion-hash":= ing-doc-hash,
        "ingestion-height":= ing-time
        }

        (if (< max-version 0) (ingest-document token-id manifest)
          {
          "max-version": max-version,
          "ingestion-document": ing-doc,
          "ingestion-hash": ing-doc-hash,
          "ingestion-height": ing-time
          }
        )
        (read change-ledger token-id)
      )
    )

    (defun get-max-version:object{change-ledger-schema} (token-id:string manifest:object{manifest})

      (with-default-read change-ledger token-id
        {
          "max-version": -1,
          "ingestion-document": manifest,
          "ingestion-hash": (hash manifest),
          "ingestion-height": (at "block-height" (chain-data))
        }
        {
        "max-version":= max-version,
        "ingestion-document":= ing-doc,
        "ingestion-hash":= ing-doc-hash,
        "ingestion-height":= ing-time
        }
        {
        "max-version": max-version,
        "ingestion-document": ing-doc,
        "ingestion-hash": ing-doc-hash,
        "ingestion-height": ing-time
        }
      )
    )

    (defun increment-max-version(token-id:string)
    (bind (read change-ledger token-id)
      {
        'max-version:= max-version
      }
      (update change-ledger token-id {
        'max-version: (+ 1 max-version)
      })
      "incremented max version"
    )
    )

    (defun get-certificate:{versioned-certificate-schema} (version:integer token-id:string)
      (read versioned-certificates (get-version-id version token-id))
    )

    (defun get-version-id (version:integer token-id:string)
      (+ token-id (+ "." (int-to-str 10 version)))
    )
)
(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table change-ledger)
    (create-table versioned-certificates) ])
