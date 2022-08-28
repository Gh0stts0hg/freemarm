
(namespace (read-msg 'ns))

(module change-ledger GOVERNANCE


;;TODO Move height to time it is better metric imo

  (use kip.token-manifest)
    (defcap GOVERNANCE ()
      (enforce-guard (keyset-ref-guard 'marmalade-admin)))


    (defschema token-ledger-schema
        max-version:integer
        ;; might remove ingestion-document and keep hash, it's useful but redundant, thoughts ?
        ingestion-document:object{manifest}
        ingestion-hash:string
        ingestion-height:integer
    )

    (defschema certificate-schema
        manifest-hash:string
        transformation-list:[string]
        transformation-hash:string
        block-height:integer
    )

    (defschema versioned-certificate-schema
        certificate:object{certificate-schema}
        certificate-hash:string
        previous-version-hash:string
    )

    (deftable token-ledger:{token-ledger-schema})
    (deftable versioned-certificate:{versioned-certificate-schema})

    (defun getallchanges()
      (map (read token-ledger) (keys token-ledger))
    )
    (defun ingest-document (token-id:string doc:object{manifest})

      (let*
        (
            (certificate (create-certificate doc))
        )

        (insert token-ledger token-id {
          "max-version": 0,
          "ingestion-document": doc,
          "ingestion-hash": (hash doc),
          "ingestion-time": (at "block-height" (chain-data))
          })

        (insert versioned-certificate (+ token-id ".0") {
          "certificate": certificate,
          "certificate-hash": (hash certificate),
          "previous-version-hash": "0"
          })
      )

    )

    (defun create-certificate:object{certificate-schema} (manifest:object{manifest})
      {
        'manifest-hash:(hash manifest),
        'transformation-list:[],
        'transformation-hash: "",
        'block-height:(at "block-height" (chain-data))
      }
    )

    (defun verify-latest-manifest (token-id:string manifest:object{manifest})

      (bind (read token-ledger token-id)
        { 'max-version:=version:integer,
          'ingestion-hash:=ingestion-hash:string
        }

        (let*
          (
            (manifest-hash (at 'hash manifest))
            (candidate-manifest-hash (if (= version 0)
              ingestion-hash
              (read versioned-certificate (+ (+ token-id ".") version))
            ))
          )
          manifest

        )


      )
      true
    )
    (defun verify-manifest-at-time (token-id:string block-time:integer manifest:object{manifest})
      true
    )

)

(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table token-ledger)
    (create-table versioned-certificate) ])
