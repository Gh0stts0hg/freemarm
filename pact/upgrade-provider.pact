(namespace "free")

(module upgrade-provider GOVERNANCE
  (use kip.token-manifest)
  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

;;type is enum
;; add
;; replace (all)
;; remove (all)
;; uri (top level replace)
(defschema transform
  type:string
  obj
)


  (defun provide:object{manifest} (manifest:object{manifest} transformation-list:list)
    (fold (provide-transform) manifest transformation-list)
  )

  (defun provide-transform:object{manifest} (manifest:object{manifest} transformation:object )
    (let*
      (
          (transform-inner (at 'transform transformation))
          (transform-type (at 'type (at 'transform transformation)))
      )
      (cond
        ((= transform-type "add") (upgrade-manifest-add-datum manifest (at 'obj transform-inner)))
        ((= transform-type "replace") (upgrade-manifest-replace-datum-at-uri manifest (at 'obj transform-inner)))
        ((= transform-type "remove") (upgrade-manifest-remove-datum-at-uri manifest (at 'obj transform-inner)))
        ((= transform-type "uri") (upgrade-manifest-replace-uri manifest (at 'obj transform-inner)))
        manifest
      )
    )
  )


  (defun upgrade-manifest-replace-datum-at-uri (manifest:object{manifest} obj)
    (let*
      (
        (uri (at 'uri obj))
        (new-datum (at 'new-datum obj))
        (datal:[object{mf-datum}] (at 'data manifest))
        (manifest-uri (at 'uri manifest))
        (data-list:list (map (lambda (entry) (if (= (at 'uri entry) uri) new-datum entry))datal))
        (upgraded-manifest (create-manifest manifest-uri data-list))
      )
        (enforce-verify-manifest upgraded-manifest)
        upgraded-manifest
    )
  )

  (defun upgrade-manifest-remove-datum-at-uri (manifest:object{manifest} obj)
    (let*
      (
        (uri (at 'uri obj))
        (data-list:[object{mf-datum}] (at 'data manifest))
        (manifest-uri (at 'uri manifest))
        (filtered-data-list:list (filter (where 'uri (!= uri)) data-list))
        (upgraded-manifest (create-manifest manifest-uri filtered-data-list))
      )
        (enforce-verify-manifest upgraded-manifest)
        upgraded-manifest
    )
  )

  (defun upgrade-manifest-add-datum (manifest:object{manifest} new-datum:object{mf-datum})
    (let*
      (
        (data-list:[object{mf-datum}] (at 'data manifest))
        (manifest-uri (at 'uri manifest))
        (added-data-list:list (+ [new-datum] data-list))
        (upgraded-manifest (create-manifest manifest-uri added-data-list))
      )
        (enforce-verify-manifest upgraded-manifest)
        upgraded-manifest
    )
  )

  (defun upgrade-manifest-replace-uri (manifest:object{manifest} obj)
    (let*
      (
        (new-uri:object{mf-uri} (at 'uri obj))
        (data-list:[object{mf-datum}] (at 'data manifest))
        (upgraded-manifest (create-manifest new-uri data-list))
      )
        (enforce-verify-manifest upgraded-manifest)
        upgraded-manifest
    )
  )

  (defun get-datum-with-uri-from-list (datum-list:[object{mf-datum}] uri:object{mf-uri})
    (filter (where 'uri (!= uri)) datum-list)
  )
)
