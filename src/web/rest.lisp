(in-package :wf/web)

(snooze:defroute target (:get :application/json &key id url)
  (json:encode-json-to-string
   (hu:collecting-hash-table ()
     (let ((url (or url (get-rooturl-by-id id)))
           (id (or id (get-rooturl-id url))))
       (unless (and url id)
         (snooze:http-condition 404 "Can't find that target"))
       (hu:collect :url url)
       (hu:collect :id id)
       (hu:collect :title (grab-title url))
       (hu:collect :text (grab-text url))
       (hu:collect :warstats (warstats-for-target url))))))
