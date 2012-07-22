;; -*- mode: scheme -*-
;; This is the hard crust of Wat code around the JS core defined in `wat.js`.

(def Boolean (mktag))
(def boolean-tagger (tagger Boolean))
(def #t (boolean-tagger #void))
(def #f (boolean-tagger #void))

