;; -*- mode: scheme -*-

(defmodule man (document section para render)

  (def *current-parent* (dnew #null))
  
  (defprototype Document Object
    (id
     title
     children))
  
  (defprototype Section Object
    (id
     parent
     title
     children))
  
  (defprototype Para Object
    (text
     parent))
  
  (def (make-document (id String) (title String))
    (new Document id title (array)))
  
  (defmacro (document id title)
    (list def id (list make-document (symbol-name id) title)))
  
  (def (make-section id parent title)
    (let ((sec (new Section id parent title (array))))
      (~push (.children parent) sec)
      sec))

  (defmacro (section (parent id) title . children)
    (list begin
      (list def id (list make-section (symbol-name id) parent title))
        (list* dlet (list (list *current-parent* id))
               children)))
  
  (def (para text)
    (let* ((parent (dref *current-parent*))
           (para (new Para text (the Section parent))))
      (~push (.children parent) para)
      para))

  (defgeneric (render item))
  
  (defmethod (render (doc Document))
    (apply + (list* (+ "<h1>" (.title doc) "</h1>\n")
                    (map-list render (array-to-list (.children doc))))))
  
  (defmethod (render (sec Section))
    (apply + (list* (+ "<h2>" (.title sec) "</h2>\n")
                    (map-list render (array-to-list (.children sec))))))
  
  (defmethod (render (para Para))
    (+ "<p>" (.text para) "</p>\n"))
  
  )
