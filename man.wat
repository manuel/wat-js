;; -*- mode: scheme -*-

(define-module man (document section para render)

  (define *current-parent* (dnew null))
  
  (define-prototype Document
    (id
     title
     children))
  
  (define-prototype Section
    (id
     parent
     title
     children))
  
  (define-prototype Para
    (text
     parent))
  
  (define (make-document (id String) (title String))
    (new Document id title (array)))
  
  (define-macro (document id title)
    (list define id (list make-document (symbol-name id) title)))
  
  (define (make-section id parent title)
    (let ((sec (new Section id parent title (array))))
      (#push (.children parent) sec)
      sec))

  (define-macro (section (parent id) title . children)
    (list begin
      (list define id (list make-section (symbol-name id) parent title))
        (list* dlet (list (list *current-parent* id))
               children)))
  
  (define (para text)
    (let* ((parent (dref *current-parent*))
           (para (new Para text (the Section parent))))
      (#push (.children parent) para)
      para))

  (define-generic (render item))
  
  (define-method (render (doc Document))
    (apply cat (list* (cat "<h1>" (.title doc) "</h1>\n")
                      (map-list render (array-to-list (.children doc))))))
  
  (define-method (render (sec Section))
    (apply cat (list* (cat "<h2>" (.title sec) "</h2>\n")
                      (map-list render (array-to-list (.children sec))))))
  
  (define-method (render (para Para))
    (cat "<p>" (.text para) "</p>\n"))
  
  )
