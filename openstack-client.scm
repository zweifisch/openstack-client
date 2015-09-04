
(module openstack-client *

  (import scheme chicken)

  (use http-client intarweb uri-common medea clojurian-syntax matchable data-structures srfi-13 srfi-1)

  (define (login url name password #!key (domain-name "default"))
    (let* ((uri (uri-reference (conc url "/auth/tokens")))
           (payload `((auth . ((identity . ((methods . #("password"))
                                            (password . ((user . ((domain . ((name . ,domain-name)))
                                                                  (name . ,name)
                                                                  (password . ,password)))))))))))
           (req (make-request method: 'POST
                              uri: uri
                              headers: (headers '((content-type application/json))))))
      (call-with-input-request*
       req (json->string payload)
       (lambda (port response)
         (->> (read-json port) (assoc 'token) cdr
              (cons `(token . ,(->> (response-headers response)
                                    (header-value 'x-subject-token)))))))))


  (define (get-endpoint token service #!optional (interface "public"))
    (->> token
         (assoc 'catalog) cdr vector->list
         (find (lambda (endpoint)
                 (string= service (cdr (assoc 'type endpoint)))))
         (assoc 'endpoints) cdr vector->list
         (find (lambda (endpoint)
                 (string= interface
                          (cdr (assoc 'interface endpoint)))))
         (assoc 'url) cdr))

  (define (get-services token)
    (->> token
         (assoc 'catalog) cdr
         (map (lambda (endpoint) (->> (assoc 'type endpoint) cdr)))))


  (define (request token service path #!key (method 'GET) query body)
    (let* ((uri (uri-reference (conc (get-endpoint token service "public") path)))
           (req (make-request method: method
                              uri: (if query (update-uri uri query: query) uri)
                              headers: (headers `((content-type application/json)
                                                  (x-auth-token ,(->> token (assoc 'token) cdr)))))))
      (with-input-from-request
       req (if body (json->string body) #f) read-json)))

  (define-syntax define-resource
    (ir-macro-transformer
     (lambda (form inject compare)
       (let* ((name (inject (second form)))
              (service (third form))
              (path (fourth form))
              (singular (if (> (length form) 4) (fifth form) (symbol->string name)))
              (plural (if (> (length form) 5) (sixth form) (sprintf "~As" singular)))
              (list-res (symbol-append name '-list))
              (create-res (symbol-append name '-create))
              (get-res (symbol-append name '-get)))
         `(begin
            (define (,list-res token #!optional query)
              (->>
               (request token ,service ,path query: query)
               (alist-ref (string->symbol ,plural))
               vector->list))
            (define (,get-res token id #!optional query)
              (as->
               (request token ,service (conc ,path "/" id) query: query) x
               (alist-ref (string->symbol ,singular) x eqv? x)))
            (define (,create-res token body)
              (as->
               (request token ,service ,path method: 'POST body: (list (cons (string->symbol ,singular) body))) x
               (alist-ref (string->symbol ,singular) x eqv? x))))))))

  (define-resource service "identity" "/services")
  (define-resource endpoint "identity" "/endpoints")
  (define-resource project "identity" "/projects")
  (define-resource domain "identity" "/domains")
  (define-resource user "identity" "/users")

  (define-resource server "compute" "/servers")
  (define-resource server-detail "compute" "/servers/detail" "server")
  (define-resource flavor "compute" "/flavors")
  (define-resource flavor-detail "compute" "/flavors/detail")
  (define-resource keypair "compute" "/os-keypairs")
  (define-resource aggregate "compute" "/os-aggregates")
  (define-resource host "compute" "/os-hosts")

  (define-resource network "network" "/v2.0/networks")
  (define-resource port "network" "/v2.0/ports")
  (define-resource router "network" "/v2.0/routers")
  (define-resource subnet "network" "/v2.0/subnets")

  (define-resource image "image" "/v2/images"))
