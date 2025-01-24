(define-module (myguix services oci-containers)
  #:use-module (gnu)
  #:use-module (gnu services docker)
  #:use-module (ice-9 rdelim)
  #:use-module (rnrs files)
  #:use-module (rnrs io simple)
  #:export (get-meili-master-key meili-master-key
                                 oci-grobid-service-type
                                 oci-meilisearch-service-type
                                 oci-weaviate-service-type
                                 oci-neo4j-service-type
                                 oci-pytorch-service-type))

;; Function to read MEILI_MASTER_KEY from the credentials file
(define (get-meili-master-key)
  (if (file-exists? "/root/meili.credentials")
      (call-with-input-file "/root/meili.credentials"
        read-line) ""))
;; Return empty string if the file does not exist

;; Retrieve the MEILI_MASTER_KEY for injection
(define meili-master-key
  (get-meili-master-key))

;; Define an OCI container service for GROBID, a machine learning library for extracting
;; information from scholarly documents.
(define oci-grobid-service-type
  (oci-container-configuration (image "grobid/grobid:0.8.0")
                               (network "host")
                               (ports '(("8070" . "8070")))))

;; Define an OCI container service for Meilisearch, an open-source search engine.
(define oci-meilisearch-service-type
  (oci-container-configuration (image "getmeili/meilisearch:latest")
                               (network "host")
                               (ports '(("7700" . "7700")))
                               (environment (list '("MEILI_NO_ANALYTICS" . "true")
                                                  `("MEILI_MASTER_KEY" unquote
                                                    meili-master-key)))
                               (volumes (list '("/var/lib/meilisearch/meili_data" . "/meili_data")))))

;; Define an OCI container service for Weaviate, an open-source vector search engine.
(define oci-weaviate-service-type
  (oci-container-configuration (image
                                "cr.weaviate.io/semitechnologies/weaviate:1.24.10")
                               (network "host")
                               (ports '(("50051" . "50051")))
                               (environment (list '("QUERY_DEFAULTS_LIMIT" . "25")
                                                  '("AUTHENTICATION_ANONYMOUS_ACCESS_ENABLED" . "true")
                                                  '("PERSISTENCE_DATA_PATH" . "/var/lib/weaviate")
                                                  '("DEFAULT_VECTORIZER_MODULE" . "none")
                                                  `("ENABLE_MODULES" unquote
                                                    (string-append
                                                     "text2vec-cohere,"
                                                     "text2vec-huggingface,"
                                                     "text2vec-palm,"
                                                     "text2vec-openai,"
                                                     "generative-openai,"
                                                     "generative-cohere,"
                                                     "generative-palm,"
                                                     "ref2vec-centroid,"
                                                     "reranker-cohere,"
                                                     "qna-openai"))
                                                  '("CLUSTER_HOSTNAME" . "lagertha")))
                               (volumes (list '("/var/lib/weaviate/data" . "/var/lib/weaviate")))))

;; Define an OCI container service for Neo4j, a graph database management system.
(define oci-neo4j-service-type
  (oci-container-configuration (image "neo4j:latest")
                               (network "host")
                               (ports '(("7474" . "7474") ("7473" . "7473")
                                        ("7687" . "7687")))
                               (volumes (list '("/var/lib/neo4j/data" . "/data")
                                              '("/var/lib/neo4j/logs" . "/logs")
                                              '("/var/lib/neo4j/import" . "/var/lib/neo4j/import")
                                              '("/var/lib/neo4j/plugins" . "/plugins")))))

;; Define an OCI container service for PyTorch, a Python package that provides 1) Tensor computation (like NumPy) with strong GPU acceleration, 2) Deep neural networks built on a tape-based autograd system.
(define oci-pytorch-service-type
  (oci-container-configuration (auto-start? #t)
                               (image "pytorch/pytorch:latest")
                               (network "host")
                               (volumes '("/gnu/store:/gnu/store:ro"
                                          "/home/b/projects/nnml:/nnml"))
                               (extra-arguments (list "--gpus=all" "-w"
                                                      "/nnml"))
                               (environment (list '("NVIDIA_VISIBLE_DEVICES" . "all")
                                                  '("NVIDIA_DRIVER_CAPABILITIES" . "compute,utility")))))

;; Solr is the blazing-fast, open source, multi-modal search platform built on the full-text, vector, and geospatial search capabilities of Apache Lucene™.
(define oci-solr-service-type
  (oci-container-configuration (auto-start? #t)
                               (image "solr:latest")
                               (network "host")
                               (ports '(("8983" . "8983")))
                               (volumes '("/var/lib/solr/data:/var/solr"))
                               (command '("solr-precreate" "gettingstarted"))
                               (environment (list '("SOLR_HEAP" . "800m")))))

(define oci-cassandra-service-type
  (oci-container-configuration (auto-start? #t)
                               (image "cassandra:latest")
                               (network "host")
                               (ports '(("9042" . "9042") ("7000" . "7000")))
                               (extra-arguments '("--ulimit"
                                                  "nofile=262144:262144"))
                               (volumes '("/var/lib/cassandra/data:/var/lib/cassandra/data"))))
