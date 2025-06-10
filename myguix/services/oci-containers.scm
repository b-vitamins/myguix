(define-module (myguix services oci-containers)
  #:use-module (gnu)
  #:use-module (gnu services docker)
  #:use-module (ice-9 rdelim)
  #:use-module (rnrs files)
  #:use-module (rnrs io simple)
  #:use-module (srfi srfi-13)
  #:export (read-secret oci-airflow-service-type
                        oci-cassandra-service-type
                        oci-clickhouse-service-type
                        oci-embeddings-service-type
                        oci-grafana-service-type
                        oci-grobid-service-type
                        oci-janusgraph-service-type
                        oci-meilisearch-service-type
                        oci-minio-service-type
                        oci-neo4j-service-type
                        oci-ollama-service-type
                        oci-prometheus-service-type
                        oci-qdrant-service-type
                        oci-solr-service-type
                        oci-weaviate-service-type))

;; Common function to read secrets from files
(define (read-secret path)
  (if (file-exists? path)
      (call-with-input-file path
        (lambda (p)
          (string-trim-both (read-line p)))) ""))

;; Define all credentials in one place
(define grafana-admin-password
  (read-secret "/root/grafana-admin.credentials"))
(define meili-master-key
  (read-secret "/root/meili.credentials"))
(define minio-secret
  (read-secret "/root/minio.credentials"))
(define neo4j-password
  (read-secret "/root/neo4j.credentials"))
(define qdrant-api-key
  (read-secret "/root/qdrant.credentials"))

;; Define common environment configurations
(define cassandra-env
  (list '("CASSANDRA_CLUSTER_NAME" . "NeurIPSCluster")
        '("CASSANDRA_SEEDS" . "127.0.0.1")
        '("CASSANDRA_START_RPC" . "true")
        '("CASSANDRA_RPC_ADDRESS" . "0.0.0.0")
        '("CASSANDRA_BROADCAST_RPC_ADDRESS" . "127.0.0.1")
        '("MAX_HEAP_SIZE" . "1G")
        '("HEAP_NEWSIZE" . "256M")))

(define janusgraph-env
  (list '("JAVA_OPTIONS" . "-Xms8g -Xmx8g -XX:+UseG1GC -XX:MaxGCPauseMillis=200")
        '("STORAGE_BACKEND" . "cql")
        '("STORAGE_HOSTNAME" . "localhost:9042")
        '("STORAGE_CASSANDRA_KEYSPACE" . "janusgraph")
        '("INDEX_BACKEND" . "solr")
        '("INDEX_HOSTNAME" . "localhost:8983")))

(define neo4j-env
  (list `("NEO4J_AUTH" unquote
          (string-append "neo4j/" neo4j-password))
        '("NEO4J_ACCEPT_LICENSE_AGREEMENT" . "yes")
        '("NEO4J_gds_arrow_enabled" . "true")

        ;; Memory
        '("NEO4J_server_memory_heap_initial__size" . "8G")
        '("NEO4J_server_memory_heap_max__size" . "16G")
        '("NEO4J_server_memory_pagecache_size" . "12G")

        ;; APOC import / export
        '("NEO4J_apoc_import_file_enabled" . "true")
        '("NEO4J_apoc_export_file_enabled" . "true")
        '("NEO4J_apoc_import_file_use__neo4j__config" . "true")

        ;; Unrestricted procedures
        '("NEO4J_dbms_security_procedures_unrestricted" . "gds.*,apoc.*")

        ;; Optional – pull plugins automatically
        '("NEO4J_PLUGINS" . "[\"apoc\",\"graph-data-science\"]")))

(define weaviate-modules
  (string-append "text2vec-cohere,"
                 "text2vec-huggingface,"
                 "text2vec-palm,"
                 "text2vec-openai,"
                 "generative-openai,"
                 "generative-cohere,"
                 "generative-palm,"
                 "ref2vec-centroid,"
                 "reranker-cohere,"
                 "qna-openai"))

(define weaviate-env
  (list '("QUERY_DEFAULTS_LIMIT" . "25")
        '("AUTHENTICATION_ANONYMOUS_ACCESS_ENABLED" . "true")
        '("PERSISTENCE_DATA_PATH" . "/var/lib/weaviate")
        '("DEFAULT_VECTORIZER_MODULE" . "none")
        `("ENABLE_MODULES" unquote weaviate-modules)))

;; Apache Cassandra - NoSQL distributed database
(define oci-cassandra-service-type
  (oci-container-configuration (auto-start? #t)
                               (image "cassandra:latest")
                               (network "host")
                               (ports '(("7000" . "7000") ("7001" . "7001")
                                        ("7199" . "7199")
                                        ("9042" . "9042")
                                        ("9160" . "9160")))
                               (volumes (list '("/var/lib/cassandra/data" . "/var/lib/cassandra/data")))
                               (environment cassandra-env)))

;; ClickHouse - column-oriented SQL database for OLAP
(define oci-clickhouse-service-type
  (oci-container-configuration (auto-start? #t)
                               (image "clickhouse:latest")
                               (network "host")
                               (ports '(("8123" . "8123") ("9000" . "9000")))
                               (extra-arguments (list "--ulimit"
                                                      "nofile=262144:262144"))
                               (volumes (list '("/var/lib/clickhouse/data" . "/var/lib/clickhouse")
                                              '("/var/log/clickhouse-server" . "/var/log/clickhouse-server")))))

;; Embeddings + BM25 sparse inference
(define oci-embeddings-service-type
  (oci-container-configuration (auto-start? #t)
                               (image
                                "ghcr.io/huggingface/text-embeddings-inference:cpu-0.7.2")
                               (network "host")
                               (ports '(("8081" . "8080"))) ;host 8081 → container 8080
                               (environment (list '("MODEL_ID" . "allenai/scibert_scivocab_uncased")
                                                  '("NUM_THREADS" . "8")
                                                  '("DEVICE" . "cpu")))
                               (extra-arguments '("--memory=6g"))))

;; Grafana - analytics and monitoring
(define oci-grafana-service-type
  (oci-container-configuration (auto-start? #t)
                               (image "grafana/grafana-oss:11.0.0")
                               (network "host")
                               (ports '(("3000" . "3000")))
                               (volumes (list '("/var/lib/grafana" . "/var/lib/grafana")))
                               (environment (list (cons
                                                   "GF_SECURITY_ADMIN_PASSWORD"
                                                   grafana-admin-password)))))

;; GROBID - machine learning for scholarly document extraction
(define oci-grobid-service-type
  (oci-container-configuration (image "grobid/grobid:0.8.1")
                               (network "host")
                               (ports '(("8070" . "8070")))))

;; JanusGraph - scalable graph database
(define oci-janusgraph-service-type
  (oci-container-configuration (auto-start? #t)
                               (image "janusgraph/janusgraph:latest")
                               (network "host")
                               (ports '(("8182" . "8182") ("8183" . "8183")))
                               (volumes (list '("/var/lib/janusgraph/data" . "/janusgraph/data")))
                               (environment janusgraph-env)))

;; Meilisearch - open-source search engine
(define oci-meilisearch-service-type
  (oci-container-configuration (image "getmeili/meilisearch:latest")
                               (network "host")
                               (ports '(("7700" . "7700")))
                               (environment `(("MEILI_NO_ANALYTICS" . "true") ("MEILI_MASTER_KEY"
                                                                               unquote
                                                                               meili-master-key)))
                               (volumes (list '("/var/lib/meilisearch/meili_data" . "/meili_data")))))

;; MinIO - object storage
(define oci-minio-service-type
  (oci-container-configuration (auto-start? #t)
                               (image "minio/minio")
                               (network "host")
                               (ports '(("9000" . "9000") ("9001" . "9001"))) ;S3 + console
                               (volumes (list '("/var/lib/minio/data" . "/data")))
                               (environment (list '("MINIO_ROOT_USER" . "root")
                                                  (cons "MINIO_ROOT_PASSWORD"
                                                        minio-secret)
                                                  '("MINIO_BROWSER" . "on")))
                               (command '("server" "/data" "--console-address"
                                          ":9001"))))

;; Neo4j - graph database management system
(define oci-neo4j-service-type
  (oci-container-configuration (image "neo4j:5.26-community")
                               (network "host")
                               (ports '(("7474" . "7474") ;HTTP
                                        ("7687" . "7687") ;Bolt
                                        ("7688" . "7688"))) ;Flight
                               (volumes (list '("/var/lib/neo4j/data" . "/data")
                                              '("/var/lib/neo4j/logs" . "/logs")
                                              '("/var/lib/neo4j/import" . "/var/lib/neo4j/import")
                                              '("/var/lib/neo4j/plugins" . "/plugins")))
                               (extra-arguments '("--ulimit"
                                                  "nofile=262144:262144"))
                               (environment neo4j-env)))

;; Ollama - LLM inference
(define oci-ollama-service-type
  (oci-container-configuration (auto-start? #t)
                               (image "ollama/ollama:0.1.28")
                               (network "host")
                               (ports '(("11434" . "11434")))
                               (volumes (list '("/var/lib/ollama" . "/root/.ollama")))
                               (environment (list '("OLLAMA_MODELS" . "llama3")))
                               (command '("serve"))))

;; Prometheus - monitoring
(define oci-prometheus-service-type
  (oci-container-configuration (auto-start? #t)
                               (image "prom/prometheus:v2.52.0")
                               (network "host")
                               (ports '(("9090" . "9090")))
                               (volumes (list '("/var/lib/prometheus" . "/prometheus")
                                              '("/etc/prometheus/prometheus.yml" . "/etc/prometheus/prometheus.yml")))))

;; Qdrant - vector search engine
(define oci-qdrant-service-type
  (let* ((qdrant-cache "12G")
         (search-threads "10")
         (write-threads "2")
         (data-volume '("/var/lib/qdrant/data" . "/qdrant/storage"))
         (gpu-enabled? #t)
         (qdrant-base-env `(("QDRANT__STORAGE__CACHE_SIZE" unquote
                             qdrant-cache)
                            ("QDRANT__STORAGE__MMAP_THRESHOLD" . "128M")
                            ("QDRANT__SERVICE__MAX_SEARCH_THREADS" unquote
                             search-threads)
                            ("QDRANT__SERVICE__WRITE_THREADS" unquote
                             write-threads)
                            ("QDRANT__SERVICE__API_KEY" unquote qdrant-api-key)))
         (qdrant-gpu-env '(("CUDA_VISIBLE_DEVICES" . "0") ("QDRANT_GPU" . "1"))))
    (oci-container-configuration (auto-start? #t)
                                 (image "qdrant/qdrant:latest")
                                 (network "host")
                                 (ports '(("6333" . "6333") ;REST + gRPC
                                          ("6334" . "6334"))) ;Prometheus metrics
                                 (volumes (list data-volume))
                                 (extra-arguments '("--ulimit"
                                                    "nofile=500000:500000"
                                                    "--memory=14g"
                                                    "--memory-swap=14g"))
                                 (environment (append qdrant-base-env
                                                      (if gpu-enabled?
                                                          qdrant-gpu-env
                                                          '()))))))

;; Solr - search platform based on Lucene
(define oci-solr-service-type
  (oci-container-configuration (auto-start? #t)
                               (image "solr:latest")
                               (network "host")
                               (ports '(("8983" . "8983")))
                               (volumes (list '("/var/lib/solr/data" . "/var/solr")))
                               (command '("solr-precreate" "gettingstarted"))
                               (extra-arguments '("--ulimit"
                                                  "nofile=262144:262144"))
                               (environment (list '("SOLR_HEAP" . "800m")))))

;; Weaviate - vector search engine
(define oci-weaviate-service-type
  (oci-container-configuration (image
                                "cr.weaviate.io/semitechnologies/weaviate:1.28.4")
                               (network "host")
                               (ports '(("50051" . "50051")))
                               (environment weaviate-env)
                               (volumes (list '("/var/lib/weaviate/data" . "/var/lib/weaviate")))))
