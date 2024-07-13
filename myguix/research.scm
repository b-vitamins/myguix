(define-module (myguix research)
  #:use-module (guix records)
  #:export (
						;; affiliation
						affiliation
						affiliation?
						this-affiliation
						affiliation-institution
						affiliation-years

						;; author-ids
						author-ids
						author-ids?
						this-author-ids
						author-ids-openalex
						author-ids-orcid
						author-ids-scopus
						author-ids-twitter
						author-ids-wikipedia

                   ;; count-by-year
					count-by-year
					count-by-year?
					this-count-by-year
					count-by-year-year
					count-by-year-works-count
					count-by-year-cited-by-count

					;; summary-stats
					summary-stats
					summary-stats?
					this-summary-stats
					summary-stats-two-year-mean-citedness
					summary-stats-h-index
					summary-stats-i10-index

					;; dehydrated-institution
					dehydrated-institution
					dehydrated-institution?
					this-dehydrated-institution
					dehydrated-institution-id
					dehydrated-institution-ror
					dehydrated-institution-display-name
					dehydrated-institution-country-code
					dehydrated-institution-institution-type
					dehydrated-institution-lineage

					;; dehydrated-concept
					dehydrated-concept
					dehydrated-concept?
					this-dehydrated-concept
					dehydrated-concept-id
					dehydrated-concept-wikidata
					dehydrated-concept-display-name
					dehydrated-concept-level
					dehydrated-concept-score

					;; authorship
					authorship
					authorship?
					this-authorship
					authorship-author
					authorship-author-position
					authorship-countries
					authorship-institutions
					authorship-is-corresponding
					authorship-raw-affiliation-string
					authorship-raw-affiliation-strings
					authorship-raw-author-name

					;; apc
					apc
					apc?
					this-apc
					apc-value
					apc-currency
					apc-provenance
					apc-value-usd

					;; biblio
					biblio
					biblio?
					this-biblio
					biblio-volume
					biblio-issue
					biblio-first-page
					biblio-last-page

					;; concept
					concept
					concept?
					this-concept
					concept-id
					concept-wikidata
					concept-display-name
					concept-level
					concept-score

					;; grant
					grant
					grant?
					this-grant
					grant-funder
					grant-funder-display-name
					grant-award-id

					;; year-count
					year-count
					year-count?
					this-year-count
					year-count-year
					year-count-cited-by-count

                   ;; percentile-year
					percentile-year
					percentile-year?
					this-percentile-year
					percentile-year-max
					percentile-year-min

					;; keyword
					keyword
					keyword?
					this-keyword
					keyword-keyword
					keyword-score

					;; mesh-tag
					mesh-tag
					mesh-tag?
					this-mesh-tag
					mesh-tag-descriptor-ui
					mesh-tag-descriptor-name
					mesh-tag-qualifier-ui
					mesh-tag-qualifier-name
					mesh-tag-is-major-topic

					;; work-ids
					work-ids
					work-ids?
					this-work-ids
					work-ids-openalex
					work-ids-doi
					work-ids-mag
					work-ids-pmid
					work-ids-pmcid

					;; open-access
					open-access
					open-access?
					this-open-access
					open-access-is-oa
					open-access-oa-status
					open-access-oa-url
					open-access-any-repository-has-fulltext

					;; sdg
					sdg
					sdg?
					this-sdg
					sdg-id
					sdg-display-name
					sdg-score

					;; topic
					topic
					topic?
					this-topic
					topic-display-name
					topic-domain
					topic-field
					topic-id
					topic-score
					topic-subfield

					;; source
					source
					source?
					this-source
					source-display-name
					source-host-organization
					source-host-organization-lineage
					source-host-organization-lineage-names
					source-host-organization-name
					source-id
					source-is-in-doaj
					source-is-oa
					source-issn
					source-issn-l
					source-type

					;; publisher-ids
					publisher-ids
					publisher-ids?
					this-publisher-ids
					publisher-ids-openalex
					publisher-ids-ror
					publisher-ids-wikidata

					;; role
					role
					role?
					this-role
					role-role
					role-id
					role-works-count

					;; counts-by-year
					counts-by-year
					counts-by-year?
					this-counts-by-year
					counts-by-year-year
					counts-by-year-works-count
					counts-by-year-cited-by-count

					;; funder-ids
					funder-ids
					funder-ids?
					this-funder-ids
					funder-ids-crossref
					funder-ids-doi
					funder-ids-openalex
					funder-ids-ror
					funder-ids-wikidata

					;; apc-price
					apc-price
					apc-price?
					this-apc-price
					apc-price-price
					apc-price-currency

					;; source-ids
					source-ids
					source-ids?
					this-source-ids
					source-ids-fatcat
					source-ids-issn
					source-ids-issn-l
					source-ids-mag
					source-ids-openalex
					source-ids-wikidata

					;; society
					society
					society?
					this-society
					society-url
					society-organization

					;; concept-ids
					concept-ids
					concept-ids?
					this-concept-ids
					concept-ids-mag
					concept-ids-openalex
					concept-ids-umls-cui
					concept-ids-umls-aui
					concept-ids-wikidata
					concept-ids-wikipedia

                   ;; international-display-names
					international-display-names
					international-display-names?
					this-international-display-names
					international-display-names-display-name

					;; concept-summary-stats
					concept-summary-stats
					concept-summary-stats?
					this-concept-summary-stats
					concept-summary-stats-two-year-mean-citedness
					concept-summary-stats-h-index
					concept-summary-stats-i10-index

					;; dehydrated-institution-with-relationship
					dehydrated-institution-with-relationship
					dehydrated-institution-with-relationship?
					this-dehydrated-institution-with-relationship
					dehydrated-institution-with-relationship-id
					dehydrated-institution-with-relationship-ror
					dehydrated-institution-with-relationship-display-name
					dehydrated-institution-with-relationship-country-code
					dehydrated-institution-with-relationship-institution-type
					dehydrated-institution-with-relationship-relationship

					;; geo
					geo
					geo?
					this-geo
					geo-city
					geo-geonames-city-id
					geo-region
					geo-country-code
					geo-country
					geo-latitude
					geo-longitude

					;; institution-ids
					institution-ids
					institution-ids?
					this-institution-ids
					institution-ids-grid
					institution-ids-mag
					institution-ids-openalex
					institution-ids-ror
					institution-ids-wikipedia
					institution-ids-wikidata

					;; repository
					repository
					repository?
					this-repository
					repository-id
					repository-display-name
					repository-host-organization
					repository-host-organization-name
					repository-host-organization-lineage

					;; domain
					domain
					domain?
					this-domain
					domain-id
					domain-display-name

					;; field
					field
					field?
					this-field
					field-id
					field-display-name

					;; subfield
					subfield
					subfield?
					this-subfield
					subfield-id
					subfield-display-name

					;; topic-ids
					topic-ids
					topic-ids?
					this-topic-ids
					topic-ids-openalex
					topic-ids-wikipedia

					;; author
					author
					author?
					this-author
					author-affiliations
					author-cited-by-count
					author-counts-by-year
					author-created-date
					author-display-name
					author-display-name-alternatives
					author-id
					author-ids
					author-last-known-institution
					author-last-known-institutions
					author-orcid
					author-summary-stats
					author-updated-date
					author-works-api-url
					author-works-count
					author-x-concepts

                   ;; work
					work
					work?
					this-work
					work-abstract-inverted-index
					work-authorships
					work-apc-list
					work-apc-paid
					work-best-oa-location
					work-biblio
					work-cited-by-api-url
					work-cited-by-count
					work-cited-by-percentile-year
					work-concepts
					work-corresponding-author-ids
					work-corresponding-institution-ids
					work-countries-distinct-count
					work-counts-by-year
					work-created-date
					work-display-name
					work-doi
					work-fulltext-origin
					work-grants
					work-has-fulltext
					work-id
					work-ids
					work-indexed-in
					work-institutions-distinct-count
					work-is-paratext
					work-is-retracted
					work-keywords
					work-language
					work-locations
					work-locations-count
					work-mesh
					work-ngrams-url
					work-open-access
					work-primary-location
					work-primary-topic
					work-publication-date
					work-publication-year
					work-referenced-works
					work-referenced-works-count
					work-related-works
					work-sustainable-development-goals
					work-title
					work-topics
					work-work-type
					work-type-crossref
					work-updated-date
					work-versions

					;; institution
					institution
					institution?
					this-institution
					institution-associated-institutions
					institution-cited-by-count
					institution-country-code
					institution-counts-by-year
					institution-created-date
					institution-display-name
					institution-display-name-acronyms
					institution-display-name-alternatives
					institution-geo
					institution-homepage-url
					institution-id
					institution-ids
					institution-image-thumbnail-url
					institution-image-url
					institution-international
					institution-lineage
					institution-repositories
					institution-roles
					institution-ror
					institution-summary-stats
					institution-institution-type
					institution-institution-type-id
					institution-updated-date
					institution-works-api-url
					institution-works-count
					institution-x-concepts

					;; funder
					funder
					funder?
					this-funder
					funder-alternate-titles
					funder-cited-by-count
					funder-country-code
					funder-counts-by-year
					funder-created-date
					funder-description
					funder-display-name
					funder-grants-count
					funder-homepage-url
					funder-id
					funder-ids
					funder-image-thumbnail-url
					funder-image-url
					funder-roles
					funder-summary-stats
					funder-updated-date
					funder-works-count

                   ;; publisher
					publisher
					publisher?
					this-publisher
					publisher-alternate-titles
					publisher-cited-by-count
					publisher-country-codes
					publisher-counts-by-year
					publisher-created-date
					publisher-display-name
					publisher-hierarchy-level
					publisher-homepage-url
					publisher-id
					publisher-ids
					publisher-image-thumbnail-url
					publisher-image-url
					publisher-lineage
					publisher-parent-publisher
					publisher-roles
					publisher-sources-api-url
					publisher-summary-stats
					publisher-updated-date
					publisher-works-count

					;; source
					source
					source?
					this-source
					source-abbreviated-title
					source-alternate-titles
					source-apc-prices
					source-apc-usd
					source-cited-by-count
					source-country-code
					source-counts-by-year
					source-created-date
					source-display-name
					source-homepage-url
					source-host-organization
					source-host-organization-lineage
					source-host-organization-name
					source-id
					source-ids
					source-is-in-doaj
					source-is-oa
					source-issn
					source-issn-l
					source-societies
					source-summary-stats
					source-source-type
					source-updated-date
					source-works-api-url
					source-works-count
					source-x-concepts

					;; topic
					topic
					topic?
					this-topic
					topic-cited-by-count
					topic-created-date
					topic-description
					topic-display-name
					topic-domain
					topic-field
					topic-id
					topic-ids
					topic-keywords
					topic-siblings
					topic-subfield
					topic-updated-date
					topic-works-count
					))

;; A record type for affiliation.
(define-record-type* <affiliation> %affiliation make-affiliation
  affiliation?
  this-affiliation
  (institution affiliation-institution
               (default #f)) ;<dehydrated-institution>
  (years affiliation-years
         (default '()))) ;list of integers

;; A record for various author identifiers.
(define-record-type* <author-ids> %author-ids make-author-ids
  author-ids?
  this-author-ids
  (openalex author-ids-openalex
            (default #f)) ;string
  (orcid author-ids-orcid
         (default #f)) ;string
  (scopus author-ids-scopus
          (default #f)) ;string
  (twitter author-ids-twitter
           (default #f)) ;string
  (wikipedia author-ids-wikipedia
             (default #f))) ;string

;; A record holding counts by year.
(define-record-type* <count-by-year> %count-by-year make-count-by-year
  count-by-year?
  this-count-by-year
  (year count-by-year-year
        (default #f)) ;integer
  (works-count count-by-year-works-count
               (default #f)) ;integer
  (cited-by-count count-by-year-cited-by-count
                  (default #f))) ;integer

;; A record for summary statistics.
(define-record-type* <summary-stats> %summary-stats make-summary-stats
  summary-stats?
  this-summary-stats
  (two-year-mean-citedness summary-stats-two-year-mean-citedness
                           (default #f)) ;float
  (h-index summary-stats-h-index
           (default #f)) ;integer
  (i10-index summary-stats-i10-index
             (default #f))) ;integer

;; A record for basic institution data in a dehydrated form.
(define-record-type* <dehydrated-institution> %dehydrated-institution
                     make-dehydrated-institution
  dehydrated-institution?
  this-dehydrated-institution
  (id dehydrated-institution-id
      (default #f)) ;string
  (ror dehydrated-institution-ror
       (default #f)) ;string
  (display-name dehydrated-institution-display-name
                (default #f)) ;string
  (country-code dehydrated-institution-country-code
                (default #f)) ;string
  (institution-type dehydrated-institution-institution-type
                    (default #f)) ;string
  (lineage dehydrated-institution-lineage
           (default '()))) ;list of strings

;; A record for dehydrated concept data.
(define-record-type* <dehydrated-concept> %dehydrated-concept
                     make-dehydrated-concept
  dehydrated-concept?
  this-dehydrated-concept
  (id dehydrated-concept-id
      (default #f)) ;string
  (wikidata dehydrated-concept-wikidata
            (default #f)) ;string
  (display-name dehydrated-concept-display-name
                (default #f)) ;string
  (level dehydrated-concept-level
         (default #f)) ;integer
  (score dehydrated-concept-score
         (default #f))) ;float

;; A record for authorship.
(define-record-type* <authorship> %authorship make-authorship
  authorship?
  this-authorship
  (author authorship-author
          (default #f)) ;<author-for-authorship>
  (author-position authorship-author-position
                   (default #f)) ;string
  (countries authorship-countries
             (default '())) ;list of strings
  (institutions authorship-institutions
                (default '())) ;list of <institution-for-authorship>
  (is-corresponding authorship-is-corresponding
                    (default #f)) ;boolean
  (raw-affiliation-string authorship-raw-affiliation-string
                          (default #f)) ;string
  (raw-affiliation-strings authorship-raw-affiliation-strings
                           (default '())) ;list of strings
  (raw-author-name authorship-raw-author-name
                   (default #f))) ;string

;; A record for article processing charges.
(define-record-type* <apc> %apc make-apc
  apc?
  this-apc
  (value apc-value
         (default #f)) ;integer
  (currency apc-currency
            (default #f)) ;string
  (provenance apc-provenance
              (default #f)) ;string
  (value-usd apc-value-usd
             (default #f))) ;integer

;; A bibliographic record holding details of a publication.
(define-record-type* <biblio> %biblio make-biblio
  biblio?
  this-biblio
  (volume biblio-volume
          (default #f)) ;string
  (issue biblio-issue
         (default #f)) ;string
  (first-page biblio-first-page
              (default #f)) ;string
  (last-page biblio-last-page
             (default #f))) ;string

;; A record for basic concept data.
(define-record-type* <concept> concept make-concept
  concept?
  this-concept
  (id concept-id
      (default #f)) ;string
  (wikidata concept-wikidata
            (default #f)) ;string
  (display-name concept-display-name
                (default #f)) ;string
  (level concept-level
         (default #f)) ;integer
  (score concept-score
         (default #f))) ;float

;; A record for grant information.
(define-record-type* <grant> grant make-grant
  grant?
  this-grant
  (funder grant-funder
          (default #f)) ;string
  (funder-display-name grant-funder-display-name
                       (default #f)) ;string
  (award-id grant-award-id
            (default #f))) ;string

;; A record holding counts by year.
(define-record-type* <year-count> year-count make-year-count
  year-count?
  this-year-count
  (year year-count-year
        (default #f)) ;integer
  (cited-by-count year-count-cited-by-count
                  (default #f))) ;integer

;; A record for percentile year data.
(define-record-type* <percentile-year> percentile-year make-percentile-year
  percentile-year?
  this-percentile-year
  (max percentile-year-max
       (default #f)) ;integer
  (min percentile-year-min
       (default #f))) ;integer

;; A record for keywords.
(define-record-type* <keyword> keyword make-keyword
  keyword?
  this-keyword
  (keyword keyword-keyword
           (default #f)) ;string
  (score keyword-score
         (default #f))) ;float

;; A record for mesh tags.
(define-record-type* <mesh-tag> mesh-tag make-mesh-tag
  mesh-tag?
  this-mesh-tag
  (descriptor-ui mesh-tag-descriptor-ui
                 (default #f)) ;string
  (descriptor-name mesh-tag-descriptor-name
                   (default #f)) ;string
  (qualifier-ui mesh-tag-qualifier-ui
                (default #f)) ;string
  (qualifier-name mesh-tag-qualifier-name
                  (default #f)) ;string
  (is-major-topic mesh-tag-is-major-topic
                  (default #f))) ;boolean

;; A record for work identifiers.
(define-record-type* <work-ids> work-ids make-work-ids
  work-ids?
  this-work-ids
  (openalex work-ids-openalex
            (default #f)) ;string
  (doi work-ids-doi
       (default #f)) ;string
  (mag work-ids-mag
       (default #f)) ;string
  (pmid work-ids-pmid
        (default #f)) ;string
  (pmcid work-ids-pmcid
         (default #f))) ;string

;; A record for open access information.
(define-record-type* <open-access> open-access make-open-access
  open-access?
  this-open-access
  (is-oa open-access-is-oa
         (default #f)) ;boolean
  (oa-status open-access-oa-status
             (default #f)) ;string
  (oa-url open-access-oa-url
          (default #f)) ;string
  (any-repository-has-fulltext open-access-any-repository-has-fulltext
                               (default #f))) ;boolean

;; A record for sustainable development goals.
(define-record-type* <sdg> sdg make-sdg
  sdg?
  this-sdg
  (id sdg-id
      (default #f)) ;string
  (display-name sdg-display-name
                (default #f)) ;string
  (score sdg-score
         (default #f))) ;float

;; A record for topics.
(define-record-type* <topic> topic make-topic
  topic?
  this-topic
  (display-name topic-display-name
                (default #f)) ;string
  (domain topic-domain
          (default #f)) ;string
  (field topic-field
         (default #f)) ;string
  (id topic-id
      (default #f)) ;string
  (score topic-score
         (default #f)) ;float
  (subfield topic-subfield
            (default #f))) ;string

;; A record for sources.
(define-record-type* <source> source make-source
  source?
  this-source
  (display-name source-display-name
                (default #f)) ;string
  (host-organization source-host-organization
                     (default #f)) ;string
  (host-organization-lineage source-host-organization-lineage
                             (default '())) ;list of strings
  (host-organization-lineage-names source-host-organization-lineage-names
                                   (default '())) ;list of strings
  (host-organization-name source-host-organization-name
                          (default #f)) ;string
  (id source-id
      (default #f)) ;string
  (is-in-doaj source-is-in-doaj
              (default #f)) ;boolean
  (is-oa source-is-oa
         (default #f)) ;boolean
  (issn source-issn
        (default '())) ;list of strings
  (issn-l source-issn-l
          (default #f)) ;string
  (type source-type
        (default #f))) ;string

;; A record for publisher identifiers.
(define-record-type* <publisher-ids> publisher-ids make-publisher-ids
  publisher-ids?
  this-publisher-ids
  (openalex publisher-ids-openalex
            (default #f)) ;string
  (ror publisher-ids-ror
       (default #f)) ;string
  (wikidata publisher-ids-wikidata
            (default #f))) ;string

;; A record for roles.
(define-record-type* <role> role make-role
  role?
  this-role
  (role role-role
        (default #f)) ;string
  (id role-id
      (default #f)) ;string
  (works-count role-works-count
               (default #f))) ;integer

;; A record for counts by year.
(define-record-type* <counts-by-year> counts-by-year make-counts-by-year
  counts-by-year?
  this-counts-by-year
  (year counts-by-year-year
        (default #f)) ;integer
  (works-count counts-by-year-works-count
               (default #f)) ;integer
  (cited-by-count counts-by-year-cited-by-count
                  (default #f))) ;integer

;; A record for funder identifiers.
(define-record-type* <funder-ids> funder-ids make-funder-ids
  funder-ids?
  this-funder-ids
  (crossref funder-ids-crossref
            (default #f)) ;string
  (doi funder-ids-doi
       (default #f)) ;string
  (openalex funder-ids-openalex
            (default #f)) ;string
  (ror funder-ids-ror
       (default #f)) ;string
  (wikidata funder-ids-wikidata
            (default #f))) ;string

;; A record for APC prices.
(define-record-type* <apc-price> apc-price make-apc-price
  apc-price?
  this-apc-price
  (price apc-price-price
         (default #f)) ;integer
  (currency apc-price-currency
            (default #f))) ;string

;; A record for source identifiers.
(define-record-type* <source-ids> source-ids make-source-ids
  source-ids?
  this-source-ids
  (fatcat source-ids-fatcat
          (default #f)) ;string
  (issn source-ids-issn
        (default '())) ;list of strings
  (issn-l source-ids-issn-l
          (default #f)) ;string
  (mag source-ids-mag
       (default #f)) ;string
  (openalex source-ids-openalex
            (default #f)) ;string
  (wikidata source-ids-wikidata
            (default #f))) ;string

;; A record for societies.
(define-record-type* <society> society make-society
  society?
  this-society
  (url society-url
       (default #f)) ;string
  (organization society-organization
                (default #f))) ;string

;; A record for concept identifiers.
(define-record-type* <concept-ids> concept-ids make-concept-ids
  concept-ids?
  this-concept-ids
  (mag concept-ids-mag
       (default #f)) ;integer
  (openalex concept-ids-openalex
            (default #f)) ;string
  (umls-cui concept-ids-umls-cui
            (default '())) ;list of strings
  (umls-aui concept-ids-umls-aui
            (default '())) ;list of strings
  (wikidata concept-ids-wikidata
            (default #f)) ;string
  (wikipedia concept-ids-wikipedia
             (default #f))) ;string

;; A record for international display names.
(define-record-type* <international-display-names> international-display-names
                     make-international-display-names
  international-display-names?
  this-international-display-names
  (display-name international-display-names-display-name
                (default '()))) ;alist of (string . string)

;; A record for concept summary statistics.
(define-record-type* <concept-summary-stats> concept-summary-stats
                     make-concept-summary-stats
  concept-summary-stats?
  this-concept-summary-stats
  (two-year-mean-citedness concept-summary-stats-two-year-mean-citedness
                           (default #f)) ;float
  (h-index concept-summary-stats-h-index
           (default #f)) ;integer
  (i10-index concept-summary-stats-i10-index
             (default #f))) ;integer

;; A record for dehydrated institution with relationship.
(define-record-type* <dehydrated-institution-with-relationship>
                     dehydrated-institution-with-relationship
                     make-dehydrated-institution-with-relationship
  dehydrated-institution-with-relationship?
  this-dehydrated-institution-with-relationship
  (id dehydrated-institution-with-relationship-id
      (default #f)) ;string
  (ror dehydrated-institution-with-relationship-ror
       (default #f)) ;string
  (display-name dehydrated-institution-with-relationship-display-name
                (default #f)) ;string
  (country-code dehydrated-institution-with-relationship-country-code
                (default #f)) ;string
  (institution-type dehydrated-institution-with-relationship-institution-type
                    (default #f)) ;string
  (relationship dehydrated-institution-with-relationship-relationship
                (default #f))) ;string

;; A record for geographical information.
(define-record-type* <geo> geo make-geo
  geo?
  this-geo
  (city geo-city
        (default #f)) ;string
  (geonames-city-id geo-geonames-city-id
                    (default #f)) ;string
  (region geo-region
          (default #f)) ;string
  (country-code geo-country-code
                (default #f)) ;string
  (country geo-country
           (default #f)) ;string
  (latitude geo-latitude
            (default #f)) ;float
  (longitude geo-longitude
             (default #f))) ;float

;; A record for institution identifiers.
(define-record-type* <institution-ids> institution-ids make-institution-ids
  institution-ids?
  this-institution-ids
  (grid institution-ids-grid
        (default #f)) ;string
  (mag institution-ids-mag
       (default #f)) ;string
  (openalex institution-ids-openalex
            (default #f)) ;string
  (ror institution-ids-ror
       (default #f)) ;string
  (wikipedia institution-ids-wikipedia
             (default #f)) ;string
  (wikidata institution-ids-wikidata
            (default #f))) ;string

;; A record for repository information.
(define-record-type* <repository> repository make-repository
  repository?
  this-repository
  (id repository-id
      (default #f)) ;string
  (display-name repository-display-name
                (default #f)) ;string
  (host-organization repository-host-organization
                     (default #f)) ;string
  (host-organization-name repository-host-organization-name
                          (default #f)) ;string
  (host-organization-lineage repository-host-organization-lineage
                             (default '()))) ;list of strings

;; A record for domain information.
(define-record-type* <domain> domain make-domain
  domain?
  this-domain
  (id domain-id
      (default #f)) ;string
  (display-name domain-display-name
                (default #f))) ;string

;; A record for field information.
(define-record-type* <field> field make-field
  field?
  this-field
  (id field-id
      (default #f)) ;string
  (display-name field-display-name
                (default #f))) ;string

;; A record for subfield information.
(define-record-type* <subfield> subfield make-subfield
  subfield?
  this-subfield
  (id subfield-id
      (default #f)) ;string
  (display-name subfield-display-name
                (default #f))) ;string

;; A record for topic identifiers.
(define-record-type* <topic-ids> topic-ids make-topic-ids
  topic-ids?
  this-topic-ids
  (openalex topic-ids-openalex
            (default #f)) ;string
  (wikipedia topic-ids-wikipedia
             (default #f))) ;string

;; A record type for author.
(define-record-type* <author> %author make-author
  author?
  this-author
  (affiliations author-affiliations
                (default '())) ;list of <affiliation> | #f
  (cited-by-count author-cited-by-count
                  (default #f)) ;integer
  (counts-by-year author-counts-by-year
                  (default '())) ;list of <count-by-year> | #f
  (created-date author-created-date
                (default #f)) ;string
  (display-name author-display-name
                (default #f)) ;string
  (display-name-alternatives author-display-name-alternatives
                             (default '())) ;list of strings or #f
  (id author-id
      (default #f)) ;string
  (ids author-ids
       (default #f)) ;<author-ids> | #f
  (last-known-institution author-last-known-institution
                          (default #f)) ;<dehydrated-institution> | #f
  (last-known-institutions author-last-known-institutions
                           (default '())) ;list of <dehydrated-institution> | #f
  (orcid author-orcid
         (default #f)) ;string
  (summary-stats author-summary-stats
                 (default #f)) ;<summary-stats> | #f
  (updated-date author-updated-date
                (default #f)) ;string
  (works-api-url author-works-api-url
                 (default #f)) ;string
  (works-count author-works-count
               (default #f)) ;integer
  (x-concepts author-x-concepts
              (default '()))) ;list of <dehydrated-concept> | #f

;; A record type for a work.
(define-record-type* <work> %work make-work
  work?
  this-work
  (abstract-inverted-index work-abstract-inverted-index
                           (default '())) ;alist of (string . list of integers)
  (authorships work-authorships
               (default '())) ;list of <authorship> | #f
  (apc-list work-apc-list
            (default #f)) ;<apc> | #f
  (apc-paid work-apc-paid
            (default #f)) ;<apc> | #f
  (best-oa-location work-best-oa-location
                    (default #f)) ;<location> | #f
  (biblio work-biblio
          (default #f)) ;<biblio> | #f
  (cited-by-api-url work-cited-by-api-url
                    (default #f)) ;string
  (cited-by-count work-cited-by-count
                  (default #f)) ;integer
  (cited-by-percentile-year work-cited-by-percentile-year
                            (default #f)) ;<percentile-year> | #f
  (concepts work-concepts
            (default '())) ;list of <concept> | #f
  (corresponding-author-ids work-corresponding-author-ids
                            (default '())) ;list of strings | #f
  (corresponding-institution-ids work-corresponding-institution-ids
                                 (default '())) ;list of strings | #f
  (countries-distinct-count work-countries-distinct-count
                            (default #f)) ;integer
  (counts-by-year work-counts-by-year
                  (default '())) ;list of <year-count> | #f
  (created-date work-created-date
                (default #f)) ;string
  (display-name work-display-name
                (default #f)) ;string
  (doi work-doi
       (default #f)) ;string
  (fulltext-origin work-fulltext-origin
                   (default #f)) ;string
  (grants work-grants
          (default '())) ;list of <grant> | #f
  (has-fulltext work-has-fulltext
                (default #f)) ;boolean
  (id work-id
      (default #f)) ;string
  (ids work-ids
       (default #f)) ;<work-ids> | #f
  (indexed-in work-indexed-in
              (default '())) ;list of strings | #f
  (institutions-distinct-count work-institutions-distinct-count
                               (default #f)) ;integer
  (is-paratext work-is-paratext
               (default #f)) ;boolean
  (is-retracted work-is-retracted
                (default #f)) ;boolean
  (keywords work-keywords
            (default '())) ;list of <keyword> | #f
  (language work-language
            (default #f)) ;string
  (locations work-locations
             (default '())) ;list of <location> | #f
  (locations-count work-locations-count
                   (default #f)) ;integer
  (mesh work-mesh
        (default '())) ;list of <mesh-tag> | #f
  (ngrams-url work-ngrams-url
              (default #f)) ;string
  (open-access work-open-access
               (default #f)) ;<open-access> | #f
  (primary-location work-primary-location
                    (default #f)) ;<location> | #f
  (primary-topic work-primary-topic
                 (default #f)) ;<topic> | #f
  (publication-date work-publication-date
                    (default #f)) ;string
  (publication-year work-publication-year
                    (default #f)) ;integer
  (referenced-works work-referenced-works
                    (default '())) ;list of strings | #f
  (referenced-works-count work-referenced-works-count
                          (default #f)) ;integer
  (related-works work-related-works
                 (default '())) ;list of strings | #f
  (sustainable-development-goals work-sustainable-development-goals
                                 (default '())) ;list of <sdg> | #f
  (title work-title
         (default #f)) ;string
  (topics work-topics
          (default '())) ;list of <topic> | #f
  (work-type work-work-type
             (default #f)) ;string
  (type-crossref work-type-crossref
                 (default #f)) ;string
  (updated-date work-updated-date
                (default #f)) ;string
  (versions work-versions
            (default '()))) ;list of strings | #f

;; A record type for an institution.
(define-record-type* <institution> institution make-institution
  institution?
  this-institution
  (associated-institutions institution-associated-institutions
                           (default '())) ;list of <dehydrated-institution-with-relationship> | #f
  (cited-by-count institution-cited-by-count
                  (default #f)) ;integer
  (country-code institution-country-code
                (default #f)) ;string
  (counts-by-year institution-counts-by-year
                  (default '())) ;list of <counts-by-year> | #f
  (created-date institution-created-date
                (default #f)) ;string
  (display-name institution-display-name
                (default #f)) ;string
  (display-name-acronyms institution-display-name-acronyms
                         (default '())) ;list of strings | #f
  (display-name-alternatives institution-display-name-alternatives
                             (default '())) ;list of strings | #f
  (geo institution-geo
       (default #f)) ;<geo> | #f
  (homepage-url institution-homepage-url
                (default #f)) ;string
  (id institution-id
      (default #f)) ;string
  (ids institution-ids
       (default #f)) ;<institution-ids> | #f
  (image-thumbnail-url institution-image-thumbnail-url
                       (default #f)) ;string
  (image-url institution-image-url
             (default #f)) ;string
  (international institution-international
                 (default #f)) ;<international-display-names> | #f
  (lineage institution-lineage
           (default '())) ;list of strings | #f
  (repositories institution-repositories
                (default '())) ;list of <repository> | #f
  (roles institution-roles
         (default '())) ;list of <role> | #f
  (ror institution-ror
       (default #f)) ;string
  (summary-stats institution-summary-stats
                 (default #f)) ;<summary-stats> | #f
  (institution-type institution-institution-type
                    (default #f)) ;string
  (institution-type-id institution-institution-type-id
                       (default #f)) ;string
  (updated-date institution-updated-date
                (default #f)) ;string
  (works-api-url institution-works-api-url
                 (default #f)) ;string
  (works-count institution-works-count
               (default #f)) ;integer
  (x-concepts institution-x-concepts
              (default '()))) ;list of <dehydrated-concept> | #f

;; A record type for a funder.
(define-record-type* <funder> funder make-funder
  funder?
  this-funder
  (alternate-titles funder-alternate-titles
                    (default '())) ;list of strings | #f
  (cited-by-count funder-cited-by-count
                  (default #f)) ;integer
  (country-code funder-country-code
                (default #f)) ;string
  (counts-by-year funder-counts-by-year
                  (default '())) ;list of <counts-by-year> | #f
  (created-date funder-created-date
                (default #f)) ;string
  (description funder-description
               (default #f)) ;string
  (display-name funder-display-name
                (default #f)) ;string
  (grants-count funder-grants-count
                (default #f)) ;integer
  (homepage-url funder-homepage-url
                (default #f)) ;string
  (id funder-id
      (default #f)) ;string
  (ids funder-ids
       (default #f)) ;<funder-ids> | #f
  (image-thumbnail-url funder-image-thumbnail-url
                       (default #f)) ;string
  (image-url funder-image-url
             (default #f)) ;string
  (roles funder-roles
         (default '())) ;list of <role> | #f
  (summary-stats funder-summary-stats
                 (default #f)) ;<summary-stats> | #f
  (updated-date funder-updated-date
                (default #f)) ;string
  (works-count funder-works-count
               (default #f))) ;integer

;; A record type for a publisher.
(define-record-type* <publisher> publisher make-publisher
  publisher?
  this-publisher
  (alternate-titles publisher-alternate-titles
                    (default '())) ;list of strings | #f
  (cited-by-count publisher-cited-by-count
                  (default #f)) ;integer
  (country-codes publisher-country-codes
                 (default '())) ;list of strings | #f
  (counts-by-year publisher-counts-by-year
                  (default '())) ;list of <count-by-year> | #f
  (created-date publisher-created-date
                (default #f)) ;string
  (display-name publisher-display-name
                (default #f)) ;string
  (hierarchy-level publisher-hierarchy-level
                   (default #f)) ;integer
  (homepage-url publisher-homepage-url
                (default #f)) ;string
  (id publisher-id
      (default #f)) ;string
  (ids publisher-ids
       (default #f)) ;<publisher-ids> | #f
  (image-thumbnail-url publisher-image-thumbnail-url
                       (default #f)) ;string
  (image-url publisher-image-url
             (default #f)) ;string
  (lineage publisher-lineage
           (default '())) ;list of strings | #f
  (parent-publisher publisher-parent-publisher
                    (default #f)) ;string
  (roles publisher-roles
         (default '())) ;list of <role> | #f
  (sources-api-url publisher-sources-api-url
                   (default #f)) ;string
  (summary-stats publisher-summary-stats
                 (default #f)) ;<summary-stats> | #f
  (updated-date publisher-updated-date
                (default #f)) ;string
  (works-count publisher-works-count
               (default #f))) ;integer

;; A record type for a source.
(define-record-type* <source> source make-source
  source?
  this-source
  (abbreviated-title source-abbreviated-title
                     (default #f)) ;string
  (alternate-titles source-alternate-titles
                    (default '())) ;list of strings | #f
  (apc-prices source-apc-prices
              (default '())) ;list of <apc-price> | #f
  (apc-usd source-apc-usd
           (default #f)) ;integer
  (cited-by-count source-cited-by-count
                  (default #f)) ;integer
  (country-code source-country-code
                (default #f)) ;string
  (counts-by-year source-counts-by-year
                  (default '())) ;list of <counts-by-year> | #f
  (created-date source-created-date
                (default #f)) ;string
  (display-name source-display-name
                (default #f)) ;string
  (homepage-url source-homepage-url
                (default #f)) ;string
  (host-organization source-host-organization
                     (default #f)) ;string
  (host-organization-lineage source-host-organization-lineage
                             (default '())) ;list of strings | #f
  (host-organization-name source-host-organization-name
                          (default #f)) ;string
  (id source-id
      (default #f)) ;string
  (ids source-ids
       (default #f)) ;<source-ids> | #f
  (is-in-doaj source-is-in-doaj
              (default #f)) ;boolean
  (is-oa source-is-oa
         (default #f)) ;boolean
  (issn source-issn
        (default '())) ;list of strings | #f
  (issn-l source-issn-l
          (default #f)) ;string
  (societies source-societies
             (default '())) ;list of <society> | #f
  (summary-stats source-summary-stats
                 (default #f)) ;<summary-stats> | #f
  (source-type source-source-type
               (default #f)) ;string
  (updated-date source-updated-date
                (default #f)) ;string
  (works-api-url source-works-api-url
                 (default #f)) ;string
  (works-count source-works-count
               (default #f)) ;integer
  (x-concepts source-x-concepts
              (default '()))) ;list of <dehydrated-concept> | #f

;; A record type for a topic.
(define-record-type* <topic> topic make-topic
  topic?
  this-topic
  (cited-by-count topic-cited-by-count
                  (default #f)) ;integer
  (created-date topic-created-date
                (default #f)) ;string
  (description topic-description
               (default #f)) ;string
  (display-name topic-display-name
                (default #f)) ;string
  (domain topic-domain
          (default #f)) ;<domain> | #f
  (field topic-field
         (default #f)) ;<field> | #f
  (id topic-id
      (default #f)) ;string
  (ids topic-ids
       (default #f)) ;<topic-ids> | #f
  (keywords topic-keywords
            (default '())) ;list of strings | #f
  (siblings topic-siblings
            (default '())) ;list of strings | #f
  (subfield topic-subfield
            (default #f)) ;<subfield> | #f
  (updated-date topic-updated-date
                (default #f)) ;string
  (works-count topic-works-count
               (default #f))) ;integer
