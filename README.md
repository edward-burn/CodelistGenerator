
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CodelistGenerator

The goal of CodelistGenerator is to create a candidate set of codes for
generating a phenotype for the OMOP CDM.

## Installation

You can install the development version of CodelistGenerator like so:

``` r
devtools::install_github("edward-burn/CodelistGenerator")
```

## Example

# Note, Eunomia, used in the example below, does not include a full set of vocabularies. The full set can be downloaded from <https://athena.ohdsi.org>.

``` r
library(CodelistGenerator)
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.1.2
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(Eunomia)
#> Loading required package: DatabaseConnector
library(stringr)
library(readr)
#> Warning: package 'readr' was built under R version 4.1.2
connection <- connect(getEunomiaConnectionDetails())
#> Connecting using SQLite driver
concepts<-querySql(connection, "SELECT * FROM concept;")
concept_ancestors<-querySql(connection, "SELECT * FROM concept_ancestor;")
concept_synonyms<-querySql(connection, "SELECT * FROM concept_synonym;")
disconnect(connection)
names(concepts)<-str_to_lower(names(concepts))
names(concept_ancestors)<-str_to_lower(names(concept_ancestors))
names(concept_synonyms)<-str_to_lower(names(concept_synonyms))
get_candidate_codes(keywords="asthma",
                 concept=concepts,
                    concept_ancestor = concept_ancestors,
                    concept_synonym = concept_synonyms)
#> [1] "Getting concepts to include from exact matches"
#> [1] "Getting concepts to include from descendants of identified concepts"
#> [1] "-- Getting batch 1 of 1"
#> [1] "Getting candidate codelist took 0 minutes and 0 seconds"
#>   concept_id     concept_name domain_id vocabulary_id
#> 1    4051466 Childhood asthma Condition        SNOMED
#> 2     317009           Asthma Condition        SNOMED
#> 3    4062501 Asthma screening Procedure        SNOMED
```
