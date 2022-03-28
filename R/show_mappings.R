#' Show mappings from source vocabularies to standard
#'
#' @param candidate_codelist Dataframe
#' @param source_vocabularies Character vector
#' @param concept Dataframe
#' @param concept_relationship Dataframe
#'
#' @return Dataframe
#' @export
#'
#' @examples
#' # note, Eunomia, which is used for the example below, does not include a full set of vocabularies. The full set can be downloaded from https://athena.ohdsi.org
#' library(dplyr)
#' library(Eunomia)
#' library(stringr)
#' library(readr)
#' connection <- connect(getEunomiaConnectionDetails())
#' concepts<-querySql(connection, "SELECT * FROM concept;")
#' concept_relationships<-querySql(connection, "SELECT * FROM CONCEPT_RELATIONSHIP;")
#' concept_ancestors<-querySql(connection, "SELECT * FROM concept_ancestor;")
#' concept_synonyms<-querySql(connection, "SELECT * FROM concept_synonym;")
#' disconnect(connection)
#' names(concepts)<-str_to_lower(names(concepts))
#' names(concept_relationships)<-str_to_lower(names(concept_relationships))
#' names(concept_ancestors)<-str_to_lower(names(concept_ancestors))
#' names(concept_synonyms)<-str_to_lower(names(concept_synonyms))
#' asthma_codes<- get_candidate_codes(keywords="asthma",
#'                  concept=concepts,
#'                     concept_ancestor = concept_ancestors,
#'                     concept_synonym = concept_synonyms)
#' show_mappings(candidate_codelist= asthma_codes,
#'            concept = concepts,
#'            concept_relationship =  concept_relationships)

show_mappings<-function(candidate_codelist,
                        source_vocabularies=c("ATC","ICD10CM","ICD10PCS" ,
                                              "ICD9CM",  "ICD9Proc" ,
                                              "LOINC","OPCS4","Read",
                                              "RxNorm" ,"RxNorm Extension",
                                              "SNOMED"),
                        concept,
                        concept_relationship){

 errorMessage <- checkmate::makeAssertCollection()

checkmate::assertVector(source_vocabularies, add = errorMessage)
checkmate::assertDataFrame(candidate_codelist, add = errorMessage)
checkmate::assertDataFrame(concept, add = errorMessage)
checkmate::assertDataFrame(concept_relationship, add = errorMessage)

checkmate::reportAssertions(collection = errorMessage)

maps_from<-concept_relationship %>%
  dplyr::filter(.data$relationship_id=="Mapped from")

mapped.codes<- candidate_codelist %>%
    dplyr::select("concept_id", "concept_name") %>%
    dplyr::rename("Standard concept name"="concept_name") %>%
    dplyr::rename("concept_id_1"="concept_id") %>%
    dplyr::left_join(maps_from, by = "concept_id_1") %>%
    dplyr::select("concept_id_1","Standard concept name", "concept_id_2")%>%
    dplyr::rename("concept_id"="concept_id_2")  %>%
    dplyr::left_join(concept %>%
                dplyr::select("concept_id", "concept_name", "vocabulary_id", "concept_code")) %>%
    dplyr::filter(.data$vocabulary_id %in% source_vocabularies) %>%
  dplyr::arrange(.data$concept_id_1) %>%
  dplyr::distinct()

mapped.codes<-mapped.codes %>%
  dplyr::rename("Standed concept_id (mapped to)"="concept_id_1") %>%
  dplyr::rename("Source concept_id (mapped from)"="concept_id") %>%
  dplyr::rename("Source code"="concept_code") %>%
  dplyr::rename("Source name"="concept_name")

mapped.codes

}
