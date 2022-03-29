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
#'untar(xzfile(system.file("sqlite", "cdm.tar.xz", package = "Eunomia"), open = "rb"),
#'        exdir =  tempdir())
#'db <- dbConnect(RSQLite::SQLite(), paste0(tempdir(),"\\cdm.sqlite"))
#'vocabulary_database_schema<-"main"
#'get_candidate_codes(keywords="asthma",
#'                    db=db,
#'                    vocabulary_schema = "main")
#'asthma_codes<-get_candidate_codes(keywords="asthma",
#'                    search.synonyms=TRUE,
#'                              fuzzy.match=TRUE,
#'                              exclude=NULL,
#'                              include.descendants=TRUE,
#'                              include.ancestor=FALSE,
#'                    db=db,
#'                    vocabulary_database_schema = "main")
#' show_mappings(candidate_codelist= asthma_codes,
#'                    db=db,
#'                    vocabulary_database_schema = "main")

show_mappings<-function(candidate_codelist,
                        source_vocabularies=c("ATC","ICD10CM","ICD10PCS" ,
                                              "ICD9CM",  "ICD9Proc" ,
                                              "LOINC","OPCS4","Read",
                                              "RxNorm" ,"RxNorm Extension",
                                              "SNOMED"),
                              db,
                              vocabulary_database_schema){

 errorMessage <- checkmate::makeAssertCollection()

checkmate::assertVector(source_vocabularies, add = errorMessage)
checkmate::assertDataFrame(candidate_codelist, add = errorMessage)
# checkmate::assertDataFrame(concept, add = errorMessage)
# checkmate::assertDataFrame(concept_relationship, add = errorMessage)

checkmate::reportAssertions(collection = errorMessage)

concept_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                        vocabulary_database_schema,
                                        ".concept")))
concept_relationship_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                        vocabulary_database_schema,
                                        ".concept_relationship")))
concept<-concept_db %>%
  # filter(concept_id %in% !!candidate_codelist$concept_id) %>%
  collect()
maps_from<-concept_relationship_db %>%
  dplyr::filter(.data$relationship_id=="Mapped from") %>%
  filter(concept_id_1 %in% !!candidate_codelist$concept_id) %>%
  collect()

mapped.codes<- candidate_codelist %>%
    dplyr::select("concept_id", "concept_name") %>%
    dplyr::rename("Standard concept name"="concept_name") %>%
    dplyr::rename("concept_id_1"="concept_id") %>%
    dplyr::left_join(maps_from, by = "concept_id_1") %>%
    dplyr::select("concept_id_1","Standard concept name", "concept_id_2") %>%
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
