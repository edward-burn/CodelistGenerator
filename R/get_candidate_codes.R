
#' Generate candidate codelist for the OMOP CDM
#'
#' @description
#' This function generates a set of codes that can be concidered for creating a phenotype
#' using the OMOP CDM.
#'
#' @param keywords Character vector of words to search for. Where more than one word is given (e.g. "knee osteoarthritis"), all words will be identified but can be in different positions (e.g. "osteoarthritis of knee") should be identified.
#' @param domains  Character vector with one or more of the OMOP CDM domain (e.g. "Condition").
#' @param search.synonyms Either TRUE or FALSE. If TRUE the code will also search via the concept synonym table.
#' @param fuzzy.match Either TRUE or FALSE. If TRUE the fuzzy matches will be used, with approximate matches identified.
#' @param fuzzy.match.max.distance The max.distance parmeter for fuzzy matching (see ??base::agrep for further details).
#' @param exclude  Character vector of words to search for to identify concepts to exclude.
#' @param include.descendants Either TRUE or FALSE. If TRUE descendant concepts of identified concepts will be included in the candidate codelist.
#' @param include.ancestor Either TRUE or FALSE. If TRUE the direct ancestor concepts of identified concepts will be included in the candidate codelist.
#' @param concept Dataframe with the OMOP CDM vocabulary concept table,
#' @param concept_synonym Dataframe with the OMOP CDM vocabulary concept_synonym table.
#' @param concept_ancestor Dataframe with the OMOP CDM vocabulary concept_ancestor table.
#'
#' @return Dataframe
#' @importFrom rlang .data
#' @export
#'
#' @examples
# # note, Eunomia, used in the example below, does not include a full set of vocabularies. The full set can be downloaded from https://athena.ohdsi.org
# library(dplyr)
# library(Eunomia)
# library(stringr)
# library(readr)
# connection <- connect(getEunomiaConnectionDetails())
# concepts<-querySql(connection, "SELECT * FROM concept;")
# concept_ancestors<-querySql(connection, "SELECT * FROM concept_ancestor;")
# concept_synonyms<-querySql(connection, "SELECT * FROM concept_synonym;")
# disconnect(connection)
# names(concepts)<-str_to_lower(names(concepts))
# names(concept_ancestors)<-str_to_lower(names(concept_ancestors))
# names(concept_synonyms)<-str_to_lower(names(concept_synonyms))
# get_candidate_codes(keywords="asthma",
#                  concept=concepts,
#                     concept_ancestor = concept_ancestors,
#                     concept_synonym = concept_synonyms)
#'
get_candidate_codes<-function(keywords,
                              domains=c("Condition", "Drug" ,"Device", "Observation",
                                        "Procedure"),
                              search.synonyms=FALSE,
                              fuzzy.match=FALSE,
                              fuzzy.match.max.distance=0.1,
                              exclude=NULL,
                              include.descendants=TRUE,
                              include.ancestor=FALSE,
                              concept,
                              concept_synonym,
                              concept_ancestor){

errorMessage <- checkmate::makeAssertCollection()

checkmate::assertVector(keywords, add = errorMessage)
checkmate::assertVector(exclude,null.ok = TRUE, add = errorMessage)

checkmate::assertDataFrame(concept, add = errorMessage)
checkmate::assertDataFrame(concept_synonym, add = errorMessage)
checkmate::assertDataFrame(concept_ancestor, add = errorMessage)

checkmate::reportAssertions(collection = errorMessage)

# concept<-dtplyr::lazy_dt(concept)
# concept_ancestor<-dtplyr::lazy_dt(concept_ancestor)
# concept_synonym<-dtplyr::lazy_dt(concept_synonym)

# filter to only relevant data
print(1)
concept<-concept %>%
  dplyr::filter(.data$domain_id %in% domains) %>%
  dplyr::filter(.data$standard_concept=="S")
print(2)
concept_ancestor<-dtplyr::lazy_dt(concept_ancestor) %>%
  dplyr::left_join(dtplyr::lazy_dt(concept   %>%
               dplyr::select("concept_id", "domain_id", "standard_concept") %>%
               dplyr::rename("ancestor_concept_id"="concept_id")),
              by="ancestor_concept_id")  %>%
   dplyr::as_tibble()%>%
  dplyr::filter(.data$domain_id %in% domains)%>%
  dplyr::filter(.data$standard_concept=="S")  %>%
  dplyr::select(-"domain_id") %>%
  dplyr::select(-"standard_concept")
print(3)
concept_ancestor<-dtplyr::lazy_dt(concept_ancestor) %>%
  dplyr::left_join(dtplyr::lazy_dt(concept %>%
              dplyr::select("concept_id", "domain_id", "standard_concept") %>%
              dplyr::rename("descendant_concept_id"="concept_id")),
              by="descendant_concept_id") %>%
   dplyr::as_tibble(.data)%>%
  dplyr::filter(.data$domain_id %in% domains)%>%
  dplyr::filter(.data$standard_concept=="S")  %>%
  dplyr::select(-"domain_id") %>%
  dplyr::select(-"standard_concept")
print(4)
concept_synonym<-dtplyr::lazy_dt(concept_synonym) %>%
  dplyr::left_join(dtplyr::lazy_dt(concept %>%
              dplyr::select("concept_id", "domain_id", "standard_concept")),
              by="concept_id") %>%
   dplyr::as_tibble(.data)%>%
  dplyr::filter(.data$domain_id %in% domains)%>%
  dplyr::filter(.data$standard_concept=="S")  %>%
  dplyr::select(-"domain_id") %>%
  dplyr::select(-"standard_concept")
print(5)
# concept<-as.data.frame(concept)
# concept_ancestor<-as.data.frame(concept_ancestor)
# concept_synonym<-as.data.frame(concept_synonym)


# 1) codes to exclude
# will anti_Join throughought to make sure these don't appear
# exact matches only

if(length(exclude)>0){
print("Getting concepts to exclude")
# Get standard, condition concepts which include one of the exclusion words
exclude<-clean_words(exclude)

exclude.codes<-lapply(seq_along(exclude), function(i) {
working.exclude<-unlist(strsplit(exclude[i]," "))
working.concepts<-concept %>%  # start with all
  dplyr::mutate(concept_name=clean_words(.data$concept_name))

working.concepts %>%
  dplyr::filter(apply(sapply(X = working.exclude,
       FUN = grepl, working.concepts$concept_name),
      MARGIN =  1, FUN = all)) %>%
  dplyr::distinct()
})
exclude.codes<-dplyr::bind_rows(exclude.codes)
}

# 2) Get standard, condition concepts which include one of the keywords
start<-Sys.time()
print("Getting concepts to include from exact matches")

keywords<-clean_words(keywords)

# because there may be a lot of synonyms, get these from a loop
# (stringr::str_detect slows considerably as more options are added in a single call using "|")
# where multiple words, split up and search (i.e. they don´t need to be next to each other)
candidate.codes.list<-list()
for(i in 1:length(keywords)){

working.keywords<-unlist(strsplit(keywords[i]," "))
working.concepts<-concept %>%  # start with all
  dplyr::mutate(concept_name=clean_words(.data$concept_name))

candidate.codes.list[[i]]<-working.concepts %>%
  dplyr::filter(apply(sapply(X = working.keywords,
       FUN = grepl, working.concepts$concept_name),
      MARGIN =  1, FUN = all))
}
candidate.codes<-dplyr::bind_rows(candidate.codes.list) %>%
  dplyr::distinct()

if(length(exclude)>0){
if(nrow(exclude.codes)>0){
candidate.codes<-candidate.codes %>%
  dplyr::anti_join(exclude.codes %>% dplyr::select("concept_id"),
                   by = "concept_id")
}}

# 2) use fuzzy match to include
if(fuzzy.match==TRUE){
  print("Getting concepts to include from fuzzy matches")

candidate.codes.fuzzy<-list()
for(i in 1:length(keywords)){
working.keywords<-unlist(strsplit(keywords[i]," "))
working.concepts<-concept %>%  # start with all
  dplyr::mutate(concept_name=clean_words(.data$concept_name))

for(j in 1:length(working.keywords)){ # dplyr::filter each term
indx<-agrep(working.keywords[j], working.concepts$concept_name, max.distance = fuzzy.match.max.distance)
working.concepts<-working.concepts[indx,]
}

candidate.codes.fuzzy[[i]]<-working.concepts
}
candidate.codes.fuzzy<-dplyr::bind_rows(candidate.codes.fuzzy) %>%
  dplyr::distinct()

candidate.codes<-dplyr::bind_rows(candidate.codes, candidate.codes.fuzzy) %>%
  dplyr::distinct()

if(length(exclude)>0){
if(nrow(exclude.codes)>0){
candidate.codes<-candidate.codes %>%
  dplyr::anti_join(exclude.codes %>% dplyr::select("concept_id"),
                   by = "concept_id")
}}

}

if(nrow(candidate.codes)==0){
candidate.codes
message("-- No codes found for given keywords")
} else {
# 4) look for any standard, condition concepts with a synonym of the
# codes found from the keywords
if(search.synonyms==TRUE){
print("Getting concepts to include from exact matches of synonyms")

synonyms<-concept_synonym %>%
  dplyr::filter(.data$concept_id %in% !!candidate.codes$concept_id) %>%
  dplyr::select("concept_synonym_name") %>%
  dplyr::distinct() %>%
  dplyr::pull()
synonyms<-unique(clean_words(synonyms))

working.concepts<-concept %>% # start with all
  dplyr::mutate(concept_name=clean_words(.data$concept_name))

# get these from a loop
synonym.codes.list<-lapply(seq_along(synonyms), function(i) {
working.synonyms<-unlist(strsplit(synonyms[i]," "))
synonym.codes.list<-working.concepts %>%
  dplyr::filter(apply(sapply(X = working.synonyms,
       FUN = grepl, working.concepts$concept_name),
      MARGIN =  1, FUN = all))
})
synonym.codes<-dplyr::bind_rows(synonym.codes.list) %>% dplyr::distinct()

candidate.codes<-dplyr::bind_rows(candidate.codes, synonym.codes) %>%
  dplyr::distinct()
rm(synonyms,synonym.codes,synonym.codes.list)

if(length(exclude)>0){
if(nrow(exclude.codes)>0){
candidate.codes<-candidate.codes %>%
  dplyr::anti_join(exclude.codes %>% dplyr::select("concept_id"),
                   by = "concept_id")
}}

}

# 5) add any codes lower in the hierachy (and deduplicate)
if(include.descendants==TRUE){
print("Getting concepts to include from descendants of identified concepts")

candidate.code.descendants <-  concept_ancestor  %>%
   dplyr::filter(.data$ancestor_concept_id  %in% !!candidate.codes$concept_id) %>%
   dplyr::select("descendant_concept_id")  %>%
   dplyr::rename(concept_id=.data$descendant_concept_id)

# to avoid potential memory problems will join in batches
n.batches<-100 # number in a batch
batched.codes<-split(candidate.code.descendants$concept_id,
                        ceiling(seq_along(candidate.code.descendants$concept_id)/
                                  n.batches))
candidate.code.descendants.batched<-list()
for(j in 1:length(batched.codes)){
# print(paste0("-- Getting batch ", j, " of ", length(batched.codes)))
using.codes.batch<-dplyr::tibble(concept_id=batched.codes[[j]])
candidate.code.descendants.batched[[j]] <- using.codes.batch %>%
   dplyr::left_join(concept, by = "concept_id" )
}
candidate.code.descendants<-dplyr::bind_rows(candidate.code.descendants.batched)


candidate.codes<-dplyr::bind_rows(candidate.codes, candidate.code.descendants) %>%
  dplyr::distinct()
rm(candidate.code.descendants)

if(length(exclude)>0){
if(nrow(exclude.codes)>0){
candidate.codes<-candidate.codes %>%
  dplyr::anti_join(exclude.codes %>% dplyr::select("concept_id"),
                   by = "concept_id")
}}

}

# 5) add any codes one level above in the hierachy (and deduplicate)
if(include.ancestor==TRUE){
print("Getting concepts to include from direct ancestors of identified concepts")

candidate.code.ancestor <-  concept_ancestor  %>%
   dplyr::filter(.data$descendant_concept_id  %in% !!candidate.codes$concept_id) %>%
   dplyr::filter(.data$min_levels_of_separation==1) %>%
   dplyr::select("ancestor_concept_id")  %>%
   dplyr::rename(concept_id=.data$ancestor_concept_id)

# only if not already in candidate.codes
candidate.code.ancestor<-candidate.code.ancestor %>%
  dplyr::anti_join(candidate.codes %>% dplyr::select(.data$concept_id),
             by = "concept_id")%>%
   dplyr::left_join(concept, by = "concept_id")

candidate.codes<-dplyr::bind_rows(candidate.codes, candidate.code.ancestor) %>%
  dplyr::distinct()
rm(candidate.code.ancestor)

if(length(exclude)>0){
if(nrow(exclude.codes)>0){
candidate.codes<-candidate.codes %>%
  dplyr::anti_join(exclude.codes %>% dplyr::select("concept_id"),
                   by = "concept_id")
}}

}


# get original names back
candidate.codes<-candidate.codes %>%
  dplyr::select(.data$concept_id) %>%
  dplyr::left_join(concept,
            by= c("concept_id")) %>%
  dplyr::distinct()

candidate.codes<- candidate.codes %>%
  dplyr::select("concept_id", "concept_name", "domain_id", "vocabulary_id")

x <- abs(as.numeric(Sys.time()-start, units="secs"))
print(paste0("Getting candidate codelist took ",
             floor(x/60), " minutes and ",  x %% 60 %/% 1, " seconds"))
candidate.codes %>%
  dplyr::distinct() # return
}

}


.datatable.aware <- TRUE
