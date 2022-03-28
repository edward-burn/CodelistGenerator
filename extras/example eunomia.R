library(dplyr)
library(Eunomia)
library(stringr)
library(readr)
connection <- connect(getEunomiaConnectionDetails())
concepts<-querySql(connection, "SELECT * FROM concept;")
concept_relationships<-querySql(connection, "SELECT * FROM CONCEPT_RELATIONSHIP;")
maps_froms<-querySql(connection, "SELECT * FROM CONCEPT_RELATIONSHIP;") %>%
 filter(RELATIONSHIP_ID=="Mapped from")
concept_ancestors<-querySql(connection, "SELECT * FROM concept_ancestor;")
concept_synonyms<-querySql(connection, "SELECT * FROM concept_synonym;")
disconnect(connection)
names(concepts)<-str_to_lower(names(concepts))
names(concept_relationships)<-str_to_lower(names(concept_relationships))
names(concept_ancestors)<-str_to_lower(names(concept_ancestors))
names(concept_synonyms)<-str_to_lower(names(concept_synonyms))

# vocab.folder<-"E:/CdmVocab2" # directory of unzipped files
# concept<-read_delim(paste0(vocab.folder,"/CONCEPT.csv"),
#      "\t", escape_double = FALSE, trim_ws = TRUE)
# maps_from<-read_delim(paste0(vocab.folder,"/CONCEPT_RELATIONSHIP.csv"),
#      "\t", escape_double = FALSE, trim_ws = TRUE) %>%
#   filter(relationship_id=="Mapped from")
# concept_ancestor<-read_delim(paste0(vocab.folder,"/CONCEPT_ANCESTOR.csv"),
#      "\t", escape_double = FALSE, trim_ws = TRUE)
# concept_synonym<-read_delim(paste0(vocab.folder,"/CONCEPT_SYNONYM.csv"),
#      "\t", escape_double = FALSE, trim_ws = TRUE)

# supported_domains<-c("Condition")
# concept<-concept %>%
#   filter(domain_id %in% supported_domains)
# concept_ancestor<-concept_ancestor %>%
#   left_join(concept %>%
#               select("concept_id", "domain_id") %>%
#               rename("ancestor_concept_id"="concept_id")) %>%
#   filter(domain_id %in% supported_domains) %>%
#   select(-domain_id)
# concept_ancestor<-concept_ancestor %>%
#   left_join(concept %>%
#               select("concept_id", "domain_id") %>%
#               rename("descendant_concept_id"="concept_id")) %>%
#   filter(domain_id %in% supported_domains) %>%
#   select(-domain_id)
# concept_synonym<-concept_synonym %>%
#   left_join(concept %>%
#               select("concept_id", "domain_id")) %>%
#   filter(domain_id %in% supported_domains) %>%
#   select(-domain_id)
# maps_from<-maps_from %>%
#   left_join(concept %>%
#               select("concept_id", "domain_id") %>%
#               rename("concept_id_1"="concept_id")) %>%
#   filter(domain_id %in% supported_domains) %>%
#   select(-domain_id)



###
a<-get_candidate_codes(keywords="kidney stone",
                    concept=concepts,
                    concept_ancestor = concept_ancestors,
                    concept_synonym = concept_synonyms)
get_candidate_codes(keywords="asthma",
                    concept=concepts,
                    concept_ancestor = concept_ancestors,
                    concept_synonym = concept_synonyms,
                    concept_relationship = concept_relationships)
get_candidate_codes(keywords="asthma",
                    search.synonyms=TRUE,
                              fuzzy.match=TRUE,
                              exclude=NULL,
                              include.descendants=TRUE,
                              include.ancestor=FALSE,
                    concept=concepts,
                    concept_ancestor = concept_ancestors,
                    concept_synonym = concept_synonyms,
                    concept_relationship = concept_relationships)
#
