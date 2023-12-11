



library(tidyverse)
library(TNRS)
library(kewr)
library(APCalign)
library(xlsx)
resources<-load_taxonomic_resources()







do_comparison <- function(species_in) {
  apcout <-
    create_taxonomic_update_lookup(species_in, resources = resources) %>%
    select(original_name, suggested_name)
  
  kewout <- match_knms(species_in)
  kewouttidy <- tidy(kewout)
  kewouttidy %>%
    group_by(submitted) %>%
    summarize(kew = matched_record[1], original_name = submitted[1]) %>%
    select(original_name, kew) -> kew_only_one
  
  TNRS_out <- TNRS(species_in, matches = "best")
  
  TNRS_out %>%
    select(original_name = Name_submitted, tnrs = Accepted_name) -> tnrs_ss
  
  compare <- tibble(original_name = species_in) %>%
    left_join(apcout) %>%
    left_join(kew_only_one) %>%
    left_join(tnrs_ss)

return(compare)
}


a<-read_csv("data/comparison_datasets/taxon_names_Angevin_2011.csv")
ange_compare<-do_comparison(a$taxon_name)
write_csv(ange_compare,"angevin_comparison.csv")


b<-read_csv("data/comparison_datasets/taxon_names_Kooyman_2011.csv")
kooy_compare<-do_comparison(b$taxon_name)
write_csv(kooy_compare,"kooy_comparison.csv")
