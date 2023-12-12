



library(tidyverse)
library(TNRS)
library(kewr)
library(APCalign)
library(taxize)
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
 
  # Taxize can't handle massive lists
  if(length(species_in) > 100){
    species_in_ls <- split(species_in, ceiling(seq_along(species_in)/100))
    
    taxize_output <- purrr::map_dfr(species_in_ls, 
                                ~resolve(.x, db = "gnr"))
  } else
  taxize_output <- resolve(species_in, db = "gnr")
  
  taxize_tidy <- taxize_output$gnr |>  
    select(user_supplied_name, matched_name, score) |> 
    rename(original_name = user_supplied_name, 
           taxize = matched_name) |> 
    distinct() |> 
    group_by(original_name) |> 
    slice_max(order_by = score, with_ties = FALSE) |> 
    select(-score)
  
  compare <- tibble(original_name = species_in) %>%
    left_join(apcout) %>%
    left_join(kew_only_one) %>%
    left_join(tnrs_ss) |> 
    left_join(taxize_tidy)

return(compare)
}


a<-read_csv("data/comparison_datasets/taxon_names_Angevin_2011.csv")
ange_compare<-do_comparison(a$taxon_name)
write_csv(ange_compare,"angevin_comparison.csv")


b<-read_csv("data/comparison_datasets/taxon_names_Kooyman_2011.csv")
kooy_compare<-do_comparison(b$taxon_name)
write_csv(kooy_compare,"kooy_comparison.csv")

c<-read_csv("data/comparison_datasets/test_matches_alignments_updates.csv")
alignments_compare <-do_comparison(c$original_name)
write_csv(alignments_compare,"alignments_compare.csv")

d<-read_csv("data/comparison_datasets/test_splits_synonyms.csv")
synonyms_compare <-do_comparison(d$original_name)
write_csv(alignments_compare,"synonyms_compare.csv")
