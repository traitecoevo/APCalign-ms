



library(tidyverse)
library(TNRS)
library(kewr)
library(APCalign)
library(taxize)
resources<-load_taxonomic_resources()







do_comparison <- function(species_in) {
  apcout <-
    create_taxonomic_update_lookup(species_in, resources = resources)
  
  apc_only_names <- apcout %>%
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
  
  all_output <- list(APCalign = apcout,
                     kewr = kewout,
                     TNRS = TNRS_out,
                     taxize = taxize_output$gnr)

return(list(comparison = compare, 
            output = all_output))
}


a<-read_csv("data/comparison_datasets/taxon_names_Angevin_2011.csv")
ange_compare<-do_comparison(a$taxon_name)
write_csv(ange_compare$comparisons,"angevin_comparison.csv")
saveRDS(ange_compare, "output/Angevin_tools_output.rds")


b<-read_csv("data/comparison_datasets/taxon_names_Kooyman_2011.csv")
kooy_compare<-do_comparison(b$taxon_name)
write_csv(kooy_compare$comparisons,"kooy_comparison.csv")
saveRDS(kooy_compare, "output/Kooyman_tools_output.rds")

c<-read_csv("data/comparison_datasets/test_matches_alignments_updates.csv")
alignments_compare <-do_comparison(c$original_name)
write_csv(alignments_compare$comparisons,"alignments_compare.csv")
saveRDS(alignments_compare, "output/test_alignments_tools_output.rds")

d<-read_csv("data/comparison_datasets/test_splits_synonyms.csv")
synonyms_compare <-do_comparison(d$original_name)
write_csv(alignments_compare$comparisons,"synonyms_compare.csv")
saveRDS(synonyms_compare, "output/test_synonyms_tools_output.rds")
