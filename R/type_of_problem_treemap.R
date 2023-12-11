

library(treemap)
library(tidyverse)


a<-read_csv("data/AusTraits_alignment_summary.csv")

extract_and_concatenate_numbers_with_char <- function(input_string) {
  # Extract numbers and the following character
  numbers_list <- str_extract_all(input_string, "(\\d+\\D)")
  
  # Flatten the list to a vector
  numbers_vector <- unlist(numbers_list)
  
  # Concatenate all numbers with their following character into a single string
  return(paste0(numbers_vector, collapse = ""))
}

a$code_number<-substr(sapply(a$alignment_code,extract_and_concatenate_numbers),1,3)


treemap(a,index = "code_number",vSize="count",fontsize.labels=32)
