

library(treemap)
library(tidyverse)


a<-read_csv("data/austraits_name_summary.csv")



a$`Name handling class`<-as.factor(a$`Name handling class`)

# Calculate the sums for each 'Name handling class'
class_counts <- a %>%
  group_by(`Name handling class`) %>%
  summarise(Total = sum(counter)) %>%
  ungroup()


overall_total <- sum(class_counts$Total)

# Create a new string with the class name and the percentage
class_counts <- class_counts %>%
  mutate(Percentage = (Total / overall_total) * 100,
         Label = paste(`Name handling class`, sprintf("(%0.2f%%)", Percentage)))

# Create a lookup table to match the old class names with the new labels
lookup_table <- class_counts %>%
  select(`Name handling class`, Label)

# Join this back to the original data to create a new factor
a <- a %>%
  left_join(lookup_table) %>%
  mutate(`Name handling class` = as.factor(Label))


class_sums <- a %>%
  group_by(`Name handling class`) %>%
  summarise(Total = sum(counter)) %>%
  arrange(-Total) %>%
  ungroup()



# Order the factor levels based on the sums
a$`Name handling class` <- factor(a$`Name handling class`, levels = class_sums$`Name handling class`)



num_colors <- length(unique(a$`Name handling class`))
subdued_rainbow_palette <- hcl(seq(15, 375, length = num_colors + 1), 
                               l = 65, c = 100)[1:num_colors]

png("treemap.png",width=11, height = 5, res = 450,units="in")
treemap(
  a,
  index = "z",         # Column for labels
  vSize = "counter",   # Column for size
  vColor = "Name handling class",
  type="categorical", # Column for color
  fontsize.labels = 12, # Adjust label font size as needed
  border.col = "white", # Color of the borders (change as desired)
  title = "Types of corrections in the AusTraits data assembly process",
  palette = subdued_rainbow_palette
)
dev.off()
