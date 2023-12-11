

library(treemap)
library(tidyverse)


a<-read_csv("data/austraits_name_summary.csv")


treemap(a,index = "z",vSize="counter",vColor = `Name handling class`,type="categorical",fontsize.labels=32)

a$ `Name handling class`<-as.factor(a$`Name handling class`)

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
