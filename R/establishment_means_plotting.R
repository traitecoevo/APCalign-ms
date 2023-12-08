library(APCalign)
library(tidyverse)
library(ozmaps)
library(sf)

resources<-load_taxonomic_resources()
ss <- create_species_state_origin_matrix(resources = resources)
apcfam<-APCalign:::get_apc_genus_family_lookup()
ss$genus<-word(ss$species,1,1)
ss2<-left_join(ss,apcfam)

plot_taxa_heat_map <- function(fam, s2=s2) {
  
  ss2 %>%
    pivot_longer(2:19, names_to = "State") %>%
    filter(family==fam) %>%
    filter(value != "not present") %>%
    filter(
      value %in% c(
        "native",
        "presumed extinct",
        "naturalised",
        "formerly naturalised",
        "doubtfully naturalised"
      )
    ) %>%
    filter(State %in% c("WA", "Qld", "NT", "NSW", "Vic", "Tas", "SA", "ACT")) %>%
    group_by(State, value) %>%
    summarise (`number of species` = n()) %>%
    mutate(family=fam)
}

plot_list<-list()
for (i in c("Fabaceae","Myrtaceae","Orchidaceae","Asteraceae","Poaceae","Proteaceae")){
  plot_list[[i]]<-plot_taxa_heat_map(i)
}
all_data<-bind_rows(plot_list)

theme_set(theme_classic(base_size = 16))

# Create the plot with improved aesthetics
ggplot(all_data, aes(x = State, y = value, fill = `number of species`)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC", high = "#FF0000") +
  coord_fixed() +
  facet_wrap(~family, ncol = 2) +
  ylab("") +
  xlab("State/Territory") +
  # Customize title and label sizes
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    strip.text = element_text(size = 16) # Adjust facet label size
  )

ggsave("output/establishment_by_state.pdf")
ggsave("output/establishment_by_state.png")







#### Mapping below here

aus <- ozmap_data(data = "states")

all_data$NAME<-case_when(all_data$State=="NSW" ~ "New South Wales",
                         all_data$State=="ACT" ~ "Australian Capital Territory",
                         all_data$State=="NT" ~ "Northern Territory",
                         all_data$State=="Tas" ~ "Tasmania",
                         all_data$State=="WA" ~ "Western Australia",
                         all_data$State=="Qld" ~ "Queensland",
                         all_data$State=="Vic" ~ "Victoria",
                         all_data$State=="SA" ~ "South Australia"
)

merged_data<-left_join(aus,all_data) 

native<-filter(merged_data,value=="native")

ggplot(data = native) +
  geom_sf(aes(fill = `number of species`))+
  scale_fill_gradient2(low = "#075AFF", mid = "#FFFFCC", high = "#FF0000")+
  facet_wrap(~family, ncol = 2) +
  labs(title = "Native Species Richness in Australian States and Territories")+theme_void()