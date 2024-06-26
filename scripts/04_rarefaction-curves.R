##
## Rarefaction curves - {iNEXT}
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## Libraries ####
library(dplyr)
library(iNEXT)
library(ggplot2)
library(patchwork)

## Read the data #### 
data <- read.csv("./data-processed/occ_iNEXT.csv")

species_cols <- colnames(data)[17:ncol(data)]

## {iNEXT} prep ####

# unique(data$ecoregion)

list_inext_ecoregion <- 
  list("Rio Grande" = 
         data.frame(t(data[data$ecoregion == "Rio Grande", c(species_cols)])), 
       "Southeastern Brazil" = 
         data.frame(t(data[data$ecoregion == "Southeastern Brazil", c(species_cols)])),
       "Eastern Brazil" = 
         data.frame(t(data[data$ecoregion == "Eastern Brazil", c(species_cols)])),
       "Trindade and Martin Vaz Islands" = 
         data.frame(t(data[data$ecoregion == "Trindade and Martin Vaz Islands", c(species_cols)])),
       "Northeastern Brazil" = 
         data.frame(t(data[data$ecoregion == "Northeastern Brazil", c(species_cols)])),
       "Amazonia" = 
         data.frame(t(data[data$ecoregion == "Amazonia", c(species_cols)])))

# unique(data$province)

list_inext_province <- 
  list("Warm Temperate Southwestern Atlantic" = 
         data.frame(t(data[data$province == "Warm Temperate Southwestern Atlantic", c(species_cols)])), 
       "Tropical Southwestern Atlantic" = 
         data.frame(t(data[data$province == "Tropical Southwestern Atlantic", c(species_cols)])),
       "North Brazil Shelf" = 
         data.frame(t(data[data$province == "North Brazil Shelf", c(species_cols)])))

## Rarefaction curves ####

## iNEXT function [q = 0 means spp richness itself]
inext_eco <- 
  iNEXT::iNEXT(list_inext_ecoregion, q = c(0), ## q = c(0,1,2)
               datatype = "incidence_raw") 

# Sample-size-based R/E curve
curve_sample_size_based <- 
  iNEXT::ggiNEXT(inext_eco, type = 1, se = TRUE) + 
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  coord_cartesian(ylim = c(0, 43)) +
  theme_bw() + 
  theme(legend.position = "none")

# Coverage-based R/E curves
curve_coverage_based <- 
  iNEXT::ggiNEXT(inext_eco, type = 3, se = TRUE) + 
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  coord_cartesian(ylim = c(0, 43)) +
  ylab("") +
  theme_bw() +
  theme(legend.position = "right") +
  guides(linetype = "none")

inext_ecoregion <- curve_sample_size_based + curve_coverage_based

ggsave(inext_ecoregion, 
       filename = "./results/iNEXT_eco.pdf", 
       height = 10 , width = 22, units = "cm", dpi = 300)

## iNEXT function [q = 0 means spp richness itself]
inext_pro <- 
  iNEXT::iNEXT(list_inext_province, q = c(0), ## q = c(0,1,2)
               datatype = "incidence_raw") 

# Sample-size-based R/E curve
curve_sample_size_based_p <- 
  iNEXT::ggiNEXT(inext_pro, type = 1, se = TRUE) + 
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  coord_cartesian(ylim = c(0, 43)) +
  theme_bw() + 
  theme(legend.position = "none")

# Coverage-based R/E curves
curve_coverage_based_p <- 
  iNEXT::ggiNEXT(inext_pro, type = 3, se = TRUE) + 
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  coord_cartesian(ylim = c(0, 43)) +
  ylab("") +
  theme_bw() +
  theme(legend.position = "right") +
  guides(linetype = "none")

inext_province <- curve_sample_size_based_p + curve_coverage_based_p

ggsave(inext_province, 
       filename = "./results/iNEXT_province.pdf", 
       height = 10 , width = 22, units = "cm", dpi = 300)
