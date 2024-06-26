##
## Summaries and descriptive maps
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## Libraries ####
library(plyr)
library(dplyr)
library(tidyr)
library(tibble)
library(sf)
library(classInt)
library(ggplot2)
library(RColorBrewer)
library(patchwork)
library(rnaturalearth)
library(ggspatial)

## Read data ####
abund_data <- read.csv("./data-processed/abund.csv")
occ_data <- read.csv("./data-processed/occ.csv")

# Column names
all_taxa_cols <- colnames(abund_data)[8:67]
sp_only_cols <- colnames(occ_data)[17:60]

all_procellariifomes <- 
  all_taxa_cols[all_taxa_cols %in% c("bulweria_bulwerii", "calonectris_borealis", 
                                     "calonectris_edwardsii", "calonectris_sp", 
                                     "daption_capensis", "diomedea_dabbenena", 
                                     "diomedea_epomophora", "diomedea_exulans",
                                     "diomedea_sanfordi", "diomedea_sp", 
                                     "fregetta_grallaria", "fregetta_tropica",
                                     "fulmarus_glacialoides", "hydrobatidae",
                                     "hydrobates_pelagicus", "macronectes_giganteus",
                                     "macronectes_halli", "macronectes_sp",
                                     "oceanites_oceanicus", "oceanitidae", 
                                     "oceanodroma_leucorhoa", "pachyptila_sp",
                                     "procellaria_aequinoctialis", "procellaria_conspicillata",
                                     "procellaria_sp", "procellariidae",
                                     "pterodroma_arminjoniana", "pterodroma_incerta",
                                     "pterodroma_mollis", "pterodroma_sp",
                                     "puffinus_gravis", "puffinus_griseus",
                                     "puffinus_puffinus", "puffinus_sp",
                                     "thalassarche_chlororhynchos", "thalassarche_melanophris",
                                     "thalassarche_sp")]

sp_procellariifomes <- all_procellariifomes[! grepl(pattern = "dae$|_sp",
                                                    x = all_procellariifomes)]

all_charadriiformes <- 
  all_taxa_cols[all_taxa_cols %in% c("anous_stolidus", "anous_sp",
                                     "chroicocephalus_maculipennis", "gygis_alba",
                                     "larus_dominicanus", "leucophaeus_atricilla",
                                     "onychoprion_fuscatus", "phaetusa_simplex",
                                     "stercorarius_antarticus",
                                     "stercorarius_longicaudus", "stercorarius_maccormicki",
                                     "stercorarius_parasiticus", "stercorarius_pomarinus",
                                     "stercorarius_skua", "stercorarius_sp",
                                     "sterna_dougalli", "sterna_hirundinacea",
                                     "sterna_hirundo", "sterna_paradisaea",
                                     "sterna_sp", "sterna_trudeaui",
                                     "thalasseus_acuflavidus", "thalasseus_maximus")]

sp_charadriiformes <- all_charadriiformes[! grepl(pattern = "dae$|_sp",
                                                  x = all_charadriiformes)]

## Summaries for Table 1 ####

# abundance ECOREGION summary
abund_data %>% 
  dplyr::select(ecoregion, total_abund) %>% 
  dplyr::group_by(ecoregion) %>% 
  dplyr::summarise(total_birds = sum(total_abund, na.rm = TRUE),
                   n_samples = n())

# abundance PROVINCE summary
abund_data %>% 
  dplyr::select(province, total_abund) %>% 
  dplyr::group_by(province) %>% 
  dplyr::summarise(total_birds = sum(total_abund, na.rm = TRUE),
                   n_samples = n())

# species richness ECOREGION summary
occ_data %>% 
  dplyr::select(ecoregion, all_of(sp_only_cols)) %>% 
  tidyr::pivot_longer(., cols = all_of(sp_only_cols)) %>% 
  dplyr::group_by(ecoregion) %>% 
  dplyr::summarise(sp_richness = n_distinct(name[value>0])) %>% 
  cbind(occ_data %>% 
          dplyr::select(ecoregion, all_of(sp_only_cols)) %>% 
          dplyr::group_by(ecoregion) %>% 
          dplyr::summarise(n_samples = n()) %>% 
          dplyr::select(n_samples))

# species richness PROVINCE summary
occ_data %>% 
  dplyr::select(province, all_of(sp_only_cols)) %>% 
  tidyr::pivot_longer(., cols = all_of(sp_only_cols)) %>% 
  dplyr::group_by(province) %>% 
  dplyr::summarise(sp_richness = n_distinct(name[value>0])) %>% 
  cbind(occ_data %>% 
          dplyr::select(province, all_of(sp_only_cols)) %>% 
          dplyr::group_by(province) %>% 
          dplyr::summarise(n_samples = n()) %>% 
          dplyr::select(n_samples))

## Overall summaries ####

# At least 1 bird recorded?
plyr::count(abund_data$total_abund > 0)

# Total number of birds and species recorded
sum(abund_data$total_abund)
length(sp_only_cols)

# Procellariiformes number of birds and species recorded
sum(abund_data$procellariiformes_abund)
length(sp_procellariifomes)

# Charadriiformes number of birds and species recorded
sum(abund_data$charadriiformes_abund)
length(sp_charadriiformes)

# How many occurrences per species
number_occ_by_species <- 
  apply(occ_data[, sp_only_cols], MARGIN = 2, sum) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("species") %>% 
  dplyr::rename("number_occ" = '.') %>% 
  dplyr::arrange(desc(number_occ))

View(number_occ_by_species)

## Prep data for maps ####

### ---- Transform into spatial feature

# To be used for the Effort map
sf_counts <- 
  occ_data %>% 
  sf::st_as_sf(coords = c("longitude", "latitude")) %>% 
  sf::st_set_crs(4326)

# To be used for the Richness maps
sf_occ <- 
  occ_data %>% 
  dplyr::filter(sp_richness != 0) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude")) %>% 
  sf::st_set_crs(4326)

# To be used for the (Normalised) Relative abundance maps
sf_abund <- 
  abund_data %>% 
  sf::st_as_sf(coords = c("longitude", "latitude")) %>% 
  sf::st_set_crs(4326)

### ---- Create function to normalise values of abundance
normFunction <- function(x) {(x - min(x)) / (max(x) - min(x))}

### ---- Create 1 x 1 degree grid cells
grid <- 
  sf::st_as_sf(
    sf::st_make_grid(sf_counts, cellsize = c(1, 1)), 
    crs = 4326) %>% 
  dplyr::mutate(id = 1:nrow(.))

## Spatial wrangling and calculate values for maps ####

## A repetitive section, as I didn't figure out how to automatise it... 

### ---- Effort map ---- ###

grid_effort <- 
  sf::st_join(sf_counts, grid)

# mapview::mapview(grid_effort, zcol="id") + grid

# ONE samples did not merge with the grid -- so I'm manually specifying it to 
# be (not even) 30 meters to the east, in grid 'id' "1" (for the descriptive maps)
grid_effort$id[is.na(grid_effort$id)] <- 1

grid_effort <- 
  grid_effort %>% 
  sf::st_drop_geometry() %>% 
  as.data.frame() %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(n = n())

grid_effort <- 
  merge(grid_effort, grid, by = "id") %>% 
  sf::st_as_sf()

# Set breaks to make it a better plot
n_brks <- classInt::classIntervals(grid_effort$n, n = 7, style = "pretty")

grid_effort <- 
  grid_effort %>% 
  dplyr::mutate(n_breaks = cut(n, n_brks$brks))

# mapview::mapview(grid_effort, zcol = "n")
# sf::st_write(grid_effort, "./results/grid_effort.gpkg")

### ---- Overall species richness ---- ###

grid_overall_sprich <- 
  sf::st_join(sf_occ, grid)

# mapview::mapview(grid_overall_sprich, zcol="id") + grid

# ONE samples did not merge with the grid -- so I'm manually specifying it to 
# be (not even) 30 meters to the east, in grid 'id' "1" (for the descriptive maps)
grid_overall_sprich$id[is.na(grid_overall_sprich$id)] <- 1

grid_overall_sprich <- 
  sf::st_drop_geometry(grid_overall_sprich) %>% 
  as.data.frame() %>% 
  dplyr::select(id, all_of(sp_only_cols))

grid_overall_sprich <-
  grid_overall_sprich %>% 
  tidyr::pivot_longer(cols = all_of(sp_only_cols),
                      names_to = "species",
                      values_to = "pres_abs") %>% 
  dplyr::filter(pres_abs > 0) %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(sp_rich = n_distinct(species))

grid_overall_sprich <- 
  merge(grid_overall_sprich, grid, by = "id") %>% 
  sf::st_as_sf()

# Set breaks to make it a better plot
n_brks <- classInt::classIntervals(grid_overall_sprich$sp_rich, 
                                   n = 8, style = "pretty") # returns 9 classes.

grid_overall_sprich <- 
  grid_overall_sprich %>% 
  dplyr::mutate(n_breaks = cut(sp_rich, n_brks$brks))

# mapview::mapview(grid_overall_sprich, zcol = "sp_rich")
# sf::st_write(grid_overall_sprich, "./results/grid_overall-species-richness.gpkg")

### ---- (Normalised) Relative abundance ---- ###

grid_norm_abund <- 
  sf::st_join(sf_abund, grid)

# mapview::mapview(grid_norm_abund, zcol="id") + grid

# ONE samples did not merge with the grid -- so I'm manually specifying it to 
# be (not even) 30 meters to the east, in grid 'id' "1" (for the descriptive maps)
grid_norm_abund$id[is.na(grid_norm_abund$id)] <- 1

grid_norm_abund <- 
  sf::st_drop_geometry(grid_norm_abund) %>% 
  as.data.frame() %>% 
  dplyr::select(id, all_of(all_taxa_cols))

# Remove columns that sum zero
grid_norm_abund <- 
  grid_norm_abund[, colSums(grid_norm_abund != 0) > 0]

tmp_spp_cols <- colnames(grid_norm_abund)[-1] # 1=='id'

## Calculate mean values for each species, by grid ('id')
grid_norm_abund <-
  grid_norm_abund %>% 
  tidyr::pivot_longer(cols = all_of(tmp_spp_cols),
                      names_to = "species",
                      values_to = "count") %>% 
  dplyr::filter(count > 0) %>% 
  dplyr::group_by(id, species) %>% 
  dplyr::summarise(mean_abund = mean(count)) %>% 
  tidyr::pivot_wider(id_cols = "id",
                     names_from = "species",
                     values_from = "mean_abund",
                     values_fill = 0)

## Normalise values for each species individually
grid_norm_abund[, tmp_spp_cols] <- 
  apply(grid_norm_abund[, tmp_spp_cols], MARGIN = 2, normFunction)

## Sum the normalised values into 'norm_relative_abund'
grid_norm_abund <- 
  dplyr::mutate(grid_norm_abund, 
                norm_relative_abund = rowSums(across(all_of(tmp_spp_cols)))) 

## Merge it back again with 'grid'
grid_norm_abund <- 
  merge(grid_norm_abund, grid, by = "id") %>% 
  sf::st_as_sf()

# Set breaks to make it a better plot
n_brks <- classInt::classIntervals(grid_norm_abund$norm_relative_abund, 
                                   n = 6, style = "pretty")

grid_norm_abund <- 
  grid_norm_abund %>% 
  dplyr::mutate(n_breaks = cut(norm_relative_abund, n_brks$brks))

# mapview::mapview(grid_norm_abund, zcol = "norm_relative_abund")
# sf::st_write(grid_norm_abund, "./results/grid_overall-normalised-abundance.gpkg")

### ---- Procellariiformes species richness ---- ###

grid_procellariiformes_sprich <- 
  sf::st_join(sf_occ, grid)

# mapview::mapview(grid_procellariiformes_sprich, zcol="id") + grid

# ONE samples did not merge with the grid -- so I'm manually specifying it to 
# be (not even) 30 meters to the east, in grid 'id' "1" (for the descriptive maps)
grid_procellariiformes_sprich$id[is.na(grid_procellariiformes_sprich$id)] <- 1

grid_procellariiformes_sprich <- 
  sf::st_drop_geometry(grid_procellariiformes_sprich) %>% 
  as.data.frame() %>% 
  dplyr::select(id, all_of(sp_procellariifomes))

grid_procellariiformes_sprich <-
  grid_procellariiformes_sprich %>% 
  tidyr::pivot_longer(cols = all_of(sp_procellariifomes),
                      names_to = "species",
                      values_to = "pres_abs") %>% 
  dplyr::filter(pres_abs > 0) %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(sp_rich = n_distinct(species))

grid_procellariiformes_sprich <- 
  merge(grid_procellariiformes_sprich, grid, by = "id") %>% 
  sf::st_as_sf()

# Set breaks to make it a better plot
n_brks <- classInt::classIntervals(grid_procellariiformes_sprich$sp_rich, 
                                   n = 5, style = "pretty") 

grid_procellariiformes_sprich <- 
  grid_procellariiformes_sprich %>% 
  dplyr::mutate(n_breaks = cut(sp_rich, n_brks$brks))

# mapview::mapview(grid_procellariiformes_sprich, zcol = "sp_rich")
# sf::st_write(grid_procellariiformes_sprich, "./results/grid_procellariiformes-species-richness.gpkg")

### ---- Charadriiformes species richness ---- ###

grid_charadriiformes_sprich <- 
  sf::st_join(sf_occ, grid)

# mapview::mapview(grid_charadriiformes_sprich, zcol="id") + grid

# ONE samples did not merge with the grid -- so I'm manually specifying it to 
# be (not even) 30 meters to the east, in grid 'id' "1" (for the descriptive maps)
grid_charadriiformes_sprich$id[is.na(grid_charadriiformes_sprich$id)] <- 1

grid_charadriiformes_sprich <- 
  sf::st_drop_geometry(grid_charadriiformes_sprich) %>% 
  as.data.frame() %>% 
  dplyr::select(id, all_of(sp_charadriiformes))

grid_charadriiformes_sprich <-
  grid_charadriiformes_sprich %>% 
  tidyr::pivot_longer(cols = all_of(sp_charadriiformes),
                      names_to = "species",
                      values_to = "pres_abs") %>% 
  dplyr::filter(pres_abs > 0) %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(sp_rich = n_distinct(species))

grid_charadriiformes_sprich <- 
  merge(grid_charadriiformes_sprich, grid, by = "id") %>% 
  sf::st_as_sf()

# Set breaks to make it a better plot
n_brks <- classInt::classIntervals(grid_charadriiformes_sprich$sp_rich, 
                                   n = 5, style = "pretty")

grid_charadriiformes_sprich <- 
  grid_charadriiformes_sprich %>% 
  dplyr::mutate(n_breaks = cut(sp_rich, n_brks$brks))

# mapview::mapview(grid_charadriiformes_sprich, zcol = "sp_rich")
# sf::st_write(grid_charadriiformes_sprich, "./results/grid_charadriiformes-species-richness.gpkg")

### ---- (Normalised) Procellariiformes relative abundance ---- ###

grid_procellariiformes_norm_abund <- 
  sf::st_join(sf_abund, grid)

# mapview::mapview(grid_procellariiformes_norm_abund, zcol="id") + grid

# ONE samples did not merge with the grid -- so I'm manually specifying it to 
# be (not even) 30 meters to the east, in grid 'id' "1" (for the descriptive maps)
grid_procellariiformes_norm_abund$id[is.na(grid_procellariiformes_norm_abund$id)] <- 1

grid_procellariiformes_norm_abund <- 
  sf::st_drop_geometry(grid_procellariiformes_norm_abund) %>% 
  as.data.frame() %>% 
  dplyr::select(id, all_of(all_procellariifomes))

# Remove columns that sum zero
grid_procellariiformes_norm_abund <-
  grid_procellariiformes_norm_abund[, colSums(grid_procellariiformes_norm_abund != 0) > 0]

tmp_spp_cols <- colnames(grid_procellariiformes_norm_abund)[-1] # 1=='id'

grid_procellariiformes_norm_abund <-
  grid_procellariiformes_norm_abund %>% 
  tidyr::pivot_longer(cols = all_of(tmp_spp_cols),
                      names_to = "species",
                      values_to = "count") %>% 
  dplyr::filter(count > 0) %>% 
  dplyr::group_by(id, species) %>% 
  dplyr::summarise(mean_abund = mean(count)) %>% 
  tidyr::pivot_wider(id_cols = "id",
                     names_from = "species",
                     values_from = "mean_abund",
                     values_fill = 0)

## Normalise values for each species individually
grid_procellariiformes_norm_abund[, tmp_spp_cols] <- 
  apply(grid_procellariiformes_norm_abund[, tmp_spp_cols], MARGIN = 2, normFunction)

## Sum the normalised values into 'norm_relative_abund'
grid_procellariiformes_norm_abund <- 
  dplyr::mutate(grid_procellariiformes_norm_abund, 
                norm_relative_abund = rowSums(across(all_of(tmp_spp_cols)))) 

## Merge it back again with 'grid'
grid_procellariiformes_norm_abund <- 
  merge(grid_procellariiformes_norm_abund, grid, by = "id") %>% 
  sf::st_as_sf()

# Set breaks to make it a better plot
n_brks <- classInt::classIntervals(grid_procellariiformes_norm_abund$norm_relative_abund, 
                                   n = 5, style = "pretty")

grid_procellariiformes_norm_abund <- 
  grid_procellariiformes_norm_abund %>% 
  dplyr::mutate(n_breaks = cut(norm_relative_abund, n_brks$brks))

# mapview::mapview(grid_procellariiformes_norm_abund, zcol = "norm_relative_abund")
# sf::st_write(grid_procellariiformes_norm_abund, "./results/grid_procellariiformes-normalised-abundance.gpkg")

### ---- (Normalised) Charadriiformes relative abundance ---- ###

grid_charadriiformes_norm_abund <- 
  sf::st_join(sf_abund, grid)

# mapview::mapview(grid_charadriiformes_norm_abund, zcol="id") + grid

# ONE samples did not merge with the grid -- so I'm manually specifying it to 
# be (not even) 30 meters to the east, in grid 'id' "1" (for the descriptive maps)
grid_charadriiformes_norm_abund$id[is.na(grid_charadriiformes_norm_abund$id)] <- 1

grid_charadriiformes_norm_abund <- 
  sf::st_drop_geometry(grid_charadriiformes_norm_abund) %>% 
  as.data.frame() %>% 
  dplyr::select(id, all_of(all_charadriiformes))

# Remove columns that sum zero
grid_charadriiformes_norm_abund <-
  grid_charadriiformes_norm_abund[, colSums(grid_charadriiformes_norm_abund != 0) > 0]

tmp_spp_cols <- colnames(grid_charadriiformes_norm_abund)[-1] # 1=='id'

grid_charadriiformes_norm_abund <-
  grid_charadriiformes_norm_abund %>% 
  tidyr::pivot_longer(cols = all_of(tmp_spp_cols),
                      names_to = "species",
                      values_to = "count") %>% 
  dplyr::filter(count > 0) %>% 
  dplyr::group_by(id, species) %>% 
  dplyr::summarise(mean_abund = mean(count)) %>% 
  tidyr::pivot_wider(id_cols = "id",
                     names_from = "species",
                     values_from = "mean_abund",
                     values_fill = 0)

## Normalise values for each species individually
grid_charadriiformes_norm_abund[, tmp_spp_cols] <- 
  apply(grid_charadriiformes_norm_abund[, tmp_spp_cols], MARGIN = 2, normFunction)

## Sum the normalised values into 'norm_relative_abund'
grid_charadriiformes_norm_abund <- 
  dplyr::mutate(grid_charadriiformes_norm_abund, 
                norm_relative_abund = rowSums(across(all_of(tmp_spp_cols)))) 

## Merge it back again with 'grid'
grid_charadriiformes_norm_abund <- 
  merge(grid_charadriiformes_norm_abund, grid, by = "id") %>% 
  sf::st_as_sf()

# Set breaks to make it a better plot
n_brks <- classInt::classIntervals(grid_charadriiformes_norm_abund$norm_relative_abund, 
                                   n = 5, style = "pretty")

grid_charadriiformes_norm_abund <- 
  grid_charadriiformes_norm_abund %>% 
  dplyr::mutate(n_breaks = cut(norm_relative_abund, n_brks$brks))

# mapview::mapview(grid_charadriiformes_norm_abund, zcol = "norm_relative_abund")
# sf::st_write(grid_charadriiformes_norm_abund, "./results/grid_charadriiformes-normalised-abundance.gpkg")

# gc()

### Plotting the maps ####

# RColorBrewer::display.brewer.all(colorblindFriendly = TRUE)
# I'll use --  
# for overall maps = "RdYlBu", 
# for Procellariiformes = "Blues", 
# for Charadriiformes = "Greens"

## ---- Base map (including a bit of a hack to include French Guiana) ---- ##

## Brazil
brazil_map <-
  rnaturalearth::ne_countries(country = "brazil",
                              returnclass = "sf")
# mapview::mapview(brazil_map)

SA_map <-
  rnaturalearth::ne_countries(continent = 'south america',
                              type = 'countries',
                              returnclass = "sf")
# mapview::mapview(SA_map)

## French Guiana is not part of "South America", apparently.
# So needed to do a bit of a hack here...
FG_map <- 
  rnaturalearth::ne_countries(country = 'france',
                              returnclass = "sf") %>% 
  sf::st_cast(., "MULTIPOLYGON") %>% 
  sf::st_cast(., "POLYGON", do_split = TRUE) %>% 
  tibble::rownames_to_column() %>%
  dplyr::filter(rowname == "43") # French Guiana
# mapview::mapview(FG_map)

## Join South America and French Guiana into a single feature
SA_map <- sf::st_union(SA_map, FG_map)
# mapview::mapview(SA_map) # Done!

rm("FG_map")

## Base maps 
gg_SouthAmerica <-
  ggplot() + 
  geom_sf(data = SA_map, colour = "grey", fill = "lightgrey") +
  theme_bw()

gg_brazil <- 
  gg_SouthAmerica + 
  geom_sf(data = brazil_map, colour = "black", fill = "darkgrey") + 
  labs(x = "Longitude", y = "Latitude") +
  xlim(c(-75, -26)) + ylim(c(-35, 5)) + 
  theme_bw() + 
  theme(axis.title = element_text(colour = "black", size = 8), 
        axis.text = element_text(colour = "black", size = 7),
        axis.line = element_line(colour = "black"))

rm("gg_SouthAmerica")

## ---- Add the data layers ---- ##

## Effort (number of samples)
effortTot <- 
  gg_brazil + 
  annotate(geom = "text", x = -75, y = 5, label = "a)",
           fontface = "bold", color = "black", size = 6.5) +
  geom_sf(data = grid_effort, aes(fill = n_breaks)) + 
  scale_fill_brewer(palette = "RdYlBu", direction = -1, 
                    name = "Number of samples\n(counts)") +
  theme(legend.position = c(0.2, 0.28), 
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.4, 'cm'),
        legend.background = element_rect(linetype = "solid", colour = "black"))

## Species richness
richTot <- 
  gg_brazil + 
  annotate(geom = "text", x = -75, y = 5, label = "b)",
           fontface = "bold", color = "black", size = 6.5) +
  geom_sf(data = grid_overall_sprich, aes(fill = n_breaks)) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1, name = "Species richness") + 
  ggspatial::annotation_scale(style = "bar", location = "br", 
                              pad_x = unit(5, "mm"), pad_y = unit(5, "mm"),
                              text_cex = 0.7) + 
  ggspatial::annotation_north_arrow(style = north_arrow_fancy_orienteering, 
                                    location = "br", which_north = "true", 
                                    pad_x = unit(9, "mm"), pad_y = unit(9, "mm"),
                                    height = unit(1, "cm"), width = unit(1, "cm")) +
  theme(legend.position = c(0.18, 0.3), 
        legend.title = element_text(size = 8), 
        legend.text = element_text(size = 8), 
        legend.key.size = unit(0.4, 'cm'),
        legend.background = element_rect(linetype = "solid", colour = "black"))

## Mean normalised abundance
abundTot <- 
  gg_brazil + 
  annotate(geom = "text", x = -75, y = 5, label = "c)",
           fontface = "bold", color = "black", size = 6.5) +
  geom_sf(data = grid_norm_abund, aes(fill = n_breaks)) + 
  scale_fill_brewer(palette = "RdYlBu", direction = -1, name = "Normalised\nRelative abundance") +
  theme(legend.position = c(0.2, 0.28), 
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8), 
        legend.key.size = unit(0.4, 'cm'), 
        legend.background = element_rect(linetype = "solid", colour = "black"))

## Procellariiformes richness
richProce <- 
  gg_brazil + 
  annotate(geom = "text", x = -75, y = 5, label = "a)",
           fontface = "bold", color = "black", size = 6.5) +
  geom_sf(data = grid_procellariiformes_sprich, aes(fill = n_breaks)) + 
  scale_fill_brewer(palette = "Blues", name = "Species richness") +
  theme(legend.position = c(0.18, 0.25), 
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8), 
        legend.key.size = unit(0.4, 'cm'), 
        legend.background = element_rect(linetype = "solid", colour = "black"))

## Procellariiformes mean normalised abundance
abundProce <- 
  gg_brazil + 
  annotate(geom = "text", x = -75, y = 5, label = "b)",
           fontface = "bold", color = "black", size = 6.5) +
  geom_sf(data = grid_procellariiformes_norm_abund, aes(fill = n_breaks)) + 
  scale_fill_brewer(palette = "Blues", name = "Normalised\nRelative abundance") + 
  ggspatial::annotation_scale(style = "bar", location = "br", 
                              pad_x = unit(5, "mm"), pad_y = unit(5, "mm"),
                              text_cex = 0.7) + 
  ggspatial::annotation_north_arrow(style = north_arrow_fancy_orienteering, 
                                    location = "br", which_north = "true", 
                                    pad_x = unit(9, "mm"), pad_y = unit(9, "mm"),
                                    height = unit(1, "cm"), width = unit(1, "cm")) +
  theme(legend.position = c(0.2, 0.28), 
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8), 
        legend.key.size = unit(0.4, 'cm'), 
        legend.background = element_rect(linetype = "solid", colour = "black"))

## Charadriiformes richness
richChara <- 
  gg_brazil + 
  annotate(geom = "text", x = -75, y = 5, label = "c)",
           fontface = "bold", color = "black", size = 6.5) +
  geom_sf(data = grid_charadriiformes_sprich, aes(fill = n_breaks)) + 
  scale_fill_brewer(palette = "Greens", name = "Species richness") + 
  ggspatial::annotation_scale(style = "bar", location = "br", 
                              pad_x = unit(5, "mm"), pad_y = unit(5, "mm"),
                              text_cex = 0.7) + 
  ggspatial::annotation_north_arrow(style = north_arrow_fancy_orienteering, 
                                    location = "br", which_north = "true", 
                                    pad_x = unit(9, "mm"), pad_y = unit(9, "mm"),
                                    height = unit(1, "cm"), width = unit(1, "cm")) +
  theme(legend.position = c(0.18, 0.25), 
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8), 
        legend.key.size = unit(0.4, 'cm'), 
        legend.background = element_rect(linetype = "solid", colour = "black"))

## Charadriiformes mean normalized abundance
abundChara <- 
  gg_brazil + 
  annotate(geom = "text", x = -75, y = 5, label = "d)",
           fontface = "bold", color = "black", size = 6.5) +
  geom_sf(data = grid_charadriiformes_norm_abund, aes(fill = n_breaks)) + 
  scale_fill_brewer(palette = "Greens", name = "Normalised\nRelative abundance") +
  theme(legend.position = c(0.2, 0.25), 
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8), 
        legend.key.size = unit(0.4, 'cm'), 
        legend.background = element_rect(linetype = "solid", colour = "black"))

## ---- Tile the maps with {patchwork} and save them ---- ##

summaryPlot <- effortTot / richTot / abundTot

ggplot2::ggsave(summaryPlot, 
                filename = "./results/effort_richness_norm-abund.pdf", 
                height = 27, width = 12, units = "cm", dpi = 300)

ordersPlot <- (richProce | abundProce) / (richChara | abundChara)

ggplot2::ggsave(ordersPlot, 
                filename = "./results/procellariiformes-charadriiformes_richness_norm-abund.pdf", 
                height = 20 , width = 20, units = "cm", dpi = 300)
