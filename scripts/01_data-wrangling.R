##
## Raw to processed data
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## Libraries ####
library(dplyr)
library(tidyr)
library(sf)
library(stringr)

## Read seabird data and tidy it up #### 

## Raw dataset (columns in Portuguese)
raw_data <- read.csv("./data-raw/censosOficialLAATM.csv")

## Correct Date format
raw_data$Data <- as.Date(raw_data$Data, format = "%m/%d/%Y")

## Delete unused columns

# (Ship name, Observer name, Census type, Bearing, Ship speed, Depth, Cloud cover, 
#  Pressure, Wind direction, Wind speed, Salinity, Sea Surface Temperature, 
#  Non-identified birds, Observations on non-identified birds, Pinnipeds, Cetaceans, 
#  Sea turtles, General observations, Observations related to bearing,
#  Birds on the ship, Birds out the 300m band of counts) -- respectively.
raw_data <- 
  raw_data %>% 
  dplyr::select(-c(Embarcacao, Observador, Tipo.de.censo, Rumo, Velocidade.navio,
                   Profundidade, Cobertura.de.nuvens, Pressao, Vento.direcao,
                   Vento.velocidade, Salinidade, TSM, N.ID, Obs.N.ID, Pinipedes,
                   Cetaceos, Tartarugas, Obs.Geral, Obs.Rumo, Aves.na.embarcacao,
                   Aves.fora.da.area.de.censo))

# Remove non-seabird family/species
raw_data <- 
  raw_data %>% 
  dplyr::select(-c(Hirundinidae, Numenius.hudsonicus))

# Species columns 'as.numeric'
all_taxa_cols <- colnames(raw_data)[8:74]

raw_data[, all_taxa_cols] <- 
  apply(raw_data[, all_taxa_cols], 2, function(x) as.numeric(as.character(x)))

# As NA were introduced by coercion, replace them with zero
raw_data[, all_taxa_cols] <- 
  apply(raw_data[, all_taxa_cols], 2, tidyr::replace_na, 0)

## Calculate total abundance of seabirds, by Order

sp_procellariifomes <- 
  all_taxa_cols[all_taxa_cols %in% c("Bulweria.bulwerii", "Calonectris.borealis", 
                                     "Calonectris.edwardsii", "Calonectris.sp.", 
                                     "Daption.capensis", "Diomedea.dabbenena", 
                                     "Diomedea.epomophora", "Diomedea.exulans",
                                     "Diomedea.sanfordi", "Diomedea.sp.", 
                                     "Fregetta.grallaria", "Fregetta.tropica",
                                     "Fulmarus.glacialoides", "Hydrobatidae",
                                     "Hydrobates.pelagicus", "Macronectes.giganteus",
                                     "Macronectes.halli", "Macronectes.sp.",
                                     "Oceanites.oceanicus", "Oceanitidae", 
                                     "Oceanodroma.leucorhoa", "Pachyptila.sp.",
                                     "Procellaria.aequinoctialis", "Procellaria.conspicillata",
                                     "Procellaria.sp.", "Procellariidae",
                                     "Pterodroma.arminjoniana", "Pterodroma.incerta",
                                     "Pterodroma.mollis", "Pterodroma.sp.",
                                     "Puffinus.gravis", "Puffinus.griseus",
                                     "Puffinus.puffinus", "Puffinus.sp.",
                                     "Thalassarche.chlororhynchos", "Thalassarche.melanophris",
                                     "Thalassarche.sp.")]

sp_suliformes <- 
  all_taxa_cols[all_taxa_cols %in% c("Fregata.magnificens", "Fregata.sp.",
                                     "Sula.dactylatra", "Sula.leucogaster",
                                     "Sula.sp.", "Sula.sula")]

sp_charadriiformes <- 
  all_taxa_cols[all_taxa_cols %in% c("Anous.stolidus", "Anous.sp.",
                                     "Chroicocephalus.maculipennis", "Gygis.alba",
                                     "Larus.dominicanus", "Leucophaeus.atricilla",
                                     "Onychoprion.fuscatus", "Phaetusa.simplex",
                                     "Stercorarius.antarticus",
                                     "Stercorarius.longicaudus", "Stercorarius.maccormicki",
                                     "Stercorarius.parasiticus", "Stercorarius.pomarinus",
                                     "Stercorarius.skua", "Stercorarius.sp.",
                                     "Sterna.dougalli", "Sterna.hirundinacea",
                                     "Sterna.hirundo", "Sterna.paradisaea",
                                     "Sterna.sp.", "Sterna.trudeaui",
                                     "Thalasseus.acuflavidus", "Thalasseus.maximus")]

## For n=1 not worth creating a vector for 'calculating' anything... 
## (see below - it will be removed, anyway, too)
# sp_sphenisciformes <- 
#   all_taxa_cols[all_taxa_cols %in% c("Spheniscus.magellanicus")]

raw_data$Procellariiformes.abund <- 
  base::rowSums(raw_data[, c(sp_procellariifomes)])
raw_data$Suliformes.abund <- 
  base::rowSums(raw_data[, c(sp_suliformes)])
raw_data$Charadriiformes.abund <- 
  base::rowSums(raw_data[, c(sp_charadriiformes)])

## Calculate total abundance of seabirds, by Family

# Procellariiformes families
sp_diomedeidae <- 
  all_taxa_cols[all_taxa_cols %in% c("Diomedea.dabbenena", "Diomedea.epomophora",
                                     "Diomedea.exulans", "Diomedea.sanfordi",
                                     "Diomedea.sp.", "Thalassarche.chlororhynchos",
                                     "Thalassarche.melanophris", "Thalassarche.sp.")]

sp_procellariidae <- 
  all_taxa_cols[all_taxa_cols %in% c("Bulweria.bulwerii", "Calonectris.borealis", 
                                     "Calonectris.edwardsii", "Calonectris.sp.", 
                                     "Daption.capensis", "Fulmarus.glacialoides",
                                     "Macronectes.giganteus", "Macronectes.halli",
                                     "Macronectes.sp.", "Pachyptila.sp.",
                                     "Procellaria.aequinoctialis", "Procellaria.conspicillata",
                                     "Procellaria.sp.", "Procellariidae",
                                     "Pterodroma.arminjoniana", "Pterodroma.incerta",
                                     "Pterodroma.mollis", "Pterodroma.sp.",
                                     "Puffinus.gravis", "Puffinus.griseus",
                                     "Puffinus.puffinus", "Puffinus.sp.")]
sp_hydrobatidae <- 
  all_taxa_cols[all_taxa_cols %in% c("Fregetta.grallaria", "Fregetta.tropica",
                                     "Hydrobatidae", "Hydrobates.pelagicus", 
                                     "Oceanites.oceanicus", "Oceanitidae", 
                                     "Oceanodroma.leucorhoa")]

raw_data$Diomedeidae.abund <- base::rowSums(raw_data[, c(sp_diomedeidae)])
raw_data$Procellariidae.abund <- base::rowSums(raw_data[, c(sp_procellariidae)])
raw_data$Hydrobatidae.abund <- base::rowSums(raw_data[, c(sp_hydrobatidae)])

# Suliformes families
sp_sulidae <- 
  all_taxa_cols[all_taxa_cols %in% c("Sula.dactylatra", "Sula.leucogaster",
                                     "Sula.sp.", "Sula.sula")]

sp_fregatidae <- 
  all_taxa_cols[all_taxa_cols %in% c("Fregata.magnificens", "Fregata.sp.")]

raw_data$Fregatidae.abund <- base::rowSums(raw_data[, c(sp_fregatidae)])
raw_data$Sulidae.abund <- base::rowSums(raw_data[, c(sp_sulidae)])

# Charadriiformes families
sp_stercorariidae <- 
  all_taxa_cols[all_taxa_cols %in% c("Stercorarius.antarticus",
                                     "Stercorarius.longicaudus", "Stercorarius.maccormicki",
                                     "Stercorarius.parasiticus", "Stercorarius.pomarinus",
                                     "Stercorarius.skua", "Stercorarius.sp.")]

sp_laridae <- 
  all_taxa_cols[all_taxa_cols %in% c("Chroicocephalus.maculipennis", "Larus.dominicanus",
                                     "Leucophaeus.atricilla")]

sp_sternidae <- 
  all_taxa_cols[all_taxa_cols %in% c("Anous.stolidus", "Anous.sp.",
                                     "Gygis.alba", "Onychoprion.fuscatus", 
                                     "Phaetusa.simplex",
                                     "Sterna.dougalli", "Sterna.hirundinacea",
                                     "Sterna.hirundo", "Sterna.paradisaea",
                                     "Sterna.sp.", "Sterna.trudeaui",
                                     "Thalasseus.acuflavidus", "Thalasseus.maximus")]

raw_data$Stercorariidae.abund <- base::rowSums(raw_data[, c(sp_stercorariidae)])
raw_data$Laridae.abund <- base::rowSums(raw_data[, c(sp_laridae)])
raw_data$Sternidae.abund <- base::rowSums(raw_data[, c(sp_sternidae)])

## Calculate total abundance of seabirds
raw_data$total_abund <- base::rowSums(raw_data[, c(all_taxa_cols)])

## Remove taxa with zero records
sp_zero_records <- 
  # Get species names and number of occurrences
  data.frame(
    species = all_taxa_cols,
    n_occ = apply(raw_data[, all_taxa_cols], MARGIN = 2, function(x) sum(x >= 1)),
    row.names = NULL) %>%
  # Filter and pull species names
  dplyr::filter(n_occ < 1) %>%
  dplyr::pull(species)

raw_data <- 
  raw_data %>% dplyr::select(- all_of(sp_zero_records))

## Environmental data ####

## Read environmental data (after MATLAB processing -- see Methods in the main text)
raw_env_data <- read.csv("./data-raw/rawEnvData.csv")

## Create 'Date' and 'Hour' columns to merge with seabird data
raw_env_data$Data <- as.Date(with(raw_env_data, 
                                  paste(ano, mes, dia, sep = "-")), "%Y-%m-%d")

raw_env_data$Hora <- paste((base::sprintf("%02d", raw_env_data$hora)),
                           (base::sprintf("%02d", raw_env_data$minuto)), sep = ":")

raw_env_data <- raw_env_data[, - c(1:5)]

## Merge Seabird and Environmental data #### 

# Change a cell to avoid a conflict when merging

raw_env_data$Hora[
  raw_env_data$Hora == "08:10" & 
    raw_env_data$Data == "2015-04-27" & 
    raw_env_data$lat == -33.3825] <- "08:11"

raw_data$Hora[
  raw_data$Hora == "08:10" & 
    raw_data$Data == "2015-04-27" & 
    raw_data$Cruzeiro == "Talude IX"] <- "08:11"

seabirds_env_data <- merge(raw_data, raw_env_data, id = c("Data", "Hora"))

## Spatial merge with MEOW ####

## Transform the data in a spatial object
sf_seabirds_env_data <- 
  seabirds_env_data %>% 
  sf::st_as_sf(coords = c("lon", "lat")) %>% 
  sf::st_set_crs(4326)

## Read the MEOW's shapefile
MEOW <- 
  sf::st_read("./data-raw/MEOWv2_Final_June2007/MEOW_SWAtlantic.shp") %>% 
  dplyr::select(ECOREGION, PROVINCE)
# plot(MEOW) # Quick check

## PS: in the folder there are 2 shapefiles --
#  the original one, from Spalding et al 2007 BioScience; and
#  one which I masked only the SW Atlantic Ocean.
## Also, note: In the cropped 'SWAtlantic' file, I already joint Spalding's
# 'NE Brazil' + 'Sao Pedro and Sao Paulo Islands + 'Fernando de Naronha and Atoll das Rocas'
# into "Northeastern Brazil" (see Methods in the main text)

## Spatial join 
sf_seabirds_env_data_MEOW <- sf::st_join(sf_seabirds_env_data, MEOW)

## Check the join - - - - - -  - - - - - -
# library(mapview)
# mapview::mapview(sf_seabirds_env_data_MEOW, zcol="ECOREGION")
# mapview::mapview(sf_seabirds_env_data_MEOW, zcol="PROVINCE")
## - - - - - - - - - - -  - - - - - - - - -

# gc()

## Transform the spatial object back to data.frame
df_seabirds_env_data_MEOW <- sf::st_drop_geometry(sf_seabirds_env_data_MEOW)

## Ten sample-points were just off MEOW's border, 
## so I manually designate the MEOW classifications for them

df_seabirds_env_data_MEOW$ECOREGION <- 
  tidyr::replace_na(df_seabirds_env_data_MEOW$ECOREGION, "Eastern Brazil")
df_seabirds_env_data_MEOW$PROVINCE <- 
  tidyr::replace_na(df_seabirds_env_data_MEOW$PROVINCE, "Tropical Southwestern Atlantic")

## Specifically, these were the samples:
# Cruzeiro [trip] == Chevron I - 
# Date|Hour: 2011-12-21 | 06:19, 07:00, 07:30, 08:05
# Cruzeiro [trip] == Trindade III - 
# Date|Hour: 2012-03-16 | 05:36, 06:03, 06:32, 07:06, 07:48, 14:35
#
# All of them from the "Eastern Brazil" ECOREGION, 
# and the "Tropical Southwestern Atlantic" PROVINCE.

## Check Ecoregions/Provinces
# unique(df_seabirds_env_data_MEOW$ECOREGION)
# unique(df_seabirds_env_data_MEOW$PROVINCE)

## A few final tidying up details ####

## Rename columns to English
colnames(df_seabirds_env_data_MEOW)[1] <- "date"
colnames(df_seabirds_env_data_MEOW)[2] <- "hour"
colnames(df_seabirds_env_data_MEOW)[3] <- "voyage"
colnames(df_seabirds_env_data_MEOW)[4] <- "seabird_count_type"

## Filter away 'Dados-fisicos' rows (== 'abiotic data') & 
## translate "seabird_count_type" to English 
df_seabirds_env_data_MEOW <-
  df_seabirds_env_data_MEOW %>% 
  dplyr::filter(! seabird_count_type == "Dados-fisicos")

df_seabirds_env_data_MEOW$seabird_count_type <-
  stringr::str_replace_all(df_seabirds_env_data_MEOW$seabird_count_type,
                           pattern = "Aves-seguidoras",
                           replacement = "ship_followers")

df_seabirds_env_data_MEOW$seabird_count_type <-
  stringr::str_replace_all(df_seabirds_env_data_MEOW$seabird_count_type,
                           pattern = "Continuo",
                           replacement = "continuous")

df_seabirds_env_data_MEOW$seabird_count_type <-
  stringr::str_replace_all(df_seabirds_env_data_MEOW$seabird_count_type,
                           pattern = "Instantaneo",
                           replacement = "snapshot")

df_seabirds_env_data_MEOW$seabird_count_type <-
  stringr::str_replace_all(df_seabirds_env_data_MEOW$seabird_count_type,
                           pattern = "Aves",
                           replacement = "point_count")

## Standard 'snakecase' column names
df_seabirds_env_data_MEOW <- janitor::clean_names(df_seabirds_env_data_MEOW)

### Grooming done - save it as a CSV ####

write.csv(df_seabirds_env_data_MEOW,
          "./data-processed/seabirds_env_MEOW_data.csv", 
          row.names = FALSE)
