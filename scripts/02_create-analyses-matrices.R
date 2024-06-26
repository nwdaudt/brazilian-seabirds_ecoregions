##
## Create analyses matrices
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## Libraries ####
library(dplyr)
library(tidyr)

## Read data ####
all_data <- read.csv("./data-processed/seabirds_env_MEOW_data.csv")

## Abundance and CAP matrices ####
# Based on 'continuous count' method only

abund_mat <- 
  dplyr::filter(all_data, seabird_count_type == "continuous")

# hist(abund_mat$total_abund, breaks = 100)

## See Methods in the main paper, but for CAP we will only use samples with
# at least [3 or 5] individuals recorded
cap_n3 <- 
  abund_mat %>% dplyr::filter(total_abund >= 3)

cap_n5 <- 
  abund_mat %>% dplyr::filter(total_abund >= 5)

## Occurrence matrix ####
# Based on all four count methods: 
# ship-following, continuous, snapshots, and point-counts

## Create an index for each sample ("g")
occurrence_mat <- 
  all_data %>% 
  dplyr::group_by(date, hour) %>% 
  {dplyr::mutate(ungroup(.), g = group_indices(.))} %>% 
  as.data.frame()

## Remove "sp.", "families", and already summarized cols with "abundance" data
occurrence_mat <- 
  occurrence_mat %>% 
  dplyr::select(- ends_with(c("_sp", "dae", "_abund")))

## Subset environmental data, trip description, and "g"
abiotic_cols <- c("g", "date","hour", "voyage", "latitude",
                  "longitude", "beaufort", "dist_coast", "bathymetry", "chl",
                  "sss", "sst", "d_sst", "ecoregion", "province")

occ_env <- 
  occurrence_mat %>% 
  dplyr::group_by(g) %>% 
  dplyr::distinct(date, hour, .keep_all = TRUE) %>% 
  dplyr::select(all_of(abiotic_cols)) %>% 
  dplyr::ungroup()

## Subset species and "g"; also transform it to presence/absence (1/0)
sp_only_cols <- 
  colnames(occurrence_mat)[! colnames(occurrence_mat) %in% abiotic_cols][-1]
                                                                      ## 1 == "seabird_count_type"

occ_sp <- 
  occurrence_mat %>% 
  dplyr::select(g, all_of(sp_only_cols)) %>% 
  tidyr::pivot_longer(cols = all_of(sp_only_cols),
                      names_to = "species",
                      values_to = "count") %>% 
  dplyr::group_by(g, species) %>% 
  dplyr::summarise(count = sum(count)) %>% 
  dplyr::ungroup(.) %>% 
  dplyr::mutate(count = replace(count, count >= 1, 1)) %>% 
  tidyr::pivot_wider(id_cols = g,
                     names_from = species,
                     values_from = count)

## For the iNEXT analysis, we didn't use 'point-counts' 
# (from the 'Foz do Amazonas' voyage) -- so remove them
point_counts_g <- 
  occurrence_mat %>% 
  dplyr::filter(occurrence_mat$seabird_count_type == 'point_count')

occ_iNEXT <- 
  dplyr::anti_join(occurrence_mat, point_counts_g, by = "g")

occ_iNEXT <- 
  occ_iNEXT %>% 
  dplyr::select(g, all_of(sp_only_cols)) %>% 
  tidyr::pivot_longer(cols = all_of(sp_only_cols),
                      names_to = "species",
                      values_to = "count") %>% 
  dplyr::group_by(g, species) %>% 
  dplyr::summarise(count = sum(count)) %>% 
  dplyr::ungroup(.) %>% 
  dplyr::mutate(count = replace(count, count >= 1, 1)) %>% 
  tidyr::pivot_wider(id_cols = g,
                     names_from = species,
                     values_from = count)

## Join "occ_env" and "occ_sp" -- final Occurrence matrix ('total')
occurrence_mat <- 
  dplyr::left_join(occ_env, occ_sp, by = "g") %>% 
  dplyr::mutate(sp_richness = rowSums(occ_sp[, c(2:ncol(occ_sp))]),
                .after = province)

## Join "occ_env" and "occ_iNEXT" -- final Occurrence matrix (for 'iNEXT')
occurrence_mat_iNEXT <- 
  dplyr::left_join(occ_env, occ_iNEXT, by = "g") %>% 
  dplyr::filter(! is.na(anous_stolidus)) %>% 
  dplyr::mutate(sp_richness = rowSums(occ_iNEXT[, c(2:ncol(occ_iNEXT))]),
                .after = province)

## Save them all ####

write.csv(abund_mat, file = "./data-processed/abund.csv", row.names = FALSE)
write.csv(cap_n3, file = "./data-processed/cap_n3.csv", row.names = FALSE)
write.csv(cap_n5, file = "./data-processed/cap_n5.csv", row.names = FALSE)
write.csv(occurrence_mat, file = "./data-processed/occ.csv", row.names = FALSE)
write.csv(occurrence_mat_iNEXT, file = "./data-processed/occ_iNEXT.csv", row.names = FALSE)
