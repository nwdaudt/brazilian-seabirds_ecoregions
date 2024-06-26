##
## Hierarchical cluster and CAP
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## Libraries ####
library(dplyr)
library(tidyr)
library(tibble)
library(labdsv)
library(BiodiversityR)

## Read the data ####
data <- read.csv("./data-processed/occ.csv")
cap_n3 <- read.csv("./data-processed/cap_n3.csv")

# cap_n5 <- read.csv("./data-processed/cap_n5.csv")
## >> with 'at least 5 birds counted' (n5) there are no 
## >> Amazonia/North Brazil Shelf samples -- so we won't use it...

species_cols <- colnames(data)[17:ncol(data)]

## Hierarchical clusters ####

## Provinces
PROV_cluster <- 
  data.frame(data[, colnames(data) %in% c("province", species_cols)]) %>% 
  dplyr::arrange(province) %>% 
  tidyr::pivot_longer(cols = all_of(species_cols),
                      names_to = "species",
                      values_to = "pres_abs") %>% 
  dplyr::group_by(province, species) %>% 
  dplyr::summarise(sum_pres_abs = sum(pres_abs, na.rm = TRUE)) %>% 
  dplyr::mutate(sum_pres_abs = replace(sum_pres_abs, sum_pres_abs >= 1, 1)) %>% 
  tidyr::pivot_wider(id_cols = "province",
                     names_from = "species",
                     values_from = "sum_pres_abs",
                     values_fill = 0) %>% 
  tibble::column_to_rownames(var = "province")

PROV_cluster <- 
  PROV_cluster %>% 
  labdsv::dsvdis(as.matrix(.), index = "sorensen") %>% 
  stats::hclust(method = "single")

# pdf("./results/hcluster_province.pdf", height = 9, width = 8)
# plot(PROV_cluster, 
#      cex = 1.8, cex.axis = 1.5, lwd = 1.8, 
#      xlab = "", ylab = "", main = "", sub = "")
# dev.off()

## Ecoregions
ECO_cluster <- 
  data.frame(data[, colnames(data) %in% c("ecoregion", species_cols)]) %>% 
  dplyr::arrange(ecoregion) %>% 
  tidyr::pivot_longer(cols = all_of(species_cols),
                      names_to = "species",
                      values_to = "pres_abs") %>% 
  dplyr::group_by(ecoregion, species) %>% 
  dplyr::summarise(sum_pres_abs = sum(pres_abs, na.rm = TRUE)) %>% 
  dplyr::mutate(sum_pres_abs = replace(sum_pres_abs, sum_pres_abs >= 1, 1)) %>% 
  tidyr::pivot_wider(id_cols = "ecoregion",
                     names_from = "species",
                     values_from = "sum_pres_abs",
                     values_fill = 0) %>% 
  tibble::column_to_rownames(var = "ecoregion")

ECO_cluster <- 
  ECO_cluster %>% 
  labdsv::dsvdis(as.matrix(.), index = "sorensen") %>% 
  stats::hclust(method = "single")

# pdf("./results/hcluster_ecoregion.pdf", height = 9, width = 8)
# plot(ECO_cluster, 
#      cex = 1.8, cex.axis = 1.5, lwd = 1.8, 
#      xlab = "", ylab = "", main = "", sub = "")
# dev.off()

pdf("./results/hcluster.pdf", height = 18, width = 8)
par(mfrow = c(2, 1))
plot(ECO_cluster, 
     cex = 1.8, cex.axis = 1.5, lwd = 1.8, 
     xlab = "", ylab = "", main = "", sub = "")

plot(PROV_cluster, 
     cex = 1.8, cex.axis = 1.5, lwd = 1.8, 
     xlab = "", ylab = "", main = "", sub = "")
dev.off()

## CAP ####

spp_cols <- colnames(cap_n3)[8:67]
env_cols <- colnames(cap_n3)[80:85]

## Seabird matrix
cap_seabirds <- 
  cap_n3 %>% 
  dplyr::select(all_of(spp_cols)) %>% 
  as.data.frame()

# Keep just columns with records
cap_seabirds <- cap_seabirds[, -colSums(cap_seabirds) != 0]

## Environmental + Ecoregion/Province matrix
cap_env_PROV <- 
  cap_n3 %>% 
  dplyr::select(all_of(env_cols), province) %>%
  as.data.frame()

cap_env_PROV$province <- as.factor(cap_env_PROV$province)

cap_env_ECO <- 
  cap_n3 %>% 
  dplyr::select(all_of(env_cols), ecoregion) %>%
  as.data.frame()

cap_env_ECO$ecoregion <- as.factor(cap_env_ECO$ecoregion)

## ---- CAP Provinces ---- ##

cap_PROV <- 
  BiodiversityR::CAPdiscrim(cap_seabirds ~ province, data = cap_env_PROV, 
                            dist = "bray", axes = 2, m = 0, add = FALSE)

# [R prompt regarding LDA]
# Overall classification success (m=8) : 90.3553299492386 percent
# North Brazil Shelf (n=5) correct: 0 percent
# Tropical Southwestern Atlantic (n=113) correct: 85.8407079646018 percent
# Warm Temperate Southwestern Atlantic (n=276) correct: 93.8405797101449 percent

# pdf("./results/CAP_province.pdf", height = 7, width = 7)
# plot2 <- vegan::ordiplot(cap_PROV, type = "none", 
#                          xlim = c(-5, 4), ylim = c(-2.5, 4.5))
# 
# cap_PROV_plot <- BiodiversityR::ordisymbol(plot2, cap_env_PROV , "province", 
#                                            legend = TRUE)
# dev.off()

## ---- CAP Ecoregion ---- ##

cap_ECO <- 
  BiodiversityR::CAPdiscrim(cap_seabirds ~ ecoregion, data = cap_env_ECO, 
                            dist = "bray", axes = 2, m = 0, add = FALSE)

# [R prompt regarding LDA]
# Overall classification success (m=10) : 67.51269035533 percent
# Amazonia (n=5) correct: 0 percent
# Eastern Brazil (n=41) correct: 70.7317073170732 percent
# Northeastern Brazil (n=10) correct: 80 percent
# Rio Grande (n=201) correct: 81.0945273631841 percent
# Southeastern Brazil (n=75) correct: 26.6666666666667 percent
# Trindade and Martin Vaz Islands (n=62) correct: 74.1935483870968 percent

# pdf("./results/CAP_ecoregion.pdf", height = 7, width = 7)
# plot1 <- vegan::ordiplot(cap_ECO, type = "none", 
#                          xlim = c(-4, 6.5), ylim = c(-4, 6))
# 
# cap_ECO_plot <- BiodiversityR::ordisymbol(plot1, cap_env_ECO , "ecoregion", 
#                                           legend = TRUE, legend.x = "topleft")
# dev.off()


pdf("./results/CAP.pdf", height = 8, width = 16)
par(mfrow = c(1, 2))

plot2 <- vegan::ordiplot(cap_PROV, type = "none", 
                         xlim = c(-5, 4), ylim = c(-2.5, 4.5))

cap_PROV_plot <- BiodiversityR::ordisymbol(plot2, cap_env_PROV , "province", 
                                           legend = TRUE)

plot1 <- vegan::ordiplot(cap_ECO, type = "none", 
                         xlim = c(-4, 6.5), ylim = c(-4, 6))

cap_ECO_plot <- BiodiversityR::ordisymbol(plot1, cap_env_ECO , "ecoregion", 
                                          legend = TRUE, legend.x = "topleft")

dev.off()