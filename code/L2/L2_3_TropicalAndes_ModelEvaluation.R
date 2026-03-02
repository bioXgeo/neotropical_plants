#title: "Tropical Andes functional and taxonomic diversity model comparison"
#author: "Jenna B. Baljunas, Hazel J. Anderson"
#project: "Plant-Frugivore Diversity"
#collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske"
#overview: "This script compares FD and TD models."
#date: "2024-05-16; 2025-12-19"
#data input: "compare_MPrichness_100km.rds", "compare_MPrichness_75km.rds", "compare_MPrichness_50km.rds", "compare_MPrichness_25km.rds", "compare_MPrichness_10km.rds", "compare_MPrichness_5km.rds", "compare_BPrichness_100km.rds", "compare_BPrichness_75km.rds", "compare_BPrichness_50km.rds", "compare_BPrichness_25km.rds", "compare_BPrichness_10km.rds", "compare_BPrichness_5km.rds", "compare_MPFDis_100km.rds", "compare_MPFDis_75km.rds", "compare_MPFDis_50km.rds", "compare_MPFDis_25km.rds", "compare_MPFDis_10km.rds", "compare_MPFDis_5km.rds", "compare_BPFDis_100km.rds", "compare_BPFDis_75km.rds", "compare_BPFDis_50km.rds", "compare_BPFDis_25km.rds", "compare_BPFDis_10km.rds", "compare_BPFDis_5km.rds"
#data output: "LM_comparison_r2.png", "LM_comparison_sc.png", "r2_sc_comparison.png"
#notes: JB used HPCC


# load required packages
library(mFD); library(sf); library(dplyr); library(ggplot2); library(rnaturalearth); library(ggspatial); library(rlang); library(doParallel); library(foreach); library(purrr); library(ggpubr); library(patchwork)


# set file paths
data_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
output_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2')
figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures')

# #HPCC
# data_path_L1 <- file.path('/mnt/research/nasabio/data_2025/plants/L1')
# output_path_L2 <- file.path('/mnt/research/nasabio/data_2025/plants/L2')
# figure_path <- file.path('/mnt/research/nasabio/data_2025/plants/figures')


# load functions
source("C:/GitHub_projects/plant-frugivore diversity/neotropical_plants/code/Functions.R")

## HPCC
# source("/mnt/ffs24/home/baljunas/Documents/neotropical_plants/code/Functions.R")


# diversity comparison linear model information

#### taxonomic diversity ####

# mammals
compare_MPrichness_100km <- readRDS(file = file.path(output_path_L2,"compare_MPrichness_100km.rds"))
compare_MPrichness_75km <- readRDS(file = file.path(output_path_L2,"compare_MPrichness_75km.rds"))
compare_MPrichness_50km <- readRDS(file = file.path(output_path_L2,"compare_MPrichness_50km.rds"))
compare_MPrichness_25km <- readRDS(file = file.path(output_path_L2,"compare_MPrichness_25km.rds"))
compare_MPrichness_10km <- readRDS(file = file.path(output_path_L2,"compare_MPrichness_10km.rds"))
compare_MPrichness_5km <- readRDS(file = file.path(output_path_L2,"compare_MPrichness_5km.rds"))

# birds 
compare_BPrichness_100km <- readRDS(file = file.path(output_path_L2,"compare_BPrichness_100km.rds"))
compare_BPrichness_75km <- readRDS(file = file.path(output_path_L2,"compare_BPrichness_75km.rds"))
compare_BPrichness_50km <- readRDS(file = file.path(output_path_L2,"compare_BPrichness_50km.rds"))
compare_BPrichness_25km <- readRDS(file = file.path(output_path_L2,"compare_BPrichness_25km.rds"))
compare_BPrichness_10km <- readRDS(file = file.path(output_path_L2,"compare_BPrichness_10km.rds"))
compare_BPrichness_5km <- readRDS(file = file.path(output_path_L2,"compare_BPrichness_5km.rds"))


#### functional diversity ####
# mammals
compare_MPFDis_100km <- readRDS(file = file.path(output_path_L2,"compare_MPFDis_100km.rds"))
compare_MPFDis_75km <- readRDS(file = file.path(output_path_L2,"compare_MPFDis_75km.rds"))
compare_MPFDis_50km <- readRDS(file = file.path(output_path_L2,"compare_MPFDis_50km.rds"))
compare_MPFDis_25km <- readRDS(file = file.path(output_path_L2,"compare_MPFDis_25km.rds"))
compare_MPFDis_10km <- readRDS(file = file.path(output_path_L2,"compare_MPFDis_10km.rds"))
compare_MPFDis_5km <- readRDS(file = file.path(output_path_L2,"compare_MPFDis_5km.rds"))

# birds 
compare_BPFDis_100km <- readRDS(file = file.path(output_path_L2,"compare_BPFDis_100km.rds"))
compare_BPFDis_75km <- readRDS(file = file.path(output_path_L2,"compare_BPFDis_75km.rds"))
compare_BPFDis_50km <- readRDS(file = file.path(output_path_L2,"compare_BPFDis_50km.rds"))
compare_BPFDis_25km <- readRDS(file = file.path(output_path_L2,"compare_BPFDis_25km.rds"))
compare_BPFDis_10km <- readRDS(file = file.path(output_path_L2,"compare_BPFDis_10km.rds"))
compare_BPFDis_5km <- readRDS(file = file.path(output_path_L2,"compare_BPFDis_5km.rds"))


#### compare the linear model r2 and slope coefficents for taxonomic and functional diversity ####

# extract r2s

# richness
compare_MPrichness_100km_r2 <- compare_MPrichness_100km$R.squared
compare_MPrichness_75km_r2 <- compare_MPrichness_75km$R.squared
compare_MPrichness_50km_r2 <- compare_MPrichness_50km$R.squared
compare_MPrichness_25km_r2 <- compare_MPrichness_25km$R.squared
compare_MPrichness_10km_r2 <- compare_MPrichness_10km$R.squared
compare_MPrichness_5km_r2 <- compare_MPrichness_5km$R.squared

compare_BPrichness_100km_r2 <- compare_BPrichness_100km$R.squared
compare_BPrichness_75km_r2 <- compare_BPrichness_75km$R.squared
compare_BPrichness_50km_r2 <- compare_BPrichness_50km$R.squared
compare_BPrichness_25km_r2 <- compare_BPrichness_25km$R.squared
compare_BPrichness_10km_r2 <- compare_BPrichness_10km$R.squared
compare_BPrichness_5km_r2 <- compare_BPrichness_5km$R.squared

# FDis
compare_MPFDis_100km_r2 <- compare_MPFDis_100km$R.squared
compare_MPFDis_75km_r2 <- compare_MPFDis_75km$R.squared
compare_MPFDis_50km_r2 <- compare_MPFDis_50km$R.squared
compare_MPFDis_25km_r2 <- compare_MPFDis_25km$R.squared
compare_MPFDis_10km_r2 <- compare_MPFDis_10km$R.squared
compare_MPFDis_5km_r2 <- compare_MPFDis_5km$R.squared

compare_BPFDis_100km_r2 <- compare_BPFDis_100km$R.squared
compare_BPFDis_75km_r2 <- compare_BPFDis_75km$R.squared
compare_BPFDis_50km_r2 <- compare_BPFDis_50km$R.squared
compare_BPFDis_25km_r2 <- compare_BPFDis_25km$R.squared
compare_BPFDis_10km_r2 <- compare_BPFDis_10km$R.squared
compare_BPFDis_5km_r2 <- compare_BPFDis_5km$R.squared


# Create a tibble
bothdiv_r2_compare <- tibble(
  resolution = c(5, 10, 25, 50, 75, 100),
  taxdiv_r2_linear_mammal = c(
    as.numeric(compare_MPrichness_5km_r2), 
    as.numeric(compare_MPrichness_10km_r2), 
    as.numeric(compare_MPrichness_25km_r2), 
    as.numeric(compare_MPrichness_50km_r2), 
    as.numeric(compare_MPrichness_75km_r2), 
    as.numeric(compare_MPrichness_100km_r2)
  ),
  taxdiv_r2_linear_bird = c(
    as.numeric(compare_BPrichness_5km_r2), 
    as.numeric(compare_BPrichness_10km_r2), 
    as.numeric(compare_BPrichness_25km_r2), 
    as.numeric(compare_BPrichness_50km_r2), 
    as.numeric(compare_BPrichness_75km_r2), 
    as.numeric(compare_BPrichness_100km_r2)
  ),
  fundiv_r2_linear_mammal = c(
    as.numeric(compare_MPFDis_5km_r2), 
    as.numeric(compare_MPFDis_10km_r2), 
    as.numeric(compare_MPFDis_25km_r2), 
    as.numeric(compare_MPFDis_50km_r2), 
    as.numeric(compare_MPFDis_75km_r2), 
    as.numeric(compare_MPFDis_100km_r2)
  ),
  fundiv_r2_linear_bird = c(
    as.numeric(compare_BPFDis_5km_r2), 
    as.numeric(compare_BPFDis_10km_r2), 
    as.numeric(compare_BPFDis_25km_r2), 
    as.numeric(compare_BPFDis_50km_r2), 
    as.numeric(compare_BPFDis_75km_r2), 
    as.numeric(compare_BPFDis_100km_r2)
  )
)

bothdiv_r2_long <- bothdiv_r2_compare %>%
  pivot_longer(cols = c(taxdiv_r2_linear_mammal, taxdiv_r2_linear_bird, fundiv_r2_linear_mammal, fundiv_r2_linear_bird), 
               names_to = "type", 
               values_to = "r_squared")

r2 <- ggplot(bothdiv_r2_long, aes(x=resolution, y=r_squared, color=type, linetype=type))+
  geom_line()+
  scale_color_manual(values=c('lightsteelblue2','burlywood3','lightsteelblue2','burlywood3'), labels=c('Bird FDis','Mammal FDis','Bird S','Mammal S'))+
  scale_linetype_manual(values=c('solid','solid','dashed','dashed'))+
  geom_point(size=3)+
  scale_x_continuous(breaks=c(5,10,25,50,75,100))+
  labs(color='Linear model', x='Resolution', y=expression('R'^2))+
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid", "dashed", "dashed"), shape = NA)), linetype = "none") +
  theme_classic() +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12), legend.title = element_text(size = 16), legend.text = element_text(size = 12))
ggsave('LM_comparison_r2.png', r2, path = figure_path, width = 8, height = 6, units = "in", dpi=1000)


# extract slope coefficients

# richness
compare_MPrichness_100km_sc <- compare_MPrichness_100km$parameter$a
compare_MPrichness_75km_sc <- compare_MPrichness_75km$parameter$a
compare_MPrichness_50km_sc <- compare_MPrichness_50km$parameter$a
compare_MPrichness_25km_sc <- compare_MPrichness_25km$parameter$a
compare_MPrichness_10km_sc <- compare_MPrichness_10km$parameter$a
compare_MPrichness_5km_sc <- compare_MPrichness_5km$parameter$a

compare_BPrichness_100km_sc <- compare_BPrichness_100km$parameter$a
compare_BPrichness_75km_sc <- compare_BPrichness_75km$parameter$a
compare_BPrichness_50km_sc <- compare_BPrichness_50km$parameter$a
compare_BPrichness_25km_sc <- compare_BPrichness_25km$parameter$a
compare_BPrichness_10km_sc <- compare_BPrichness_10km$parameter$a
compare_BPrichness_5km_sc <- compare_BPrichness_5km$parameter$a

# FDis
compare_MPFDis_100km_sc <- compare_MPFDis_100km$parameter$a
compare_MPFDis_75km_sc <- compare_MPFDis_75km$parameter$a
compare_MPFDis_50km_sc <- compare_MPFDis_50km$parameter$a
compare_MPFDis_25km_sc <- compare_MPFDis_25km$parameter$a
compare_MPFDis_10km_sc <- compare_MPFDis_10km$parameter$a
compare_MPFDis_5km_sc <- compare_MPFDis_5km$parameter$a

compare_BPFDis_100km_sc <- compare_BPFDis_100km$parameter$a
compare_BPFDis_75km_sc <- compare_BPFDis_75km$parameter$a
compare_BPFDis_50km_sc <- compare_BPFDis_50km$parameter$a
compare_BPFDis_25km_sc <- compare_BPFDis_25km$parameter$a
compare_BPFDis_10km_sc <- compare_BPFDis_10km$parameter$a
compare_BPFDis_5km_sc <- compare_BPFDis_5km$parameter$a


# Create a tibble
bothdiv_sc_compare <- tibble(
  resolution = c(5, 10, 25, 50, 75, 100),
  taxdiv_sc_linear_mammal = c(
    as.numeric(compare_MPrichness_5km_sc), 
    as.numeric(compare_MPrichness_10km_sc), 
    as.numeric(compare_MPrichness_25km_sc), 
    as.numeric(compare_MPrichness_50km_sc), 
    as.numeric(compare_MPrichness_75km_sc), 
    as.numeric(compare_MPrichness_100km_sc)
  ),
  taxdiv_sc_linear_bird = c(
    as.numeric(compare_BPrichness_5km_sc), 
    as.numeric(compare_BPrichness_10km_sc), 
    as.numeric(compare_BPrichness_25km_sc), 
    as.numeric(compare_BPrichness_50km_sc), 
    as.numeric(compare_BPrichness_75km_sc), 
    as.numeric(compare_BPrichness_100km_sc)
  ),
  fundiv_sc_linear_mammal = c(
    as.numeric(compare_MPFDis_5km_sc), 
    as.numeric(compare_MPFDis_10km_sc), 
    as.numeric(compare_MPFDis_25km_sc), 
    as.numeric(compare_MPFDis_50km_sc), 
    as.numeric(compare_MPFDis_75km_sc), 
    as.numeric(compare_MPFDis_100km_sc)
  ),
  fundiv_sc_linear_bird = c(
    as.numeric(compare_BPFDis_5km_sc), 
    as.numeric(compare_BPFDis_10km_sc), 
    as.numeric(compare_BPFDis_25km_sc), 
    as.numeric(compare_BPFDis_50km_sc), 
    as.numeric(compare_BPFDis_75km_sc), 
    as.numeric(compare_BPFDis_100km_sc)
  )
)

bothdiv_sc_long <- bothdiv_sc_compare %>%
  pivot_longer(cols = c(taxdiv_sc_linear_mammal, taxdiv_sc_linear_bird, fundiv_sc_linear_mammal, fundiv_sc_linear_bird), 
               names_to = "type", 
               values_to = "slope_coefficient")

sc <- ggplot(bothdiv_sc_long, aes(x=resolution, y=slope_coefficient, color=type, linetype=type))+
  geom_line()+
  scale_color_manual(values=c('lightsteelblue2','burlywood3','lightsteelblue2','burlywood3'), labels=c('Bird FDis','Mammal FDis','Bird S','Mammal S'))+
  scale_linetype_manual(values=c('solid','solid','dashed','dashed'))+
  geom_point(size=3)+
  scale_x_continuous(breaks=c(5,10,25,50,75,100))+
  labs(color='Linear model', x='Resolution', y='Slope coefficient')+
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid", "dashed", "dashed"), shape = NA)), linetype = "none") +
  theme_classic() +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12), legend.title = element_text(size = 16), legend.text = element_text(size = 12))
ggsave('LM_comparison_sc.png', sc, path = figure_path, width = 8, height = 6, units = "in", dpi=1000)

wrap_plots(r2, sc) + plot_layout(guides='collect', axis_titles = 'collect')
ggsave('r2_sc_comparison.png', path = figure_path, width = 10, height = 6, units = "in", dpi=1000)
