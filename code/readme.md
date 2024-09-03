# Neotropical Plants

## Workflow

All directories are named for the data level, per guidelines from the [Environmental Data Initiative](https://edirepository.org/resources/thematic-standardization).
As illustrated in the workflow diagram below, the L0 code retrieves the raw data, the L1 code modifies and cleans the data, and the L2 code runs analyses with the data.

<img src="https://raw.githubusercontent.com/bioXgeo/neotropical_plants/master/Workflow_diversity_plants_frugivores.png"/>

## L0
The L0 subfolder contains scripts for Level-0 (raw data) analysis. This contains the following scripts:
- [L0_TropicalAndes_plants_BIEN_occ](https://github.com/bioXgeo/neotropical_plants/blob/master/code/L0/L0_TropicalAndes_plants_BIEN_occ.Rmd)): acquire BIEN plant occurrence data using BIEN R package
- [L0_TropicalAndes_plants_GBIF](https://github.com/bioXgeo/neotropical_plants/blob/master/code/L0/L0_TropicalAndes_plants_GBIF.Rmd): acquire GBIF plant occurrence data using rgbif R package
- [L0_TropicalAndes_frugivores_GBIF](https://github.com/bioXgeo/neotropical_plants/blob/master/code/L0/L0_TropicalAndes_frugivores_GBIF.Rmd): acquire GBIF frugivore occurrence data using rgbif R package
- [L0_TropicalAndes_plants_BIEN_traits](https://github.com/bioXgeo/neotropical_plants/blob/master/code/L0/L0_TropicalAndes_plants_BIEN_traits.Rmd): acquire BIEN plant trait data using BIEN R package
- [L0_TropicalAndes_plants_TRY](https://github.com/bioXgeo/neotropical_plants/blob/master/code/L0/L0_TropicalAndes_plants_TRY_traits.Rmd): acquire TRY plant trait data using rtry R package
- [L0_TropicalAndes_frugivores_frugivoria](https://github.com/bioXgeo/neotropical_plants/blob/master/code/L0/L0_TropicalAndes_frugivores_frugivoria_traits.Rmd): acquire Frugivoria frugivore trait data
- [L0_TropicalAndes_IUCNhabitatClassification](https://github.com/bioXgeo/neotropical_plants/blob/master/code/L0/L0_TropicalAndes_IUCNhabitatClassification.Rmd): get shapefile of Tropical Andes Forest IUCN designated areas


## L1
The L1 subfolder contains scripts for Level-1 analysis. This contains the following scripts:
- [L1_TropicalAndes_plants_BIEN_occ](): clean BIEN plant occurrence data
- [L1_TropicalAndes_plants_BIEN_traits](): clean BIEN plant trait data
- [L1_TropicalAndes_plants_GBIF](): clean GBIF plant occurrence data
- [L1_TropicalAndes_plants_TRY](): clean TRY plant trait data
- [L1_TropicalAndes_frugivores_GBIF](): clean GBIF frugivore occurrence data
- [L1_TropicalAndes_frugivores_frugivoria](): clean Frugivoria frugivore trait data
- [L1_TropicalAndes_plant_lookupTable](): create plant species lookup table
- [L1_TropicalAndes_frugivore_lookupTable](): create frugivore species lookup table


## L2
The L2 subfolder contains scripts for Level-2 analysis.
- [L2_1_TropicalAndes_plant_occ](): combine occurrence data from BIEN & GBIF
- [L2_1_TropicalAndes_plant_traits](): combine trait data from BIEN & TRY
- [L2_2_TropicalAndes_taxdiv](): calculating taxonomic diversity
- [L2_2_TropicalAndes_funcdiv](): calculating functional diversity
- [L2_3_TropicalAndes_divRelationships]():
- 





_This readme last modified by HJA 25 Jul 2023_
