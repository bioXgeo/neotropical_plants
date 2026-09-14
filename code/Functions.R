# Functions used for "The Scale-Dependent Relationships of Fruiting Plants and Frugivores in Tropical Andes Forests"


#### Data summary function used throughout scripts ####

data_summary <- function(records, species, genera, families){
  num_records <- nrow(records)
  num_species <- length(unique(species))
  num_genera <- length(unique(genera))
  num_families <- length(unique(families))
  return(cat("The number of records is", num_records, "\n", "The number of species is", num_species, "\n","The number of genera is", num_genera, "\n", "The number of families is", num_families))
}


#### L0_TropicalAndes_frugivores_frugivoria.R ####
# fix column classes for some columns in Frugivoria

col_class <- function(x) {
  # convert NA strings
  x[x == "NA" | x == "NA "] <- NA

  # extract cols and categorize them into what they should be
  cols <- colnames(x)

  numeric_cols <- c(
    "diet_breadth",
    "body_mass_e",
    "body_size_mm",
    "longevity",
    "home_range_size",
    "generation_time",
    "habitat_breadth",
    "mean_CHELSA_bio1_1981.2010_V.2.1",
    "mean_CHELSA_bio12_1981.2010_V.2.1",
    "mean_human_fp_range_2010",
    "mean_human_fp_range_2020",
    "percent_change_hf_2010_2020",
    "inferred_range_sqkm",
    "for_strat_ground_e",
    "for_strat_understory_e",
    "for_strat_midhigh_e",
    "for_strat_canopy_e",
    "for_strat_aerial_e"
  )

  factor_cols <- setdiff(cols, numeric_cols)

  # convert to numeric
  for (col in numeric_cols) {
    if (col %in% names(x)) {
      if (is.factor(x[[col]])) {
        x[[col]] <- as.numeric(levels(x[[col]]))[as.integer(x[[col]])]
      } else if (is.character(x[[col]])) {
        x[[col]] <- as.numeric(x[[col]])
      }
    }
  }

  # convert to factor
  for (col in factor_cols) {
    if (col %in% names(x) && !is.factor(x[[col]])) {
      x[[col]] <- as.factor(x[[col]])
    }
  }

  return(x)
}



#### L1_P4_TropicalAndes_plant_traits.R ####
# standardize trait measurements

# Data summary
data_summary <- function(records, species, genera, families) {
  num_records <- nrow(records)
  num_species <- length(unique(species))
  num_genera <- length(unique(genera))
  num_families <- length(unique(families))
  return(cat(
    "The number of records is",
    num_records,
    "\n",
    "The number of species is",
    num_species,
    "\n",
    "The number of genera is",
    num_genera,
    "\n",
    "The number of families is",
    num_families
  ))
}


#### Standardize trait measurements ####

numeric_data_average <- function(data, unit, traitname) {
  # Remove rows if TraitValue is non-numeric and convert to numeric
  data <- data %>%
    filter(!is.na(as.numeric(TraitValue))) %>%
    mutate(TraitValue = as.numeric(TraitValue))

  # Remove duplicate rows
  data <- data %>%
    distinct(.keep_all = TRUE)

  # Remove rows with missing or empty species names
  data <- data[!is.na(data$Accepted_species) & data$Accepted_species != "", ]

  # Calculate geometic mean value, count, and variance per trait per species
  summary_data <- data %>%
    group_by(Accepted_species) %>%
    summarise(
      TraitValue_mean = exp(mean(log(TraitValue))),
      records_used = n(),
      variance = var(TraitValue, na.rm = TRUE)
    )

  # Add trait name column
  summary_data$TraitName <- traitname

  # Rename the mean column to TraitValue
  summary_data <- summary_data %>%
    rename(TraitValue = TraitValue_mean)

  return(summary_data)
}


factor_data_merge <- function(data, traitname) {
  # Remove rows with missing or empty species names
  data <- data[!is.na(data$Accepted_species) & data$Accepted_species != "", ]

  # Subset data to just species and TraitValue
  data <- data[, c("Accepted_species", "TraitValue")]

  # Calculate mode, count, and variance-like measure (using mode frequency)
  summary_data <- data %>%
    group_by(Accepted_species) %>%
    summarise(
      ModeValue = names(sort(table(TraitValue), decreasing = TRUE))[1],
      records_used = n(),
      mode_freq = max(table(TraitValue)),
      variance = 1 - (max(table(TraitValue)) / n())
    )

  # Add trait name column
  summary_data$TraitName <- traitname

  # Rename the mean column to TraitValue
  summary_data <- summary_data %>%
    rename(TraitValue = ModeValue)

  return(summary_data)
}


# detect outliers and report species with multiple entries per trait

detect_outliers <- function(data) {
  outlier_results <- data %>%
    group_by(Accepted_species) %>%
    filter(n() >= 3) %>%
    nest() %>%
    mutate(
      outliers = map(data, ~ identify_outliers(.x, variable = "TraitValue"))
    ) %>%
    unnest(outliers) %>%
    select(Accepted_species, TraitName, TraitValue, is.outlier, is.extreme) %>%
    ungroup()

  return(outlier_results)
}

# get the species with outliers

species_with_outliers <- function(data) {
  outliers_by_species <- data %>%
    group_by(Accepted_species) %>%
    summarize(outlier_count = sum(is.outlier, na.rm = TRUE)) %>%
    filter(outlier_count > 0) %>%
    ungroup()

  return(outliers_by_species)
}

summarize_species_records <- function(df) {
  # Count the total number of species
  total_species <- df %>%
    distinct(Accepted_species) %>%
    nrow()

  # Count the number of species with only one trait record
  species_with_one_record <- df %>%
    group_by(Accepted_species) %>%
    summarize(record_count = n()) %>%
    filter(record_count == 1) %>%
    nrow()

  # Count the number of species with more than three records
  species_with_more_than_three_records <- df %>%
    group_by(Accepted_species) %>%
    summarize(record_count = n()) %>%
    filter(record_count > 3) %>%
    nrow()

  # Print the results
  cat("Total number of species:", total_species, "\n")
  cat(
    "Number of species with only one trait record:",
    species_with_one_record,
    "\n"
  )
  cat(
    "Number of species with more than three records:",
    species_with_more_than_three_records,
    "\n"
  )
}

# function to combine trait dataframes only matching columns
combine_matching_columns <- function(df_list) {
  # Only use specified columns
  common_cols <- Reduce(intersect, lapply(df_list, colnames))

  # Ensure TraitValue column is of the same type across all data frames
  df_list <- lapply(df_list, function(df) {
    if ("TraitValue" %in% colnames(df)) {
      df <- df %>% mutate(TraitValue = as.character(TraitValue))
    }
    return(df)
  })

  # Filter each data frame to keep only the common columns
  df_list_filtered <- lapply(df_list, function(df) {
    df %>% select(all_of(common_cols))
  })

  # Combine the data frames using reduce and full_join
  combined_df <- reduce(df_list_filtered, full_join, by = common_cols)

  return(combined_df)
}


#### L1_P5_TropicalAndes_plant_imputetraits.R ####
# function to retrieve taxonomic information for a chunk of species names
get_taxonomic_info_chunk_names <- function(chunk_species_names) {
  # Initialize an empty list to store taxonomic information for each chunk
  chunk_taxonomic_info <- list()

  # Loop through each species name in the chunk and retrieve taxonomic information
  for (species_name in chunk_species_names) {
    tryCatch(
      {
        # Add a delay between consecutive API requests
        Sys.sleep(1)

        # Make API request to retrieve taxonomic information
        taxon_info <- tax_name(
          species_name,
          get = c("genus", "family"),
          db = "ncbi"
        )

        # Store taxonomic information for the species in the list
        chunk_taxonomic_info[[species_name]] <- data.frame(
          Species = species_name,
          Genus = taxon_info$genus,
          Family = taxon_info$family
        )
      },
      error = function(e) {
        # Print error message
        cat(
          "Error retrieving taxonomic information for",
          species_name,
          ":",
          conditionMessage(e),
          "\n"
        )
      }
    )
  }

  # Return the list of taxonomic information for the chunk
  return(chunk_taxonomic_info)
}

# function to retrieve taxonomic information for a chunk of taxon IDs
get_taxonomic_info_chunk_powo <- function(chunk_taxon_id) {
  # Initialize an empty list to store taxonomic information for each chunk
  chunk_taxonomic_info <- list()

  # Loop through each taxon ID in the chunk and retrieve taxonomic information
  for (taxon_id in chunk_taxon_id) {
    tryCatch(
      {
        # Print taxon ID for debugging
        print(paste("Processing taxon ID:", taxon_id))

        # Add a delay between consecutive API requests
        Sys.sleep(1)

        # Query POWO database for taxonomic information
        taxon_info <- pow_lookup(id = taxon_id)

        # Check if taxon_info is NULL
        if (is.null(taxon_info)) {
          # If taxon_info is NULL, return NA values for species, family, and genus
          chunk_taxonomic_info[[taxon_id]] <- data.frame(
            taxon_id = taxon_id,
            species = NA,
            family = NA,
            genus = NA
          )
        } else {
          # Store taxonomic information for the taxon ID in the list
          chunk_taxonomic_info[[taxon_id]] <- data.frame(
            taxon_id = taxon_id,
            species = taxon_info$meta$name,
            genus = taxon_info$meta$genus,
            family = taxon_info$meta$family
          )
        }
      },
      error = function(e) {
        # Print error message for debugging
        print(paste("Error processing taxon ID:", taxon_id))
        print(e)
      }
    )
  }

  # Return the list of taxonomic information for the chunk
  return(chunk_taxonomic_info)
}

# get a single trait value for each family/genus

# numeric traits
numeric_traits_combined <- function(df, level, traitname) {
  df$trait_value <- as.numeric(df$trait_value)
  if (level == "genus") {
    summary_data <- df %>%
      group_by(scrubbed_genus) %>%
      summarise(
        trait_value_mean = exp(mean(log(trait_value))),
        records_used = n(),
        variance = var(trait_value, na.rm = TRUE)
      )
    # Add trait name column
    summary_data$TraitName <- traitname

    # Rename the mean column to TraitValue
    summary_data <- summary_data %>%
      rename(TraitValue = trait_value_mean)

    return(summary_data)
  } else if (level == "family") {
    summary_data <- df %>%
      group_by(scrubbed_family) %>%
      summarise(
        trait_value_mean = exp(mean(log(trait_value))),
        records_used = n(),
        variance = var(trait_value, na.rm = TRUE)
      )
    # Add trait name column
    summary_data$TraitName <- traitname

    # Rename the mean column to TraitValue
    summary_data <- summary_data %>%
      rename(TraitValue = trait_value_mean)

    return(summary_data)
  }
}

# non-numeric traits
cat_traits_combined <- function(df, level, traitname) {
  if (level == "genus") {
    summary_data <- df %>%
      group_by(scrubbed_genus) %>%
      summarise(
        ModeValue = names(sort(table(trait_value), decreasing = TRUE))[1],
        records_used = n(),
        mode_freq = max(table(trait_value)),
        variance = 1 - (max(table(trait_value)) / n())
      )

    # Add trait name column
    summary_data$TraitName <- traitname

    # Rename the mean column to TraitValue
    summary_data <- summary_data %>%
      rename(TraitValue = ModeValue)

    return(summary_data)
  } else if (level == "family") {
    summary_data <- df %>%
      group_by(scrubbed_family) %>%
      summarise(
        ModeValue = names(sort(table(trait_value), decreasing = TRUE))[1],
        records_used = n(),
        mode_freq = max(table(trait_value)),
        variance = 1 - (max(table(trait_value)) / n())
      )

    # Add trait name column
    summary_data$TraitName <- traitname

    # Rename the mean column to TraitValue
    summary_data <- summary_data %>%
      rename(TraitValue = ModeValue)

    return(summary_data)
  }
}

#### is this function used?? ####
# use taxize for species with no family and genus info
library(taxize)

# function to retrieve taxonomic information for a chunk of species names
# use .Reviron to add NCBI ENTREZ API info
get_taxonomic_info_chunk <- function(chunk_species_names) {
  # Initialize an empty list to store taxonomic information for each chunk
  chunk_taxonomic_info <- list()

  # Loop through each species name in the chunk and retrieve taxonomic information
  for (species_name in chunk_species_names) {
    tryCatch(
      {
        # Add a delay between consecutive API requests
        Sys.sleep(1)

        # Make API request to retrieve taxonomic information
        taxon_info <- tax_name(
          species_name,
          get = c("genus", "family"),
          db = "ncbi"
        )

        # Store taxonomic information for the species in the list
        chunk_taxonomic_info[[species_name]] <- data.frame(
          Species = species_name,
          Genus = taxon_info$genus,
          Family = taxon_info$family
        )
      },
      error = function(e) {
        # Print error message
        cat(
          "Error retrieving taxonomic information for",
          species_name,
          ":",
          conditionMessage(e),
          "\n"
        )
      }
    )
  }

  # Return the list of taxonomic information for the chunk
  return(chunk_taxonomic_info)
}



#### L1_LastStep_DiversityInputObjects.R ####
# create cells within Tropical Andes Forest, create matrices of observations/species

obs_grid <- function(resolution_meters, species_sf) {
  # Make Grid
  TAGrid <- TApoly %>%
    st_make_grid(cellsize = c(resolution_meters)) %>%
    st_intersection(TropicalAndes_IUCNHabitat_Forest) %>%
    st_cast("MULTIPOLYGON") %>%
    st_sf() %>%
    mutate(cellid = paste0("cell_", row_number()))

  # Join with species data
  obs_grid <- TAGrid %>%
    st_intersects(species_sf, ., sparse = FALSE) %>%
    as.data.frame()

  colnames(obs_grid) <- st_drop_geometry(paste0(TAGrid$cellid))

  obs_grid <- obs_grid %>%
    bind_cols(species_sf, .) %>%
    st_drop_geometry()

  # get to one row per species with TRUE in any cell in which that species occurs
  obs_grid_clean <- obs_grid %>%
    group_by(species) %>%
    select(where(~ any(., na.rm = TRUE)))

  return(obs_grid_clean)
}

sp_grid <- function(obs_grid_clean) {
  comm <- obs_grid_clean %>%
    group_by(species) %>%
    summarise(
      across(everything(), ~ sum(., na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    column_to_rownames("species") %>%
    t() %>%
    as.matrix()

  return(comm)
}

# using iNEXT
calc_coverage <- function(sp_grid) {
  cell_list <- apply(sp_grid, 1, c)

  # out <- iNEXT(cell_list, q = 0, datatype = 'abundance')

  # DataInfo contains number of observations (n), observed species richness (S.obs), sample coverage (SC), number of singletons (f1), number of doubletons (f2)
  info <- DataInfo(cell_list)

  # distribution of sample coverage across data
  coverage_dist <- ggplot(info[info$n > 1, ], aes(SC)) +
    geom_histogram(bins = 30) +
    theme_classic()

  # relationship between sample coverage and number of observations
  coverage_by_obs <- ggplot(info, aes(n, SC)) +
    geom_point() +
    scale_x_log10() +
    theme_classic()

  return(list(
    #iNEXT_output = out,
    iNEXT_calcs = info,
    coverage_dist = coverage_dist,
    coverage_by_obs = coverage_by_obs
  ))
}


#### L2_1_TropicalAndes_TD_mapping.R ####
# taxonomic diversity calculation & mapping

calculate_richness <- function(sp_grid) {
  cell_list <- apply(sp_grid, 1, c)

  # DataInfo contains number of observations (n), observed species richness (S.obs), sample coverage (SC), number of singletons (f1), number of doubletons (f2)
  info <- DataInfo(cell_list) |>
    mutate(
      richness_Chao1 = S.obs + (f1 * (f1 - 1)) / (2 * (f2 + 1))
    )

  coverage_est_0.4 <- estimateD(
    cell_list,
    q = 0,
    datatype = "abundance",
    base = "coverage",
    level = 0.4
  ) |>
    select(Assemblage, qD) |>
    rename(richness_coverage_0.4 = qD)

  coverage_est_0.5 <- estimateD(
    cell_list,
    q = 0,
    datatype = "abundance",
    base = "coverage",
    level = 0.5
  ) |>
    select(Assemblage, qD) |>
    rename(richness_coverage_0.5 = qD)

  coverage_est_0.6 <- estimateD(
    cell_list,
    q = 0,
    datatype = "abundance",
    base = "coverage",
    level = 0.6
  ) |>
    select(Assemblage, qD) |>
    rename(richness_coverage_0.6 = qD)

  info <- info |>
    left_join(
      coverage_est_0.4,
      by = "Assemblage"
    ) |>
    left_join(
      coverage_est_0.5,
      by = "Assemblage"
    ) |>
    left_join(
      coverage_est_0.6,
      by = "Assemblage"
    )

  return(info)
}

TD_map <- function(richness, resolution_meters, guild) {
  # generate coordinates, filter by cells with TD values
  TAGrid_TD <- TApoly %>%
    st_make_grid(cellsize = c(resolution_meters)) %>%
    st_intersection(TropicalAndes_IUCNHabitat_Forest) %>%
    st_cast("MULTIPOLYGON") %>%
    st_sf() %>%
    mutate(cellid = paste0("cell_", row_number())) %>%
    left_join(richness)

  if (guild == 'plant') {
    gridTDTA <-
      ggplot() +
      geom_sf(data = Americas, fill = "white") +
      geom_sf(data = TApoly, fill = "lightgrey", size = 0.1) +
      geom_sf(data = TAGrid_TD, aes(fill = richness), color = 'NA') +
      labs(fill = "Plants") +
      scale_fill_viridis_c(
        limits = lims,
        na.value = 'gray53',
        option = 'magma'
      ) +
      coord_sf(
        xlim = c(-82, -60),
        ylim = c(-24, 14),
        expand = FALSE,
        crs = 4326
      ) +
      scale_x_continuous(breaks = seq(-85, -54, by = 10)) +
      scale_y_continuous(breaks = seq(-24, 14, by = 10)) +
      theme(
        panel.background = element_rect(fill = "lightblue"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 12)
      )
  } else {
    gridTDTA <-
      ggplot() +
      geom_sf(data = Americas, fill = "white") +
      geom_sf(data = TApoly, fill = "lightgrey", size = 0.1) +
      geom_sf(data = TAGrid_TD, aes(fill = richness), color = 'NA') +
      labs(fill = paste0(str_to_title(guild), "s")) +
      scale_fill_viridis_c(limits = lims, na.value = 'gray53') +
      coord_sf(
        xlim = c(-82, -60),
        ylim = c(-24, 14),
        expand = FALSE,
        crs = 4326
      ) +
      scale_x_continuous(breaks = seq(-85, -54, by = 10)) +
      scale_y_continuous(breaks = seq(-24, 14, by = 10)) +
      theme(
        panel.background = element_rect(fill = "lightblue"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 12)
      )
  }

  list(gridTDTA = gridTDTA, spatial_TA_grid = TAGrid_TD)
}


#### L2_1_TropicalAndes_FD_mapping.R ####
# functional diversity calculation & mapping

fspaces_quality <- function(sp_grid, traits, guild) {
  if (guild %in% c('frugivore', 'mammal', 'bird')) {
    # create trait type table
    trait_name <- c(
      "body_mass_e",
      "diet_cat",
      "diet_breadth",
      "habitat_breadth",
      "generation_time"
    )
    trait_type <- c("Q", "N", "Q", "Q", "Q")
    trait_cat <- as.data.frame(cbind(trait_name, trait_type))

    # fix nominal traits as factor
    traits$diet_cat <- as.factor(traits$diet_cat)
  } else {
    trait_name <- c(
      "PlantHeight_m",
      "FruitType",
      "PlantLifespan_years",
      "SeedMass_g",
      "FruitLength_mm",
      "GrowthForm",
      "SeedLength_mm",
      "DispersalSyndrome"
    )
    trait_type <- c("Q", "N", "Q", "Q", "Q", "N", "Q", "N")
    trait_cat <- as.data.frame(cbind(trait_name, trait_type))

    traits$GrowthForm <- as.factor(traits$GrowthForm)
    traits$FruitType <- as.factor(traits$FruitType)
    traits$DispersalSyndrome <- as.factor(traits$DispersalSyndrome)
  }

  # summary of the assemblages * species dataframe
  asb_sp_summ <- asb.sp.summary(asb_sp_w = sp_grid)

  # species traits summary
  traits_summ <- sp.tr.summary(tr_cat = trait_cat, sp_tr = traits)

  # estimate functional trait-based distances between species
  sp_dist <- funct.dist(
    sp_tr = traits,
    tr_cat = trait_cat,
    metric = "gower",
    scale_euclid = "scale_center",
    ordinal_var = "classic",
    weight_type = "equal",
    stop_if_NA = TRUE
  )

  # generate a multidimensional space
  fspaces_quality <- quality.fspaces(
    sp_dist = sp_dist,
    maxdim_pcoa = 10,
    deviation_weighting = "absolute",
    fdist_scaling = FALSE,
    fdendro = "average"
  )

  assign(
    x = paste0('fspaces_quality_', guild),
    value = fspaces_quality,
    envir = .GlobalEnv
  )
}


fspace_quality_plot <- function(fspaces_quality) {
  # look at the quality spaces only (MAD index looks at the mean absolute deviation from the dissimilarity matrix; want the deviation to be low meaning that the true distances have been retained in the PCA)
  MAD <- as.data.frame(round(fspaces_quality$"quality_fspaces", 3))

  low_MAD <- MAD |>
    slice_min(mad, n = 3)

  # generate a multidimensional space
  fspaces <- quality.fspaces.plot(
    fspaces_quality = fspaces_quality,
    quality_metric = "mad",
    fspaces_plot = rownames(low_MAD)
  )

  print(c(fspaces))
  return(MAD)
}


pc_coords <- function(fspaces_quality, traits, guild) {
  # testing correlation between functional axes and traits
  sp_faxes_coord <- fspaces_quality$"details_fspaces"$"sp_pc_coord"

  # computes linear model for continuous traits and Kruskall-Wallis tests for other types.
  tr_faxes <- traits.faxes.cor(
    sp_tr = traits,
    sp_faxes_coord = sp_faxes_coord[, c("PC1", "PC2", "PC3", "PC4")],
    plot = TRUE
  )

  # print traits with significant effect:
  tr_faxes$"tr_faxes_stat"[which(tr_faxes$"tr_faxes_stat"$"p.value" < 0.05), ]

  sp_faxes_coord <- fspaces_quality$"details_fspaces"$"sp_pc_coord"

  assign(x = paste0('tr_faxes_', guild), value = tr_faxes, envir = .GlobalEnv)
  assign(
    x = paste0('sp_faxes_coord_', guild),
    value = sp_faxes_coord,
    envir = .GlobalEnv
  )
}

# Correlation between functional axes and traits
fspace_corr_plots <- function(sp_faxes_coord, tr_faxes) {
  # plotting functional space
  big_plot <- funct.space.plot(
    sp_faxes_coord = sp_faxes_coord[, c("PC1", "PC2", "PC3", "PC4")],
    faxes = c("PC1", "PC2", "PC3", "PC4"),
    alpha_ch = 0.5,
    shape_vert = 6
  )

  print(c(big_plot, tr_faxes$"tr_faxes_plot"))
}


FDis <- function(sp_grid, sp_faxes_coord) {
  # convert sp_grid to PAM
  sp_grid[sp_grid > 0] <- 1

  # need to remove parts of the PAM that have values less than or equal to the number of dimensions (4)

  # calculate row sums
  row_sums <- rowSums(sp_grid)
  subset_matrix <- sp_grid[row_sums >= 4, ]

  # match frugivore names
  sp_faxes_coord_sub <- sp_faxes_coord[, c("PC1", "PC2", "PC3", "PC4")]
  summary(sp_faxes_coord_sub)

  # extract names
  subset_matrix_names <- colnames(subset_matrix)
  sp_faxes_coord_sub_names <- row.names(sp_faxes_coord_sub)

  # create df
  sp_faxes_coord_sub <- as.data.frame(sp_faxes_coord_sub)
  row.names(sp_faxes_coord_sub) <- sp_faxes_coord_sub_names

  # remove names removed from matrix
  names <- intersect(sp_faxes_coord_sub_names, subset_matrix_names)
  names <- na.omit(names)

  sp_faxes_coord_sub <- sp_faxes_coord_sub[
    which((row.names(sp_faxes_coord_sub) %in% names) == TRUE),
  ]

  subset_matrix <- as.data.frame(subset_matrix)
  subset_matrix <- subset_matrix[, which(
    (colnames(subset_matrix) %in% names) == TRUE
  )]

  sp_faxes_coord_sub <- na.omit(sp_faxes_coord_sub)
  subset_matrix <- na.omit(subset_matrix)

  message(paste0(
    'nrows subset:',
    nrow(sp_faxes_coord_sub),
    ', ncols matrix:',
    ncol(subset_matrix)
  ))

  if (nrow(sp_faxes_coord_sub) != ncol(subset_matrix)) {
    stop(
      "Number of rows in sp_faxes_coord_sub does not match number of columns in subset_matrix"
    )
  }

  sp_faxes_coord_sub <- as.matrix(sp_faxes_coord_sub)
  subset_matrix <- as.matrix(subset_matrix)

  # computing FDis

  # the number of species per assemblage has to be higher or equal to the number of traits

  # Use three fewer cores than available
  #num_cores <- parallel::detectCores() - 3

  # HPCC
  num_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))

  # Set up parallel backend
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)

  tryCatch(
    {
      # Define chunk size
      chunk_size <- ceiling(nrow(subset_matrix) / num_cores)

      # Split data into chunks for parallel processing
      chunks <- split(
        seq_len(nrow(subset_matrix)),
        ceiling(seq_len(nrow(subset_matrix)) / chunk_size)
      )

      # Process chunks in parallel
      results_list <- foreach(
        chunk_indices = chunks,
        .combine = rbind,
        .packages = "mFD"
      ) %dopar%
        {
          subset_matrix_chunk <- subset_matrix[chunk_indices, , drop = FALSE]

          alpha_fd_indices <- alpha.fd.multidim(
            sp_faxes_coord = sp_faxes_coord_sub,
            asb_sp_w = subset_matrix,
            ind_vect = "fdis",
            details_returned = TRUE
          )

          details_list <- alpha_fd_indices$"details" # see if this is needed

          # get functional dispersion
          fdis_values <- alpha_fd_indices$functional_diversity_indices$fdis

          # match with corresponding cell numbers
          cell_numbers <- rownames(subset_matrix)[chunk_indices]

          # Create a data frame for this chunk
          chunk_results <- data.frame(cellid = cell_numbers, fdis = fdis_values)

          chunk_results
        }
      # # Combine all chunk results into a single data frame
      # all_results <- do.call(rbind, results_list)

      # Stop the parallel backend
      stopCluster(cl)

      return(results_list)
    },
    error = function(e) {
      stop("Error in parallel processing: ", conditionMessage(e))
    }
  )
}


FD_map <- function(fdis, resolution_meters, guild) {
  # generate coordinates, filter by cells with fdis values
  TAGrid_fdis <- TApoly %>%
    st_make_grid(cellsize = c(resolution_meters)) %>%
    st_intersection(TropicalAndes_IUCNHabitat_Forest) %>%
    st_cast("MULTIPOLYGON") %>%
    st_sf() %>%
    mutate(cellid = paste0("cell_", row_number())) %>%
    left_join(fdis)

  if (guild == 'plant') {
    gridFDisTA <-
      ggplot() +
      geom_sf(data = Americas, fill = "white") +
      geom_sf(data = TApoly, fill = "lightgrey", size = 0.1) +
      geom_sf(data = TAGrid_fdis, aes(fill = fdis), color = 'NA') +
      labs(fill = "Plants") +
      scale_fill_viridis_c(
        limits = lims,
        na.value = 'gray53',
        option = 'magma'
      ) +
      coord_sf(
        xlim = c(-82, -60),
        ylim = c(-24, 14),
        expand = FALSE,
        crs = 4326
      ) +
      scale_x_continuous(breaks = seq(-85, -54, by = 10)) +
      scale_y_continuous(breaks = seq(-24, 14, by = 10)) +
      theme(
        panel.background = element_rect(fill = "lightblue"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 12)
      )
  } else {
    gridFDisTA <-
      ggplot() +
      geom_sf(data = Americas, fill = "white") +
      geom_sf(data = TApoly, fill = "lightgrey", size = 0.1) +
      geom_sf(data = TAGrid_fdis, aes(fill = fdis), color = 'NA') +
      labs(fill = paste0(str_to_title(guild), "s")) +
      scale_fill_viridis_c(limits = lims, na.value = 'gray53') +
      coord_sf(
        xlim = c(-82, -60),
        ylim = c(-24, 14),
        expand = FALSE,
        crs = 4326
      ) +
      scale_x_continuous(breaks = seq(-85, -54, by = 10)) +
      scale_y_continuous(breaks = seq(-24, 14, by = 10)) +
      theme(
        panel.background = element_rect(fill = "lightblue"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 12)
      )
  }

  list(gridFDisTA = gridFDisTA, spatial_fdis_grid = TAGrid_fdis)
}


#### L2_2_TropicalAndes_TD_relationships.R & L2_2_TropicalAndes_FD_relationships.R####
# TD-TD or FD-FD comparisons

div_comparison <- function(plant_div, mammal_div, bird_div, resolution) {
  set.seed(123)

  metric <- if ("richness" %in% colnames(plant_div)) "richness" else "fdis"

  if ('richness' %in% colnames(plant_div)) {
    coords <- as.data.frame(st_coordinates(st_centroid(plant_div)))

    mammal_plant <- data.frame(
      cell_id = plant_div$cellid,
      x = coords$X,
      y = coords$Y,
      plant_div = plant_div$richness,
      frug_div = mammal_div$richness,
      taxa = c(rep('Mammal', nrow(mammal_div)))
    ) %>%
      dplyr::filter(plant_div > 0 & frug_div > 0)

    m1 <- lm(frug_div ~ plant_div, data = mammal_plant)

    bird_plant <- data.frame(
      cell_id = plant_div$cellid,
      x = coords$X,
      y = coords$Y,
      plant_div = plant_div$richness,
      frug_div = bird_div$richness,
      taxa = c(rep('Bird', nrow(bird_div)))
    ) %>%
      dplyr::filter(plant_div > 0 & frug_div > 0)

    m2 <- lm(frug_div ~ plant_div, data = bird_plant)

    rng <- range(plant_div$richness, na.rm = TRUE)

    newdata <- data.frame(
      plant_div = seq(rng[1], rng[2], length.out = 100),
      x = mean(coords$X, na.rm = TRUE),
      y = mean(coords$Y, na.rm = TRUE)
    )

    newdata$Mammal <- predict(m1, newdata = newdata, type = "response")
    newdata$Bird <- predict(m2, newdata = newdata, type = "response")

    plot_df <- newdata %>%
      pivot_longer(
        cols = c(Mammal, Bird),
        names_to = "taxa",
        values_to = "frug_div"
      )

    plot_points <- dplyr::bind_rows(mammal_plant, bird_plant)

    r2_df <- data.frame(
      taxa = c("Mammal", "Bird"),
      r2 = c(summary(m1)$adj.r.squared, summary(m2)$adj.r.squared)
    )

    (plot <- ggplot(
      data = plot_points,
      aes(x = plant_div, y = frug_div, color = taxa)
    ) +
      geom_point(alpha = 0.5, size = 3) +
      geom_line(
        data = plot_df,
        aes(x = plant_div, y = frug_div, color = taxa),
        size = 1.2
      ) +
      scale_color_manual(values = c('lightsteelblue2', 'burlywood3')) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 1600)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 400)) +
      labs(
        x = 'Plant richness by cell',
        y = 'Frugivore richness by cell',
        color = 'Taxa',
        title = paste0('[', resolution, 'km]')
      ) +
      theme_classic() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)
      ))
  } else {
    coords <- as.data.frame(st_coordinates(st_centroid(plant_div)))

    mammal_plant <- data.frame(
      cell_id = plant_div$cellid,
      x = coords$X,
      y = coords$Y,
      plant_div = plant_div$fdis,
      frug_div = mammal_div$fdis,
      taxa = c(rep('Mammal', nrow(mammal_div)))
    ) %>%
      dplyr::filter(plant_div > 0 & frug_div > 0)

    m1 <- lm(frug_div ~ plant_div, data = mammal_plant)

    bird_plant <- data.frame(
      cell_id = plant_div$cellid,
      x = coords$X,
      y = coords$Y,
      plant_div = plant_div$fdis,
      frug_div = bird_div$fdis,
      taxa = c(rep('Bird', nrow(bird_div)))
    ) %>%
      dplyr::filter(plant_div > 0 & frug_div > 0)

    m2 <- lm(frug_div ~ plant_div, data = bird_plant)

    rng <- range(plant_div$fdis, na.rm = TRUE)

    newdata <- data.frame(
      plant_div = seq(rng[1], rng[2], length.out = 100),
      x = mean(coords$X, na.rm = TRUE),
      y = mean(coords$Y, na.rm = TRUE)
    )

    newdata$Mammal <- predict(m1, newdata = newdata, type = "response")
    newdata$Bird <- predict(m2, newdata = newdata, type = "response")

    plot_df <- newdata %>%
      pivot_longer(
        cols = c(Mammal, Bird),
        names_to = "taxa",
        values_to = "frug_div"
      )

    plot_points <- dplyr::bind_rows(mammal_plant, bird_plant)

    r2_df <- data.frame(
      taxa = c("Mammal", "Bird"),
      r2 = c(summary(m1)$adj.r.squared, summary(m2)$adj.r.squared)
    )

    (plot <- ggplot(
      data = plot_points,
      aes(x = plant_div, y = frug_div, color = taxa)
    ) +
      geom_point(alpha = 0.5, size = 3) +
      geom_line(
        data = plot_df,
        aes(x = plant_div, y = frug_div, color = taxa),
        size = 1.2
      ) +
      scale_color_manual(values = c('lightsteelblue2', 'burlywood3')) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 0.8)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 0.8)) +
      labs(
        x = 'Plant richness by cell',
        y = 'Frugivore richness by cell',
        color = 'Taxa',
        title = paste0('[', resolution, 'km]')
      ) +
      theme_classic() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)
      ))
  }

  result <- list(
    plot = plot,
    r2_df = r2_df,
    m1 = m1,
    m2 = m2
  )

  return(result)
}


# TD-TD or FD-FD (GAM)
div_comparison_gam <- function(plant_div, mammal_div, bird_div, resolution) {
  set.seed(123)

  metric <- if ("richness" %in% colnames(plant_div)) "richness" else "fdis"

  if ('richness' %in% colnames(plant_div)) {
    coords <- as.data.frame(st_coordinates(st_centroid(plant_div)))

    mammal_plant <- data.frame(
      cell_id = plant_div$cellid,
      x = coords$X,
      y = coords$Y,
      plant_div = plant_div$richness,
      frug_div = mammal_div$richness,
      taxa = c(rep('Mammal', nrow(mammal_div)))
    ) %>%
      dplyr::filter(plant_div > 0 & frug_div > 0)

    m1 <- gam(
      frug_div ~ s(plant_div) + s(x, y, bs = 'gp', k = 20),
      data = mammal_plant,
      method = "REML"
    )

    m1_vario <- create_variogram(mammal_plant, m1)

    bird_plant <- data.frame(
      cell_id = plant_div$cellid,
      x = coords$X,
      y = coords$Y,
      plant_div = plant_div$richness,
      frug_div = bird_div$richness,
      taxa = c(rep('Bird', nrow(bird_div)))
    ) %>%
      dplyr::filter(plant_div > 0 & frug_div > 0)

    m2 <- gam(
      frug_div ~ s(plant_div) + s(x, y, bs = 'gp', k = 20),
      data = bird_plant,
      method = "REML"
    )

    m2_vario <- create_variogram(bird_plant, m2)

    rng <- range(plant_div$richness, na.rm = TRUE)

    newdata <- data.frame(
      plant_div = seq(rng[1], rng[2], length.out = 100),
      x = mean(coords$X, na.rm = TRUE),
      y = mean(coords$Y, na.rm = TRUE)
    )

    newdata$Mammal <- predict(m1, newdata = newdata, type = "response")
    newdata$Bird <- predict(m2, newdata = newdata, type = "response")

    plot_df <- newdata %>%
      pivot_longer(
        cols = c(Mammal, Bird),
        names_to = "taxa",
        values_to = "frug_div"
      )

    plot_points <- dplyr::bind_rows(
      mammal_plant,
      bird_plant
    )

    r2_df <- data.frame(
      taxa = c("Mammal", "Bird"),
      r2 = c(summary(m1)$r.sq, summary(m2)$r.sq)
    )

    r2_df$dev_expl <- c(summary(m1)$dev.expl, summary(m2)$dev.expl)

    (plot <- ggplot(
      data = plot_points,
      aes(x = plant_div, y = frug_div, color = taxa)
    ) +
      geom_point(alpha = 0.5, size = 3) +
      geom_line(
        data = plot_df,
        aes(x = plant_div, y = frug_div, color = taxa),
        size = 1.2
      ) +
      scale_color_manual(values = c('lightsteelblue2', 'burlywood3')) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 1600)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 400)) +
      labs(
        x = 'Plant richness by cell',
        y = 'Frugivore richness by cell',
        color = 'Taxa',
        title = paste0('[', resolution, 'km]')
      ) +
      theme_classic() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)
      ))
  } else {
    coords <- as.data.frame(st_coordinates(st_centroid(plant_div)))

    mammal_plant <- data.frame(
      cell_id = plant_div$cellid,
      x = coords$X,
      y = coords$Y,
      plant_div = plant_div$fdis,
      frug_div = mammal_div$fdis,
      taxa = c(rep('Mammal', nrow(mammal_div)))
    ) %>%
      dplyr::filter(plant_div > 0 & frug_div > 0)

    m1 <- gam(
      frug_div ~ s(plant_div),
      data = mammal_plant,
      family = betar(link = "logit"),
      method = "REML"
    )

    m1_vario <- create_variogram(mammal_plant, m1)

    bird_plant <- data.frame(
      cell_id = plant_div$cellid,
      x = coords$X,
      y = coords$Y,
      plant_div = plant_div$fdis,
      frug_div = bird_div$fdis,
      taxa = c(rep('Bird', nrow(bird_div)))
    ) %>%
      dplyr::filter(plant_div > 0 & frug_div > 0)

    m2 <- gam(
      frug_div ~ s(plant_div),
      data = bird_plant,
      family = betar(link = "logit"),
      method = "REML"
    )

    m2_vario <- create_variogram(bird_plant, m2)

    rng <- range(plant_div$fdis, na.rm = TRUE)

    newdata <- data.frame(
      plant_div = seq(rng[1], rng[2], length.out = 100),
      x = mean(coords$X, na.rm = TRUE),
      y = mean(coords$Y, na.rm = TRUE)
    )

    newdata$Mammal <- predict(m1, newdata = newdata, type = "response")
    newdata$Bird <- predict(m2, newdata = newdata, type = "response")

    plot_df <- newdata %>%
      pivot_longer(
        cols = c(Mammal, Bird),
        names_to = "taxa",
        values_to = "frug_div"
      )

    plot_points <- dplyr::bind_rows(
      mammal_plant,
      bird_plant
    )

    r2_df <- data.frame(
      taxa = c("Mammal", "Bird"),
      r2 = c(summary(m1)$r.sq, summary(m2)$r.sq)
    )

    r2_df$dev_expl <- c(summary(m1)$dev.expl, summary(m2)$dev.expl)

    (plot <- ggplot(
      data = plot_points,
      aes(x = plant_div, y = frug_div, color = taxa)
    ) +
      geom_point(alpha = 0.5, size = 3) +
      geom_line(
        data = plot_df,
        aes(x = plant_div, y = frug_div, color = taxa),
        size = 1.2
      ) +
      scale_color_manual(values = c('lightsteelblue2', 'burlywood3')) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 0.8)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 0.8)) +
      labs(
        x = 'Plant richness by cell',
        y = 'Frugivore richness by cell',
        color = 'Taxa',
        title = paste0('[', resolution, 'km]')
      ) +
      theme_classic() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)
      ))
  }

  result <- list(
    plot = plot,
    r2_df = r2_df,
    m1 = m1,
    m2 = m2,
    m1_vario = m1_vario,
    m2_vario = m2_vario
  )

  return(result)
}

# A function to examine spatial autocorrelation in the residuals of the model
create_variogram <- function(df, mod) {
  df$res <- residuals(mod, type = "working")
  coordinates(df) <- ~ x + y
  variogram(res ~ 1, data = df)
}

#### L2_2_TropicalAndes_TD-FD_relationships.R####

# TD-FD within same taxa 
div_comparison2 <- function(TD, FD, guild, resolution) {
  all_div <- data.frame(cell_id = TD$cellid, TD = TD$num_species, FD = FD$fdis)

  # Filter out rows where either plant or frugivore richness is zero
  div_filtered <- all_div %>%
    filter(TD > 0 & FD > 0)

  if (guild == 'plant') {
    plot <- ggplot(data = div_filtered, aes(x = TD, y = FD)) +
      geom_point(size = 2, color = 'darkseagreen3') +
      labs(
        x = 'Plant richness by cell',
        y = 'Plant FDis by cell',
        title = paste0('[', resolution, 'km]')
      ) +
      geom_smooth(method = 'lm', se = FALSE, color = 'darkseagreen3') +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 1600)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, .8)) +
      theme_classic() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)
      )

    sum_trend <- trendline_sum(
      div_filtered$TD,
      div_filtered$FD,
      model = "line2P"
    )
  } else {
    if (guild == 'mammal') {
      plot <- ggplot(data = div_filtered, aes(x = TD, y = FD)) +
        geom_point(size = 2, color = 'burlywood3') +
        labs(
          x = 'Mammal richness by cell',
          y = 'Mammal FDis by cell',
          title = paste0('[', resolution, 'km]')
        ) +
        geom_smooth(method = 'lm', se = FALSE, color = 'burlywood3') +
        scale_x_continuous(expand = c(0, 0), limits = c(0, 130)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, .8)) +
        theme_classic() +
        theme(
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16)
        )

      sum_trend <- trendline_sum(
        div_filtered$TD,
        div_filtered$FD,
        model = "line2P"
      )
    } else {
      plot <- ggplot(data = div_filtered, aes(x = TD, y = FD)) +
        geom_point(size = 2, color = 'lightsteelblue2') +
        labs(
          x = 'Bird richness by cell',
          y = 'Bird FDis by cell',
          title = paste0('[', resolution, 'km]')
        ) +
        geom_smooth(method = 'lm', se = FALSE, color = 'lightsteelblue2') +
        scale_x_continuous(expand = c(0, 0), limits = c(0, 360)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, .8)) +
        theme_classic() +
        theme(
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16)
        )

      sum_trend <- trendline_sum(
        div_filtered$TD,
        div_filtered$FD,
        model = "line2P"
      )
    }
  }

  result <- list(
    plot = plot,
    trend = sum_trend
  )
  return(result)
}


# TD plants-FD birds/mammals, FD plants-TD birds/mammals
div_comparison3 <- function(plant_div, mammal_div, bird_div, resolution) {
  if ('num_species' %in% colnames(plant_div)) {
    mammal_plant <- data.frame(
      cell_id = plant_div$cellid,
      plant_div = plant_div$num_species,
      frug_div = mammal_div$fdis,
      taxa = c(rep('Mammal', nrow(mammal_div)))
    )

    mammal_plant <- mammal_plant %>%
      filter(plant_div > 0 & frug_div > 0)

    bird_plant <- data.frame(
      cell_id = plant_div$cellid,
      plant_div = plant_div$num_species,
      frug_div = bird_div$fdis,
      taxa = c(rep('Bird', nrow(bird_div)))
    )

    bird_plant <- bird_plant %>%
      filter(plant_div > 0 & frug_div > 0)

    plot1 <- ggplot(data = mammal_plant, aes(x = plant_div, y = frug_div)) +
      geom_point(size = 2, color = 'burlywood3') +
      labs(
        x = 'Plant richness by cell',
        y = 'Mammal FDis by cell',
        title = paste0('[', resolution, 'km]')
      ) +
      geom_smooth(method = 'lm', se = FALSE, color = 'burlywood3') +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 1600)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, .8)) +
      theme_classic() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)
      )
    mammal_plant_sum_trend <- trendline_sum(
      mammal_plant$plant_div,
      mammal_plant$frug_div,
      model = "line2P"
    )

    plot2 <- ggplot(data = bird_plant, aes(x = plant_div, y = frug_div)) +
      geom_point(size = 2, color = 'lightsteelblue2') +
      labs(
        x = 'Plant richness by cell',
        y = 'Bird FDis by cell',
        title = paste0('[', resolution, 'km]')
      ) +
      geom_smooth(method = 'lm', se = FALSE, color = 'lightsteelblue2') +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 1600)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, .8)) +
      theme_classic() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)
      )
    bird_plant_sum_trend <- trendline_sum(
      bird_plant$plant_div,
      bird_plant$frug_div,
      model = "line2P"
    )
  } else {
    mammal_plant <- data.frame(
      cell_id = plant_div$cellid,
      plant_div = plant_div$fdis,
      frug_div = mammal_div$num_species,
      taxa = c(rep('Mammal', nrow(mammal_div)))
    )

    mammal_plant <- mammal_plant %>%
      filter(plant_div > 0 & frug_div > 0)

    bird_plant <- data.frame(
      cell_id = plant_div$cellid,
      plant_div = plant_div$fdis,
      frug_div = bird_div$num_species,
      taxa = c(rep('Bird', nrow(bird_div)))
    )

    bird_plant <- bird_plant %>%
      filter(plant_div > 0 & frug_div > 0)

    plot1 <- ggplot(data = mammal_plant, aes(x = frug_div, y = plant_div)) +
      geom_point(size = 2, color = 'burlywood3') +
      labs(
        x = 'Mammal richness by cell',
        y = 'Plant FDis by cell',
        title = paste0('[', resolution, 'km]')
      ) +
      geom_smooth(method = 'lm', se = FALSE, color = 'burlywood3') +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 130)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, .8)) +
      theme_classic() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)
      )
    mammal_plant_sum_trend <- trendline_sum(
      mammal_plant$plant_div,
      mammal_plant$frug_div,
      model = "line2P"
    )

    plot2 <- ggplot(data = bird_plant, aes(x = frug_div, y = plant_div)) +
      geom_point(size = 2, color = 'lightsteelblue2') +
      labs(
        x = 'Bird richness by cell',
        y = 'Plant FDis by cell',
        title = paste0('[', resolution, 'km]')
      ) +
      geom_smooth(method = 'lm', se = FALSE, color = 'lightsteelblue2') +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 400)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, .8)) +
      theme_classic() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)
      )
    bird_plant_sum_trend <- trendline_sum(
      bird_plant$plant_div,
      bird_plant$frug_div,
      model = "line2P"
    )
  }

  result <- list(
    plot1 = plot1,
    plot2 = plot2,
    mammal = mammal_plant_sum_trend,
    bird = bird_plant_sum_trend
  )

  return(result)
}


#### L2_2_TropicalAndes_TD-FD_mapping.R ####
# Mapping difference between FD and TD (scaled 0-1)

div_diff_map <- function(cell_FD, cell_TD, guild){
  
  df <- left_join(st_drop_geometry(cell_FD), cell_TD, by = "cellid")
  
  df$richness_scaled <- rescale(df$richness)
  
  df$div_diff <- df$richness_scaled - df$fdis
  
  sf <- df %>%
    select(cellid, geometry, div_diff)
  
  sf <- st_as_sf(sf)
  
  cols <- viridis(3)
  
  plot <- ggplot() +
    geom_sf(data = Americas, fill = "white")+
    geom_sf(data = TApoly, fill = "lightgrey", size = 0.1) +
    geom_sf(data = sf, aes(fill = div_diff), color = 'NA') +
    labs(fill = "Richness - FDis") +
    scale_fill_gradient2(limits = c(0.7, -0.7), low = cols[1], mid = cols[2], high = cols[3], midpoint = 0, na.value = 'gray53') + 
    coord_sf(xlim = c(-82, -60), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
    scale_x_continuous(breaks = seq(-85, -54, by = 10)) + 
    scale_y_continuous(breaks = seq(-24, 14, by = 10)) +
    theme(panel.background = element_rect(fill = "lightblue"), axis.title = element_text(size = 16), axis.text = element_text(size = 12), legend.title = element_text(size = 16), legend.text = element_text(size = 12), plot.title = element_text(hjust = 0.5, size=12))
  
  return(plot)
}
