# Run self-organizing map to cluster event metrics
# This script is based of of the R script, sedRegData_V2.R, from Kristen Underwood

# Load packages ----
  library("tidyverse") # general workhorse
  library("kohonen")   # to run the SOM
  library("vegan")     # to run adonis() for calc of nonparametric F test
  
# Read in data ----
  # Calculated event metrics for each site as calculated in compile_calculate_allVars.R
  wade <- read_csv("Data/eventMetrics_wade.csv")


# WADE ----  
  # PREPARE DATA ----
    # To begin, let's only use observations where there are no NAs
    dat_wd <- wade %>% 
      # Remove non-numerical columns
      select(-c(site, season, event_start)) %>% 
      # Remove response variables
      select(-c(NO3_kg_km2, SRP_kg_km2, event_NO3_SRP, turb_kg_km2)) %>% 
      # Keep the multipeak column for now, but code it as numerical
      mutate(multipeak = ifelse(multipeak == "NO", 0, 1)) %>% 
      # Keep only complete observations/rows (no NAs in any of the columns)
      na.omit()
  # We're left with  67 obs/rows of 120; but if we determine certain variables aren't useful for clustering
  # we can remove them and reiterate this process and may end up with more obs/rows
  
  # SET UP THE GRID/LATTICE FOR THE SOM ----
    # According to the heuristic rule from Vesanto 2000, number of grid elements/grid size/nodes = 5 * sqrt(n)
    VesantoNodes_wd = round(5 * sqrt(nrow(dat_wd)), 0)
    
    # To determine the the shape of the grid, we'll determine the ratio of columns to rows using the ratio of 
    # the first two eigen values of the input data set as recommended by Park et al. 2006
    # PCA
    # scale = TRUE = correlation matrix; scale = FALSE = covariance matrix
    PCA_wd <- prcomp(dat_wd, scale = TRUE, center = TRUE)
    eigenValues_wd <- PCA_wd$sdev^2
    first_wd <- nth(eigenValues_wd, 1)
    second_wd <- nth(eigenValues_wd, 2)
    gridRatio_wd <- round((first_wd/second_wd), 1)
    
    # Input this information into the params_SOM . . .csv file