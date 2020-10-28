# Run self-organizing map to cluster event metrics
# This script is based of of the R script, sedRegData_V2.R, from Kristen Underwood

# Load packages
  # to run the SOM
  library("kohonen")
  # to run adonis() for calc of nonparametric F test
  library("vegan")
