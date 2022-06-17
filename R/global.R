#-------------------------------------------------------------------------------
# Import/install all required packages
#-------------------------------------------------------------------------------

packages <- c(
  
  "this.path",
  
  # data manipulation
  "tidyverse",
  "reshape",
  "data.table",
  "kableExtra",
  "lubridate",
  
  # Plotting and animation
  "ggplot2",
  "ggthemes",
  "ggExtra",
  "patchwork",
  "scales",
  "plotly",
  "gganimate",
  "hexbin",
  
  # Price series
  "Quandl",
  "rugarch",
  
  # Fitting
  "fitdistrplus",
  "ExtDist",
  "VGAM",
  "actuar",
  
  # Copulas
  "copula",
  "VineCopula",
  "gofCopula",
  "parallel"

)

# Verify required packages are installed
new_packs <- packages[!(packages %in% installed.packages())]
if(length(new_packs)) install.packages(new_packs, dependencies = TRUE)
rm(new_packs)

# --- Load packages ---
suppressPackageStartupMessages(lapply(packages, require, character.only = TRUE))

