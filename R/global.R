
# ----------------- Import/install all required packages -----------------------

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
  "lmtest",
  
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

# --------------------- Load Utilities and Modules -----------------------------

folders <- c("utilities", "modules")
file_sources <- list.files(folders, pattern = "\\.R$", full.names = TRUE)
sapply(file_sources, source, .GlobalEnv)
