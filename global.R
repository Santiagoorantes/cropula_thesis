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
  
  # Price series
  "Quandl",
  "rugarch",
  
  # Fitting
  "fitdistrplus",
  "ExtDist",
  "VGAM",
  
  # Copulas
  "copula",
  "VineCopula"

)

# Verify required packages are installed
new_packs <- packages[!(packages %in% installed.packages())]
if(length(new_packs)) install.packages(new_packs, dependencies = TRUE)

# --- Load packages ---
suppressPackageStartupMessages(lapply(packages, require, character.only = TRUE))

rm(new_packs)

#~~~~~~~ Temporary ~~~~~~~~~~~~~~~
# 
# library(actuar)
# library(fitur)
# library(tikzDevice)
# library(extrafont)
# library(remotes)
# library(av)
# font_import() # Loading fonts (takes a while)