#-------------------------------------------------------------------------------
# Reading in the data for the project
#-------------------------------------------------------------------------------

# --- 0.0 Paths --- 

# All the csv for the crop data are in the folder "info_agr"
# All the downloaded price series were stored as csv in the folder "info_comm"
R_dir <- here()
agr_data_path <- paste(here(..=1), "/info_agr",sep = "")
comm_data_path <- paste(here(..=1), "/info_comm",sep = "")

#-------------------------------------------------------------------------------
# 1.0 SIAP Database
#-------------------------------------------------------------------------------

# --- 1.1 Raw data --- 

## source: http://infosiap.siap.gob.mx/gobmx/datosAbiertos.php
setwd(agr_data_path)

agr_files <- lapply(Sys.glob("Cierre_agricola_mun_*.csv"), read.csv)
col_names <- colnames(data.frame(agr_files[1]))
agr_files <- lapply(agr_files, setNames, col_names)
# Historical Agricultural (crop) information.
agr_df <- do.call(rbind, agr_files)
rm(agr_files)

# --- 1.2 Clean data ---

# Change column_names of SIAP db to their English equivalent.
colnames(agr_df) <- c(
  "year",
  "id_state",
  "state",
  "id_district",  
  "district", # DDR (rural-development-district)
  "id_cader",
  "cader", # Further split of the DDR
  "id_municipality",
  "municipality",
  "id_cycle",
  "cycle",
  "id_type",
  "type",
  "id_unit",
  "unit",
  "id_crop",
  "crop",
  "sowed",
  "harvested",
  "damaged",
  "volume",
  "yield",
  "price",
  "production_value"
)

setwd(R_dir)


#-------------------------------------------------------------------------------
# 2.0 Price Time Series
#-------------------------------------------------------------------------------

# --- 2.1 Nasdaq Continuous Futures Contracts API

# Our API_KEY is safely stored in the config file
source("config.R")

#CME_W3 <- Quandl("CHRIS/CME_W3", api_key = API_KEY) # Wheat
#CME_C3 <- Quandl("CHRIS/CME_C3", api_key = API_KEY, type = "xts") # Corn
#CME_S3 <- Quandl("CHRIS/CME_S3", api_key = API_KEY) # Soybean
#ICE_CT3 <- Quandl("CHRIS/ICE_CT3", api_key = API_KEY) # Cotton
#ICE_KC3 <- Quandl("CHRIS/ICE_KC3", api_key = API_KEY) # Coffee Arabica
#ICE_SB3 <- Quandl("CHRIS/ICE_SB3", api_key = API_KEY) # Sugar No.11
#CME_O3 <- Quandl("CHRIS/CME_O3", api_key = API_KEY) # Oats
#CME_RR3 <- Quandl("CHRIS/CME_RR3", api_key = API_KEY) # Rough Rice

#write.csv(CME_RR3, file = "CME_C3_20210629.csv", row.names = FALSE)

# --- 2.2 Read Corn Data ---

setwd(comm_data_path)

corn_ts <- read.csv("CME_C3_20210629.csv")

# Corn continuous futures
corn_ts$Date <- as.Date(corn_ts$Date, "%Y-%m-%d")

# Timeframe of interest
ini_date <- as.Date("2003-01-01")
end_date <- as.Date("2020-12-31")

corn_ts <- dplyr::filter(corn_ts, Date >= ini_date, Date <= end_date)

setwd(R_dir)


