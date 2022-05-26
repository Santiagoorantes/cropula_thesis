#-------------------------------------------------------------------------------
# Reading in the data for the project
#-------------------------------------------------------------------------------

# --- 0.0 Paths --- 

# All the csv for the crop data are in the folder "info_agr"
# All the downloaded price series were stored as csv in the folder "info_comm"
project_path <- getwd()
agr_data_path <- paste(getwd(), "/info_agr",sep = "")
comm_data_path <- paste(getwd(), "/info_comm",sep = "")

# --- 0.1 Crop data --- 

## source: http://infosiap.siap.gob.mx/gobmx/datosAbiertos.php
setwd(agr_data_path)

agr_files <- lapply(Sys.glob("Cierre_agricola_mun_*.csv"), read.csv)
col_names <- colnames(data.frame(agr_files[1]))
agr_files <- lapply(agr_files, setNames, col_names)
# df containing the historical agricultural (crop) information.
agr_df <- do.call(rbind, agr_files)
rm(agr_files)

setwd(project_path)

# --- 0.2 Price data ---

# --- Quandl Continuous Futures Contracts API

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

setwd(comm_data_path)
corn_ts <- read.csv("CME_C3_20210629.csv")
setwd(project_path)


