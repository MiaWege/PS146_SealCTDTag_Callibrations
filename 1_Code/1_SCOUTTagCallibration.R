# PS146 Weddell Seal CTD data calibrate 
# Code by Mia Wege and formulas for corrections by Leo Middleton
# CTD tags were placed on ship's rosette and calibration formulas put together by Leo Middleton
# Two types of tags with 2 different responses: SMRU CTD and Wildlife Computers SCOUT tag

# Load the libraries --------
{ 
  library(tidyverse)
  library(gsw)
  library(oce)
  library(cmocean)
}

#Calibration Coefficients
coef <- read.csv("./0_Data/CTD_CallibrationCoefficients.csv")
coef


# Wildlife Computers SCOUT TAGS --------------

# Set the path to your folder with SCOUT data
folder_path <- "./0_Data/PS146_SEAROSEII_WC_SCOUT_FinalDownload_31March2026/For Fielax/"

#  Create a new folder to which I will save the new data instead of overwriting
output_dir <- "./0_Data/PS146_SEAROSEII_WC_SCOUT_FinalDownload_31March2026/For Fielax/CTD_Corrected/"
dir.create(output_dir, showWarnings = FALSE)


# Get all CSV files matching the pattern
scoutfiles <- list.files(
  path = folder_path,
  pattern = "CTDprofileData.*\\.csv$",
  full.names = TRUE
)


process_SCOUT_ctd_file <- function(filepath, coef) {
  
  # --- Read & identify seal ---
  raw  <- read_csv(filepath, show_col_types = FALSE) %>% as.data.frame()
  seal <- unique(raw$DeployID)
  
  # 1. Pressure from depth because corrections happen in pressure not depth 
  raw$pressure <- gsw::gsw_p_from_z(0 - raw$Depth, raw$Latitude)
  
  # 2. Conductivity from salinity because  ---
  raw$Conductivity <- gsw::gsw_C_from_SP(raw$PSU, raw$Temperature, raw$pressure)
  
  # --- 3. Pressure correction ---
  # p_corrected = a_i × p_measured
  ai <- coef$ai[coef$Seal == seal]
  raw$pcorrected <- ai * raw$pressure
  
  # --- 4. Conductivity correction ---
  # C_corrected = (ci × T_measured + d_i) × C_measured
  ci <- coef$ci[coef$Seal == seal]
  di <- coef$di[coef$Seal == seal]
  raw$Ccorrected <- ((ci * raw$Temperature) + di) * raw$Conductivity
  
  # --- 5. Back to salinity ---
  raw$PSUCorrected <- gsw::gsw_SP_from_C(raw$Ccorrected, raw$Temperature, raw$pressure)
  
  # --- Save the file ---
  write_csv(raw, file.path(output_dir, basename(filepath)))
  message("Saved: ", basename(filepath))
  
  # --- AND return the data ---
  return(raw)
}

# --- Saves each file AND binds into one dataframe ---
scout <- lapply(scoutfiles, process_SCOUT_ctd_file, coef = coef) %>% bind_rows()
saveRDS(scout, "./0_Data/SCOUT_CTD_Corrected.RDS")
rm(ai,ci,di,seal,i, )
