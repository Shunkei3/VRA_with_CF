#/*----------------------------------*/
#' ## Preparation
#/*----------------------------------*/
library(here)
library(tmap)
library(sp)
library(sf)
library(agricolae)
library(lwgeom)
library(measurements)
library(raster)
library(data.table)
library(tidyverse)
library(gstat)

# === Load Functions === #
source(here("Codes/0_1_functions_gen_analysis_data.R"))

# === Load Field data === #
field <- readRDS(here("Data/analysis_field.rds"))

# /*=================================================*/
#' # Unconditional Gaussian geostatistical Simulation
# /*=================================================*/
# === Set up === #
# --- Geographical locations of Cells --- #
xy <- dplyr::select(field, unique_cell_id) %>%
  cbind(., st_coordinates(st_centroid(.))) %>%
  st_drop_geometry() %>%
  data.table()

# --- Number of iterations --- #
b = 100
# --- Range (m) --- #
Range = 400


#/*-------------------------------------*/
#' ## (1) Small degree of yield error
#/*-------------------------------------*/
# --- yield error size  --- #
Error_small = 0.0075

# === Generate coefficients === #
set.seed(39476)

coef_data_low <- 
  gen_coefs_par(
    B = b, 
    geo_xy = xy,
    sp_range = Range,
    psill_merror = Error_small
    )


saveRDS(coef_data_low, here("Data/", paste0('coefficients_sprange_',Range,'_low_error.rds')))



#/*-------------------------------------*/
#' ## (2) Large degree of yield error
#/*-------------------------------------*/
# --- yield error size --- #
Error_large = 0.0225

# === Generate coefficients === #
set.seed(57864)

coef_data_high <- 
  gen_coefs_par(
    B = b, 
    geo_xy = xy,
    sp_range = Range,
    psill_merror = Error_large
    )

saveRDS(coef_data_high, here("Data/", paste0('coefficients_sprange_',Range,'_high_error.rds')))


