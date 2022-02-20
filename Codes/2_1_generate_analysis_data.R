#/*----------------------------------*/
#' ## Preparation
#/*----------------------------------*/
library(here)
library(mgcv)
library(ggplot2)
library(grf)
library(sf)
library(measurements)
library(data.table)
library(tidyverse)
library(future.apply)
library(parallel)

# === Load Functions === #
source(here("Codes/0_1_functions_gen_analysis_data.R"))

# === Load coef Data sets === #
coef_data <- readRDS(here("Data/coef_data.rds"))
field <- readRDS(here("Data/analysis_field.rds"))

# === Prices ===#
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]


# /*=================================================*/
#' # Generate Cell-levle Dataset 
# /*=================================================*/
raw_data <- lapply(
	1:1000, function(x) {
		prepare_raw_data(
			i = x,
			field = field,
			coef_data_m = coef_data[sim == x, ],
			coef_data_t = coef_data[sim == ifelse(x + 1 >= max(sim), 1, x + 1), ],
			app_error="no"
		)
	}
)


reg_raw_data <- sapply(raw_data,"[",1)%>%rbindlist()
test_raw_data <- sapply(raw_data,"[",1)%>%rbindlist()

saveRDS(reg_raw_data, here("Data/reg_raw_data.rds"))
saveRDS(test_raw_data, here("Data/test_raw_data.rds"))


# /*===========================================================*/
#' # Generate Subplot-level Data sets (for the Main Simulations)
# /*===========================================================*/
##== Aggregate Cell-Level Field Data to Subplot-Level Field Data ==##
sim_data <- lapply(
	1:1000, function(x) {
		prepare_data_for_sim(
			reg_raw_data = reg_raw_data[sim==x,],
			test_raw_data = test_raw_data[sim==x,]
		)
	}
)

reg_data <- sapply(sim_data,"[",1)%>%rbindlist()
test_data <- sapply(sim_data,"[",2)%>%rbindlist()

saveRDS(reg_data, here("Data/reg_data.rds"))
saveRDS(test_data, here("Data/test_data.rds"))




