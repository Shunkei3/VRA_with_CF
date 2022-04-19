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

# === Prices ===#
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]

# === Load Field data === #
field <- 
	here("Data/analysis_field.rds") %>%
	readRDS()

# === Load coef datasets === #
coef_data <- 
	here("Data/coefficients_sprange_400.rds") %>%
	readRDS()

#/*---------------------------------------*/
#' ## (1)-1 Generate Cell-level datasets
#/*---------------------------------------*/
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
test_raw_data <- sapply(raw_data,"[",2)%>%rbindlist()

# --- NOTE: cell-level data will be used for CNN simulations --- #
saveRDS(reg_raw_data, here("Data/reg_raw_data.rds"))
saveRDS(test_raw_data, here("Data/test_raw_data.rds"))


#/*----------------------------------------------------*/
#' ## (1)-2 Generate subplot-level analysis datasets
#/*----------------------------------------------------*/
reg_raw_data <- 
	here("Data/reg_raw_data.rds") %>%
	readRDS()

test_raw_data <- 
	here("Data/test_raw_data.rds") %>%
	readRDS()

##== Aggregate Cell-level Field Data to Subplot-level Field Data ==##
sim_data <- lapply(
	1:1000, function(x) {
		# print(paste0("working on ", x, " th iteration."))
		prepare_data_for_sim(
			reg_raw_data = reg_raw_data[sim==x,],
			test_raw_data = test_raw_data[sim==x,]
		)
	}
)

reg_data <- sapply(sim_data,"[",1)%>%rbindlist()
test_data <- sapply(sim_data,"[",2)%>%rbindlist()

# --- NOTE: subplot-level data will be used for RF, BRF, and CF simulations --- #
saveRDS(reg_data, here("Data/reg_data.rds"))
saveRDS(test_data, here("Data/test_data.rds"))








