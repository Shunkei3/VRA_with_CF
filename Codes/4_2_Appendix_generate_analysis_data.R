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


# /*===========================================*/
#'=  (1) Small degree of yield error 
# /*===========================================*/
# === Load coef datasets === #
coef_data_low <- 
	here("Data/coefficients_sprange_400_low_error.rds") %>%
	readRDS()

#/*---------------------------------------*/
#' ## (1)-1 Generate Cell-level datasets
#/*---------------------------------------*/
raw_data <- lapply(
	1:100, function(x) {
		prepare_raw_data(
			i = x,
			field = field,
			coef_data_m = coef_data_low[sim == x, ],
			coef_data_t = coef_data_low[sim == ifelse(x + 1 >= max(sim), 1, x + 1), ],
			app_error="no"
		)
	}
)

reg_raw_data <- sapply(raw_data,"[",1)%>%rbindlist()
test_raw_data <- sapply(raw_data,"[",2)%>%rbindlist()

saveRDS(reg_raw_data, here("Data/reg_raw_data_low_error.rds"))
saveRDS(test_raw_data, here("Data/test_raw_data_low_error.rds"))

#/*----------------------------------------------------*/
#' ## (1)-2 Generate subplot-level analysis datasets
#/*----------------------------------------------------*/
##== Aggregate Cell-level Field Data to Subplot-level Field Data ==##
sim_data <- lapply(
	1:100, function(x) {
		# print(paste0("working on ", x, " th iteration."))
		prepare_data_for_sim(
			reg_raw_data = reg_raw_data[sim==x,],
			test_raw_data = test_raw_data[sim==x,]
		)
	}
)

reg_data <- sapply(sim_data,"[",1)%>%rbindlist()
test_data <- sapply(sim_data,"[",2)%>%rbindlist()

saveRDS(reg_data, here("Data/reg_data_low_error.rds"))
saveRDS(test_data, here("Data/test_data_low_error.rds"))







# /*===========================================*/
#'=  (2) Large degree of yield error
# /*===========================================*/
# === Load coef datasets === #
coef_data_high <- 
	here("Data/coefficients_sprange_400_high_error.rds") %>%
	readRDS()

#/*---------------------------------------*/
#' ## (2)-1 Generate Cell-level datasets
#/*---------------------------------------*/
raw_data <- lapply(
	1:100, function(x) {
		prepare_raw_data(
			i = x,
			field = field,
			coef_data_m = coef_data_high[sim == x, ],
			coef_data_t = coef_data_high[sim == ifelse(x + 1 >= max(sim), 1, x + 1), ],
			app_error="no"
		)
	}
)

reg_raw_data <- sapply(raw_data,"[",1)%>%rbindlist()
test_raw_data <- sapply(raw_data,"[",2)%>%rbindlist()

saveRDS(reg_raw_data, here("Data/reg_raw_data_high_error.rds"))
saveRDS(test_raw_data, here("Data/test_raw_data_high_error.rds"))


#/*----------------------------------------------------*/
#' ##  (2)-2 Generate subplot-level analysis datasets
#/*----------------------------------------------------*/
##== Aggregate Cell-Level Field Data to Subplot-Level Field Data ==##
sim_data <- lapply(
	1:100, function(x) {
		# print(paste0("working on ", x, " th iteration."))
		prepare_data_for_sim(
			reg_raw_data = reg_raw_data[sim==x,],
			test_raw_data = test_raw_data[sim==x,]
		)
	}
)

reg_data <- sapply(sim_data,"[",1)%>%rbindlist()
test_data <- sapply(sim_data,"[",2)%>%rbindlist()

saveRDS(reg_data, here("Data/reg_data_high_error.rds"))
saveRDS(test_data, here("Data/test_data_high_error.rds"))











