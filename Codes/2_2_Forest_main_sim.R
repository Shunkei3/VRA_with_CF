# /*===== Run on R GUI =====*/

#/*----------------------------------*/
#' ## Preparation
#/*----------------------------------*/
library(here)
library(grf)
library(data.table)
library(tidyverse)
library(future.apply)

# === Load Functions === #
setwd(here())
source("./Codes/0_2_functions_main_sim.R")

# === Load Data Sets === #
reg_data_all <- readRDS("./Data/reg_data.rds")
test_data_all <- readRDS("./Data/test_data.rds")


# /*=================================================*/
#' # Simulation by scenarios
# /*=================================================*/
# + run 1000 simulations by individual scenarios
# + the 1000 simulation results will be saved per each scenario

# === Set up for Parallel Computations === #
# --- Parallel implementation using the future_lapply --- #
plan(multicore, workers = availableCores()-2)
options(future.globals.maxSize= 850*1024^2)

# --- Prices--- #
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]

# --- Modeling Scenario --- #
var_ls_variations <- list(
    c("alpha", "beta", "ymax"),
    c("alpha", "beta", "ymax", "theta_1", "theta_2"),
    c("alpha1", "alpha2", "beta1", "beta2", "ymax1", "ymax2"),
    c("alpha1", "alpha2", "beta1", "beta2", "ymax1", "ymax2", "theta_1", "theta_2")
  )

# --- Number of iterations --- #
B=1000

# === Start Simulations === #
for (var in var_ls_variations){
	# var = c("alpha", "beta", "ymax")
	set.seed(1378)
	# --- for each modeling scenario run: --- #
	sim_results <- lapply(1:B, 
		function(x){
			sim_par(
				i = x,
				var_ls = var,
				reg_data = reg_data_all[sim==x&padding==1,],
	      		test_data = test_data_all[sim==x&padding==1,],
	      		N_levels = reg_data_all[sim==x,]$rate%>%unique()%>%sort()
	      	)
		}
	) %>%
	rbindlist()

	saveRDS(sim_results, paste0("./Data/Forest_rawRes/forest_SimRes_", paste0(var, collapse = "_"), ".rds"))
}


