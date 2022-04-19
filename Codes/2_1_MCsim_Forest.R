# ==========================================================================
# # /*===== Run on R GUI =====*/
# ==========================================================================

#/*----------------------------------*/
#' ## Preparation
#/*----------------------------------*/
library(grf)
library(data.table)
library(tidyverse)
library(future.apply)

# === Source Functions === #
# NOTE: Please change the path in setwd()
setwd("~/Dropbox/ResearchProject/CF_for_VRA/VRA_with_CF")
source("./Codes/0_2_functions_main_sim.R")

# /*----------------------------------*/
#' ## Datasets
# /*----------------------------------*/
# === prices ===#
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]


# /*=================================================*/
#' # Simulation by scenarios
# /*=================================================*/
# === set up for parallel computations === #
plan(multicore, workers = availableCores()-2)
options(future.globals.maxSize= 850*1024^2)

# --- modeling scenario --- #
var_ls_variations <- list(
    c("alpha", "beta", "ymax"),
    c("alpha", "beta", "ymax", "theta_1", "theta_2"),
    c("alpha1", "alpha2", "beta1", "beta2", "ymax1", "ymax2"),
    c("alpha1", "alpha2", "beta1", "beta2", "ymax1", "ymax2", "theta_1", "theta_2")
  )

# --- Number of iterations --- #
B=1000

# --- Data --- #
reg_data_all <- readRDS("./Data/reg_data.rds")
test_data_all <- readRDS("./Data/test_data.rds")

# === start simulations === #
dir.create("./Data/Forest_rawRes")
for (var in var_ls_variations){
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
