# ==========================================================================
# Objective:
#' + Calculate profit-deficits and RMSE of EONR 
#' + Calculate RMSE of yield predictions for RF, BRF and CNN
#'      + For CNN results, estimate  EONR in this codes
#' + Finally, put together those results into one data set
# ==========================================================================

#/*----------------------------------*/
#' ## Preparation 
#/*----------------------------------*/
library(here)
library(data.table)
library(RColorBrewer)
library(patchwork)
library(ggplot2)
library(ggthemes)
library(viridis)
library(hrbrthemes)
library(tidyverse)
library(ggpubr)
library(here)

# === Source Functions === #
source(here("Codes/0_1_functions_gen_analysis_data.R"))
source(here("Codes/0_2_functions_main_sim.R"))

# === Prices ===#
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]


# === Function for RMSE Calculation === #
rmse_general <-function(preds,actual){ 
  sqrt(mean((actual - preds)^2))
}

# === Load Training and Testing Data Sets for Evaluation === #
reg_data_all <- readRDS(here("Data/reg_data.rds"))
test_data_all <- readRDS(here("Data/test_data.rds"))

source_dt <- 
    bind_rows(reg_data_all, test_data_all, .id = "type") %>%
    .[padding==1, .(sim, type, unique_subplot_id, alpha, beta, ymax, rate, yield, opt_N)] %>%
    .[,type := case_when(type == "1" ~ "train", type == "2" ~ "test")]

# === Unique_subplot_id in a field === #
subplots_infiled <- source_dt[,unique_subplot_id] %>% unique()


# ==========================================================================
# 1. Results of RF, BRF and CF
# ==========================================================================

# /*=================================================*/
#' # Load and organize the Forest results
# /*=================================================*/
# === Load Forest Results === #
ls_res_forest <- 
    list.files(
        path = here("Data/Forest_rawRes"), 
        full.names = TRUE
    ) %>%
    gtools::mixedsort(decreasing = TRUE)


forest_simRes_all <- 
    lapply(ls_res_forest, readRDS) %>%
    rbindlist(., idcol = "Model") %>%
    .[,Model := case_when(
        Model == 1 ~ "aby",
        Model == 2 ~ "abytt",
        Model == 3 ~ "aabbyy",
        Model == 4 ~ "aabbyytt"
        )] %>%
    .[, Model := factor(Model, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]%>%
    .[, Method := factor(Method, levels = c("RF", "BRF", "CF_base"))] %>%
    .[, .(type, sim, Model, Method, unique_subplot_id, pred_yield, opt_N_hat, yield, opt_N)]


# /*=================================================*/
#' # RMSE of EONRs and Profit-deficits calculation
# /*=================================================*/
forest_optN_piLoss <- 
    source_dt[, !c("rate", "yield", "opt_N")] %>%
    forest_simRes_all[, on=c("sim", "type", "unique_subplot_id")] %>%
    .[,`:=`(
        max_pi = pCorn*gen_yield_MB(ymax, alpha, beta, opt_N) - pN*opt_N,
        pi =  pCorn*gen_yield_MB(ymax, alpha, beta, opt_N_hat) - pN*opt_N_hat)] %>%
    .[, pi_loss := max_pi - pi]

# === Summarize by Simulation Round === #
forest_summary_bySim <- 
    forest_optN_piLoss %>%
    .[,.(
        rmse_optN = rmse_general(opt_N_hat, opt_N),
        pi_loss = mean(pi_loss),
        rmse_y = rmse_general(pred_yield, yield)
    ), by= .(sim, type, Method, Model)]

# === Summarize by Method and Model === #
forest_summary_bySim %>%
    .[, .(
        mean_rmse_optN = mean(rmse_optN),
        mean_pi_loss = mean(pi_loss),
        mean_rmse_y = mean(rmse_y)
        ), by=.(type, Method, Model)] %>%
    .[order(type, Method)]



# ==========================================================================
# 2.  Results of CNN
# ==========================================================================

# /*=================================================*/
#' # Load and organize CNN results
# /*=================================================*/

# === Load CNN results === #
# --- CNN results on the training data sets --- #
res_cnn_onTrain <-
    list.files(
        path = here("Data/CNN_rawRes_onTrain"), 
        full.names = TRUE
    ) %>% 
    lapply(., fread) %>%
    rbindlist(., idcol = "Model") %>%
    .[, type := "train"]

# --- CNN results on the evaluation data sets --- #
res_cnn_onEval <- 
    list.files(
        path = here("Data/CNN_rawRes_onEval"),
        full.names=TRUE
    ) %>%
    lapply(., fread) %>%
    rbindlist(.,idcol = "Model") %>%
    .[, type := "test"]


# === Combine to one data set=== #
cnn_simRes_all <- 
    rbind(res_cnn_onTrain, res_cnn_onEval) %>%
    .[,Model := case_when(
        Model == 1 ~ "aby",
        Model == 2 ~ "abytt",
        Model == 3 ~ "aabbyy",
        Model == 4 ~ "aabbyytt"
    )] %>%
    .[, Model:=factor(Model, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))] %>%
    setnames("pred", "pred_yield") %>%
    # --- creation of unique_subplot_id--- #    
    .[, c("subplot_id", "strip_id") := tstrsplit(id, "_", fixed=TRUE)] %>%
    .[,unique_subplot_id := paste0(strip_id,"_",subplot_id)] %>%
    .[unique_subplot_id %in% subplots_infiled,]



# /*=================================================*/
#' # Estimate EONR 
# /*=================================================*/

#/*----------------------------------*/
#' ## Visualize CNN's site-specific yield response function
#/*----------------------------------*/
# Confirm that the predicted yield response functions are always linear
cnn_simRes_temp <- 
    cnn_simRes_all %>%
    .[type=="test" & sim==1 & unique_subplot_id %in% subplots_infiled[1:100],]

ggplot(cnn_simRes_temp, aes(x=rate, y=pred_yield, colour=factor(unique_subplot_id))) +
    geom_point(size = 0.7) +
    geom_smooth(method=lm, se=FALSE, size=0.3) +
    facet_wrap(~Model, ncol = 2) +
    theme(
        legend.title = element_blank(),
        legend.position = "none"
    )


#/*----------------------------------*/
#' ## EONR estimation
#/*----------------------------------*/
# ' Description: 
#' + CNN's yield response function is always linear
#' + In this case, solving profit maximizing problem is equivalent to 
#' doing the following things
#' (1) for each subplot, get the slope coefficient (i.e., slope)
#' (2) Check the sign of slope - w/p (w is N price, and p is corn price)
#' + if slope - w/p > 0, the EONR for the subplot is the maximum of the experimental N rate
#' + otherwise, the EONR is the minimum of the experimental N rate


# === Get Slope of the Yield Response Function === #
all_var_case <- c("aby", "abytt", "aabbyy", "aabbyytt")

cal_slope <- function(case){
    # case="aby"
    tmep <- cnn_simRes_all%>%
    .[Model == case, ]%>%
    .[, .(slope = coef(lm(pred_yield~rate))["rate"]), by= .(type, Model, sim, unique_subplot_id)]
}

slope_dt <- mclapply(all_var_case, cal_slope, mc.cores=detectCores()-2)%>%
    rbindlist()

# === EONR Estimation === #
pN_pC_ratio <- pN/pCorn

cnn_optN_dt <- 
    slope_dt %>%
    cnn_simRes_all[., on = c("type", "Model", "sim", "unique_subplot_id")] %>%
    .[, opt_N_hat :=
        lapply(.SD, function(x) ifelse(slope > pN_pC_ratio, max(rate), min(rate)))
            , by=.(type, Model, sim, unique_subplot_id)]

# /*=================================================*/
#' # RMSE of EONRs and Profit-deficits Calculation
# /*=================================================*/
cnn_optN_piLoss <- 
    source_dt %>%
    cnn_optN_dt[, on = c("type", "sim", "unique_subplot_id", "rate"), nomatch = NULL] %>%    
    .[,`:=`(
        max_pi = pCorn*gen_yield_MB(ymax, alpha, beta, opt_N) - pN*opt_N,
        pi =  pCorn*gen_yield_MB(ymax, alpha, beta, opt_N_hat) - pN*opt_N_hat)] %>%
    .[, pi_loss := max_pi - pi]


# === Summarize by Simulation Round === #
cnn_summry_bySim <- 
    cnn_optN_piLoss %>%
    .[,.(
        rmse_optN = rmse_general(opt_N_hat, opt_N),
        pi_loss = mean(pi_loss),
        rmse_y = rmse_general(pred_yield, yield)
    ), by= .(sim, type, Model)] %>%
    .[, Method := "CNN"] %>%
    .[,.(sim, type, Method, Model, rmse_optN, pi_loss, rmse_y)]

# === Check Summary of the CNN Results === #
cnn_summry_bySim %>%
    .[, .(
        mean_rmse_optN = mean(rmse_optN),
        mean_pi_loss = mean(pi_loss),
        mean_rmse_y = mean(rmse_y)
        ), by=.(type, Model)] %>%
    .[order(type)]



# ==========================================================================
# Merge Forest results and CNN results
# ==========================================================================
# === Merge === #
allML_summary_bySim <-
  rbind(forest_summary_bySim, cnn_summry_bySim) %>%
  .[, Method := factor(Method, levels = c("RF", "BRF", "CNN", "CF_base"))] %>%
  .[, Model := factor(Model, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]

saveRDS(allML_summary_bySim, here("Data/allML_summary_bySim.rds"))



