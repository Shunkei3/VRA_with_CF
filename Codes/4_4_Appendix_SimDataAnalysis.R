# ==========================================================================
# Objective:
# This code has two sections (1) Result analysis for simulations with low error
 # and (2) Result analysis for simulations with high error. 
# For each simulation results, 
#' + Calculate profit-deficits and RMSE of EONR 
#' + Calculate RMSE of yield predictions for RF, BRF and CNN
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
library(sf)

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


# ==========================================================================
# (1) Result analysis for simulations with low error
# ==========================================================================
# === Load Training and Testing Data Sets for Evaluation === #
reg_data_low <- readRDS(here("Data/reg_data_low_error.rds"))
test_data_low <- readRDS(here("Data/test_data_low_error.rds"))

source_dt_low <- 
    bind_rows(reg_data_low, test_data_low, .id = "type") %>%
    .[padding==1, .(sim, type, unique_subplot_id, alpha, beta, ymax, rate, yield, opt_N)] %>%
    .[,type := case_when(type == "1" ~ "train", type == "2" ~ "test")]

# === Unique_subplot_id in a field === #
subplots_infiled <- source_dt_low[,unique_subplot_id] %>% unique()


# /*===========================================*/
#'=  (1)-1 Forest regression results =
# /*===========================================*/

#/*--------------------------------*/
#' ## Load and organize the Forest results
#/*--------------------------------*/

# === Load Forest Results === #
ls_res_forest_low <- 
    list.files(
        path = here("Data/Forest_rawRes_low"), 
        full.names = TRUE
    ) %>%
    gtools::mixedsort(decreasing = TRUE)


forest_low_simRes_all <- 
    lapply(ls_res_forest_low, readRDS) %>%
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


#/*--------------------------------*/
#' ## RMSE of EONRs and Pi-loss calculation
#/*--------------------------------*/
forest_optN_piLoss_low <- 
    source_dt_low[, !c("rate", "yield", "opt_N")] %>%
    forest_low_simRes_all[, on=c("sim", "type", "unique_subplot_id")] %>%
    .[,`:=`(
        max_pi = pCorn*gen_yield_MB(ymax, alpha, beta, opt_N) - pN*opt_N,
        pi =  pCorn*gen_yield_MB(ymax, alpha, beta, opt_N_hat) - pN*opt_N_hat)] %>%
    .[, pi_loss := max_pi - pi]

# === Summarize by Simulation Round === #
forest_summary_bySim_low <- 
    forest_optN_piLoss_low %>%
    .[,.(
        rmse_optN = rmse_general(opt_N_hat, opt_N),
        pi_loss = mean(pi_loss),
        rmse_y = rmse_general(pred_yield, yield)
    ), by= .(sim, type, Method, Model)]

saveRDS(forest_summary_bySim_low, here("Data/for_writing/forest_summry_bySim_low.rds"))

# === Check Summary of the Results of RF, BRF, and CF === #
# forest_summary_bySim_low %>%
#     .[, .(
#         mean_rmse_optN = mean(rmse_optN),
#         mean_pi_loss = mean(pi_loss),
#         mean_rmse_y = mean(rmse_y)
#         ), by=.(type, Method, Model)] %>%
#     .[order(type, Method)]



# /*===========================================*/
#'=  (1)-2 CNN regression results=
# /*===========================================*/

#/*--------------------------------*/
#' ## Load CNN results
#/*--------------------------------*/
# --- CNN results on the evaluation data sets --- #
res_cnn_onEval_low <- 
    list.files(
        path = here("Data/CNN_rawRes_low"),
        pattern = "_low_error", full.names=TRUE, 
    ) %>%
    lapply(., fread) %>%
    rbindlist(.,idcol = "Model") %>%
    .[, type := "test"]


# === Organize data === #
cnn_simRes_all_low <- 
    res_cnn_onEval_low %>%
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
    .[unique_subplot_id %in% subplots_infiled,] %>%
    .[,.(sim, type, Model, unique_subplot_id, rate, pred_yield)]


#/*--------------------------------*/
#' ## Estimate EONR 
#/*--------------------------------*/
# /*===== Visualize CNN's site-specific yield response function =====*/
# # The predicted yield response functions are always linear
cnn_simRes_temp_low <- 
    copy(cnn_simRes_all_low) %>%
    .[type=="test" & Model == "aby" & sim==1 & unique_subplot_id %in% subplots_infiled[1:10],]

ggplot(cnn_simRes_temp_low, aes(x=rate, y=pred_yield, colour=factor(unique_subplot_id))) +
    geom_point(size = 1) +
    geom_line() +
    facet_wrap(~Model, ncol = 2) +
    theme(
        legend.title = element_blank(),
        legend.position = "none"
    )

# /*===== Estimate EONR =====*/
all_var_case <- c("aby", "abytt", "aabbyy", "aabbyytt")

cal_slope <- function(case){
    # case="aby"
    tmep <- cnn_simRes_all_low%>%
    .[Model == case, ]%>%
    .[, .(slope = coef(lm(pred_yield~rate))["rate"]), by= .(type, Model, sim, unique_subplot_id)]
}

slope_dt_low <- 
    lapply(all_var_case, cal_slope)%>%
    rbindlist()

pN_pC_ratio <- pN/pCorn

cnn_optN_dt_low <- 
    slope_dt_low %>%
    cnn_simRes_all_low[., on = c("type", "Model", "sim", "unique_subplot_id")] %>%
    .[, opt_N_hat :=
        lapply(.SD, function(x) ifelse(slope > pN_pC_ratio, max(rate), min(rate)))
            , by=.(type, Model, sim, unique_subplot_id)]


#/*--------------------------------------------------------*/
#' ## RMSE of EONRs and Yields and Profit-loss Calculation
#/*--------------------------------------------------------*/
cnn_summry_bySim_low <- 
    source_dt_low %>%
    cnn_optN_dt_low[, on = c("type", "sim", "unique_subplot_id", "rate"), nomatch = NULL] %>%    
    # --- profit-loss --- #
    .[,`:=`(
        max_pi = pCorn*gen_yield_MB(ymax, alpha, beta, opt_N) - pN*opt_N,
        pi =  pCorn*gen_yield_MB(ymax, alpha, beta, opt_N_hat) - pN*opt_N_hat)] %>%
    .[, pi_loss := max_pi - pi] %>%
    # --- Summarize by simulation round --- #
    .[,.(
        rmse_optN = rmse_general(opt_N_hat, opt_N),
        pi_loss = mean(pi_loss),
        rmse_y = rmse_general(pred_yield, yield)
    ), by= .(sim, type, Model)] %>%
    .[, Method := "CNN"] %>%
    .[,.(sim, type, Method, Model, rmse_optN, pi_loss, rmse_y)]

saveRDS(cnn_summry_bySim_low, here("Data/for_writing/cnn_summry_bySim_low.rds"))

# === Check Summary of the CNN Results === #
cnn_summry_bySim_low %>%
    .[, .(
        mean_rmse_optN = mean(rmse_optN),
        mean_pi_loss = mean(pi_loss),
        mean_rmse_y = mean(rmse_y)
        ), by=.(type, Model)] %>%
    .[order(type)]



# /*===========================================*/
#'=  (1)-3 Merge Forest results and CNN results =
# /*===========================================*/
allML_summary_bySim_low <- 
    rbind(forest_summary_bySim_low, cnn_summry_bySim_low) %>%
    .[, Method := factor(Method, levels = c("RF", "BRF", "CNN", "CF_base"))] %>%
    .[, Model := factor(Model, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]


saveRDS(allML_summary_bySim_low, here("Data/for_writing/allML_summary_bySim_low.rds"))



# ==========================================================================
# (2) Result analysis for simulations with low error
# ==========================================================================
# === Load Training and Testing Data Sets for Evaluation === #
reg_data_high <- readRDS(here("Data/reg_data_high_error.rds"))
test_data_high <- readRDS(here("Data/test_data_high_error.rds"))

source_dt_high <- 
    bind_rows(reg_data_high, test_data_high, .id = "type") %>%
    .[padding==1, .(sim, type, unique_subplot_id, alpha, beta, ymax, rate, yield, opt_N)] %>%
    .[,type := case_when(type == "1" ~ "train", type == "2" ~ "test")]

# === Unique_subplot_id in a field === #
subplots_infiled <- source_dt_high[,unique_subplot_id] %>% unique()

# /*===========================================*/
#'=  (2)-1 Forest regression results =
# /*===========================================*/

#/*--------------------------------*/
#' ## Load and organize the Forest results
#/*--------------------------------*/

# === Load Forest Results === #
ls_res_forest_high <- 
    list.files(
        path = here("Data/Forest_rawRes_high"), 
        full.names = TRUE
    ) %>%
    gtools::mixedsort(decreasing = TRUE)


forest_high_simRes_all <- 
    lapply(ls_res_forest_high, readRDS) %>%
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


#/*--------------------------------*/
#' ## RMSE of EONRs and Pi-loss calculation
#/*--------------------------------*/
forest_summary_bySim_high <- 
    source_dt_high[, !c("rate", "yield", "opt_N")] %>%
    forest_high_simRes_all[, on=c("sim", "type", "unique_subplot_id")] %>%
    # --- profit-loss calculation  --- #
    .[,`:=`(
        max_pi = pCorn*gen_yield_MB(ymax, alpha, beta, opt_N) - pN*opt_N,
        pi =  pCorn*gen_yield_MB(ymax, alpha, beta, opt_N_hat) - pN*opt_N_hat)] %>%
    .[, pi_loss := max_pi - pi] %>%
    # --- summarize by simulation round --- #
    .[,.(
        rmse_optN = rmse_general(opt_N_hat, opt_N),
        pi_loss = mean(pi_loss),
        rmse_y = rmse_general(pred_yield, yield)
    ), by= .(sim, type, Method, Model)]

saveRDS(forest_summary_bySim_high, here("Data/for_writing/forest_summary_bySim_high.rmd"))


# === Check Summary of the Results of RF, BRF, and CF === #
# forest_summary_bySim_high %>%
#     .[, .(
#         mean_rmse_optN = mean(rmse_optN),
#         mean_pi_loss = mean(pi_loss),
#         mean_rmse_y = mean(rmse_y)
#         ), by=.(type, Method, Model)] %>%
#     .[order(type, Method)]





# /*===========================================*/
#'=  (2)-2 CNN regression results=
# /*===========================================*/

#/*--------------------------------*/
#' ## Load CNN results
#/*--------------------------------*/
# --- CNN results on the evaluation data sets --- #
res_cnn_onEval_high <- 
    list.files(
        path = here("Data/CNN_rawRes_high"),
        pattern = "_high_error", full.names=TRUE, 
    ) %>%
    lapply(., fread) %>%
    rbindlist(.,idcol = "Model") %>%
    .[, type := "test"]


# === Organize data === #
cnn_simRes_all_high <- 
    res_cnn_onEval_high %>%
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
    .[unique_subplot_id %in% subplots_infiled,] %>%
    .[,.(sim, type, Model, unique_subplot_id, rate, pred_yield)]


#/*--------------------------------*/
#' ## Estimate EONR 
#/*--------------------------------*/
# /*===== Visualize CNN's site-specific yield response function =====*/
# # The predicted yield response functions are always linear
cnn_simRes_temp_high <- 
    copy(cnn_simRes_all_high) %>%
    .[type=="test" & Model == "aby" & sim==1 & unique_subplot_id %in% subplots_infiled[1:10],]

ggplot(cnn_simRes_temp_high, aes(x=rate, y=pred_yield, colour=factor(unique_subplot_id))) +
    geom_point(size = 1) +
    geom_line() +
    facet_wrap(~Model, ncol = 2) +
    theme(
        legend.title = element_blank(),
        legend.position = "none"
    )

# /*===== Estimate EONR =====*/
all_var_case <- c("aby", "abytt", "aabbyy", "aabbyytt")

cal_slope <- function(case){
    # case="aby"
    tmep <- cnn_simRes_all_high%>%
    .[Model == case, ]%>%
    .[, .(slope = coef(lm(pred_yield~rate))["rate"]), by= .(type, Model, sim, unique_subplot_id)]
}

slope_dt_high <- 
    lapply(all_var_case, cal_slope)%>%
    rbindlist()

pN_pC_ratio <- pN/pCorn

cnn_optN_dt_high <- 
    slope_dt_high %>%
    cnn_simRes_all_high[., on = c("type", "Model", "sim", "unique_subplot_id")] %>%
    .[, opt_N_hat :=
        lapply(.SD, function(x) ifelse(slope > pN_pC_ratio, max(rate), min(rate)))
            , by=.(type, Model, sim, unique_subplot_id)]


#/*--------------------------------------------------------*/
#' ## RMSE of EONRs and Yields and Profit-loss Calculation
#/*--------------------------------------------------------*/
cnn_summry_bySim_high <- 
    source_dt_high %>%
    cnn_optN_dt_high[, on = c("type", "sim", "unique_subplot_id", "rate"), nomatch = NULL] %>%    
    # --- profit-loss --- #
    .[,`:=`(
        max_pi = pCorn*gen_yield_MB(ymax, alpha, beta, opt_N) - pN*opt_N,
        pi =  pCorn*gen_yield_MB(ymax, alpha, beta, opt_N_hat) - pN*opt_N_hat)] %>%
    .[, pi_loss := max_pi - pi] %>%
    # --- Summarize by simulation round --- #
    .[,.(
        rmse_optN = rmse_general(opt_N_hat, opt_N),
        pi_loss = mean(pi_loss),
        rmse_y = rmse_general(pred_yield, yield)
    ), by= .(sim, type, Model)] %>%
    .[, Method := "CNN"] %>%
    .[,.(sim, type, Method, Model, rmse_optN, pi_loss, rmse_y)]

saveRDS(cnn_summry_bySim_high, here("Data/for_writing/cnn_summry_bySim_high.rds"))

# === Check Summary of the CNN Results === #
# cnn_summry_bySim_high %>%
#     .[, .(
#         mean_rmse_optN = mean(rmse_optN),
#         mean_pi_loss = mean(pi_loss),
#         mean_rmse_y = mean(rmse_y)
#         ), by=.(type, Model)] %>%
#     .[order(type)]



# /*===========================================*/
#'=  (1)-3 Merge Forest results and CNN results =
# /*===========================================*/
allML_summary_bySim_high <- 
    rbind(forest_summary_bySim_high, cnn_summry_bySim_high) %>%
    .[, Method := factor(Method, levels = c("RF", "BRF", "CNN", "CF_base"))] %>%
    .[, Model := factor(Model, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]


saveRDS(allML_summary_bySim_high, here("Data/for_writing/allML_summary_bySim_high.rds"))


















