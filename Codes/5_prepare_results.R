# ==========================================================================
# Prepare field data set for writing
# ==========================================================================
#' Objective:
#' + create field data sets by cells, subplots, and plots
library(here)
library(sf)
library(data.table)
library(ggplot2)
library(ggthemes)
library(viridis)
library(tidyverse)
library(ggpubr)


# === Load Functions === #
source(here("Codes/0_1_functions_gen_analysis_data.R"))

# === Prices === #
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]

# === Field === #
field <- readRDS(here("Data/analysis_field.rds"))

# ==========================================================================
# Field Data Sets
# ==========================================================================

#/*----------------------------------*/
#' ## (1) cell-level data set 
#/*----------------------------------*/
# + NOTE: I needed to do the following things, because the existing raw data does not have m_error
coef_data <- readRDS(here("Data/coefficients_sprange_400.rds"))

x=1
coef_data_m <- coef_data[sim == x, ]
coef_data_t <- coef_data[sim == ifelse(x + 1 >= max(sim), 1, x + 1), ]

sample_dt_cell <- 
  prepare_raw_data(
    x, field=field, coef_data_m=coef_data_m, coef_data_t=coef_data_t, app_error="no"
    )

sample_train_cell_dt <- sample_dt_cell$reg_raw_data
sample_test_cell_dt <- sample_dt_cell$test_raw_data

field_cell_sf <- 
  left_join(dplyr::select(field, unique_cell_id), sample_train_cell_dt, by="unique_cell_id")%>%
    na.omit() %>%
    mutate(plot_id = ceiling(subplot_id/4)) %>%
    filter(padding==1)
   
saveRDS(field_cell_sf, here("Data/for_writing/sample_field_cell_sf.rds"))

#/*----------------------------------*/
#' ## (2) subplot-level field without padding area 
#/*----------------------------------*/

field_subplot_sf <- 
  field_cell_sf %>%
  group_by(subplot_id, strip_id) %>%
  summarise(
    sim = mean(sim),
      yield = mean(yield),
      opt_N = mean(opt_N),
      rate = mean(rate),
      aa_n = mean(aa_n), 
      alpha = mean(alpha),
      beta = mean(beta),
      ymax = mean(ymax),
      alpha1 = mean(alpha1),
      alpha2 = mean(alpha2),
      beta1 = mean(beta1),
      beta2 = mean(beta2),
      ymax1 = mean(ymax1),
      ymax2 = mean(ymax2),
      theta_1 = mean(theta_1),
      theta_2 = mean(theta_2)
  ) %>%
  mutate(unique_subplot_id = paste0(strip_id,"_",subplot_id))%>%
  select(!c(strip_id, subplot_id))

saveRDS(field_subplot_sf, here("Data/for_writing/sample_field_subplot_sf.rds"))


field_subplot_test_dt <- 
  sample_test_cell_dt %>%
  .[padding==1,] %>%
  .[,.(
    sim = mean(sim),
    yield = mean(yield),
    opt_N = mean(opt_N),
    rate = mean(rate),
    aa_n = mean(aa_n), 
    alpha = mean(alpha),
    beta = mean(beta),
    ymax = mean(ymax),
    m_error = mean(m_error),
    alpha1 = mean(alpha1),
    alpha2 = mean(alpha2),
    beta1 = mean(beta1),
    beta2 = mean(beta2),
    ymax1 = mean(ymax1),
    ymax2 = mean(ymax2),
    theta_1 = mean(theta_1),
    theta_2 = mean(theta_2)
    ), by = .(subplot_id, strip_id)
  ] %>%
  .[,unique_subplot_id := paste0(strip_id,"_",subplot_id)] %>%
  .[, `:=` (strip_id=NULL, subplot_id=NULL)]

saveRDS(field_subplot_test_dt, here("Data/for_writing/field_subplot_test_dt.rds"))

#/*----------------------------------*/
#' ## (3) plot-level dataset without padding area  
#/*----------------------------------*/
field_plot_sf <- field_cell_sf%>%
    group_by(plot_id, strip_id, rate)%>%
    summarise()

saveRDS(field_plot_sf, here("Data/for_writing/sample_field_plot_sf.rds"))


# ==========================================================================
# Preparation: True yield vs predicted yield =
# ==========================================================================

#/*--------------------------------*/
#' ## Preparation
#/*--------------------------------*/
# === Prepare Actual Yield Data set === #
test_data_all <- 
  here("Data/test_data.rds") %>%
  readRDS() %>%
  .[padding==1, .(sim, unique_subplot_id, alpha, beta, ymax, rate, yield)] %>%
  .[,det_yield := gen_yield_MB(ymax, alpha, beta, rate)]

# === Unique_subplot_id in a field === #
subplots_infiled <- test_data_all[,unique_subplot_id] %>% unique()

# === Load Forest Results === #
ls_res_forest <- 
    list.files(
        path = here("Data/Forest_rawRes"), 
        full.names = TRUE
    ) %>%
    gtools::mixedsort(decreasing = TRUE)

forest_simRes_test_raw <- 
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
    .[type == "test" & Method %in% c("RF", "BRF")]

forest_simRes_test <-
  test_data_all %>%
  forest_simRes_test_raw[, on = c("sim", "unique_subplot_id"), nomatch = NULL] %>%
  .[, .(sim, Model, Method, unique_subplot_id, pred_yield, yield, det_yield)]  


# === Load CNN Results === #
cnn_simRes_test_raw <- 
    list.files(
        path = here("Data/CNN_rawRes_onEval"),
        full.names=TRUE
    ) %>%
    lapply(., fread) %>%
    rbindlist(.,idcol = "Model") %>%
    .[,Model := case_when(
        Model == 1 ~ "aby",
        Model == 2 ~ "abytt",
        Model == 3 ~ "aabbyy",
        Model == 4 ~ "aabbyytt"
    )] %>%
    .[, Model:=factor(Model, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))] %>%
    setnames("pred", "pred_yield") %>%
    # --- creation of unique_subplot_id--- #    
    .[,c("subplot_id", "strip_id") := tstrsplit(id, "_", fixed=TRUE)] %>%
    .[,unique_subplot_id := paste0(strip_id,"_",subplot_id)] %>%
    .[unique_subplot_id %in% subplots_infiled,] %>%
    .[,Method := "CNN"] %>%


# --- select rows matching with test data --- #
# For each subplot in cnn_simRes_test_raw data, five predicted yields per each N experiment rates in that round is contained.
# So, we want to select only rows where N rate matches with N rate in testing data.
cnn_simRes_test <-
  test_data_all %>%
  cnn_simRes_test_raw[, on = c("sim", "unique_subplot_id", "rate"), nomatch = NULL] %>%
  .[, .(sim, Model, Method, unique_subplot_id, pred_yield, yield, det_yield)]  


# === Combine Forest results and CNN results === #
simRes_test_y <- rbind(forest_simRes_test, cnn_simRes_test) %>%
  .[, rmse_y := rmse_general(pred_yield, yield), by= .(sim, Method, Model) ]

saveRDS(simRes_test_y, here("Data/for_writing/simRes_test_y.rds"))

#/*--------------------------------*/
#' ## Visualization
#/*--------------------------------*/
x <- 100

sample_simRes_test_y <-
  simRes_test_y %>%
  .[sim==x]

saveRDS(sample_simRes_test_y, here("Data/for_writing/sample_simRes_test_y.rds"))


# ==========================================================================
# Cell-level Field dataset (Low-error) =
# ==========================================================================
field_cell_low <- 
  here("Data/reg_raw_data_low_error.rds") %>%
  readRDS()

sample_field_cell_low <- field_cell_low[sim==4,]

field_cell_low_sf <- 
  left_join(dplyr::select(field, unique_cell_id), sample_field_cell_low, by="unique_cell_id")%>%
    na.omit() %>%
    filter(padding==1)

# ggplot(field_cell_low_sf) +
#   geom_sf(aes(fill = yield_error), size = 0) +
#   scale_fill_viridis_c() +
#   labs(fill = expression(paste(~epsilon, " (kg/ha)"))) +
#   ggtitle("(1) Low error")

saveRDS(field_cell_low_sf, here("Data/for_writing/sample_field_cell_low_sf.rds"))


# ==========================================================================
# Cell-level Field dataset (High-error) =
# ==========================================================================
field_cell_high <- 
  here("Data/reg_raw_data_high_error.rds") %>%
  readRDS()

sample_field_cell_high <- field_cell_high[sim==3,]

field_cell_high_sf <- 
  left_join(dplyr::select(field, unique_cell_id), sample_field_cell_high, by="unique_cell_id")%>%
    na.omit() %>%
    filter(padding==1)

# ggplot(field_cell_high_sf) +
#   geom_sf(aes(fill = yield_error), size = 0) +
#   scale_fill_viridis_c() +
#   labs(fill = expression(paste(~epsilon, " (kg/ha)"))) +
#   ggtitle("(1) Low error")

saveRDS(field_cell_high_sf, here("Data/for_writing/sample_field_cell_high_sf.rds"))










