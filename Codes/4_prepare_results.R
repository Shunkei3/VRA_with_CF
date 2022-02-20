# ==========================================================================
# Prepare field data set for writing
# ==========================================================================
#' Objective:
#' + create field data sets by cells, subplots, and plots
#' + create a figure of an example causal tree 
library(here)
library(sf)
library(data.table)
library(ggplot2)
library(ggthemes)
library(viridis)
library(tidyverse)
library(ggpubr)
library(grf)

source("Codes/0_1_functions_gen_analysis_data.R")


pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]


# /*=================================================*/
#' # Field Data Sets
# /*=================================================*/

#/*----------------------------------*/
#' ## (1) cell-level data set 
#/*----------------------------------*/
# + NOTE: I needed to do the following things, because the existing raw data does not have m_error
field <- readRDS(here("Data/analysis_field.rds"))

coef_data <- readRDS(here("Data/coef_data.rds"))

x=1
coef_data_m <- coef_data[sim == x, ]
coef_data_t <- coef_data[sim == ifelse(x + 1 >= max(sim), 1, x + 1), ]

sample_dt_cell <- prepare_raw_data(x, field=field, coef_data_m=coef_data_m, coef_data_t=coef_data_t, app_error="no") %>%
  .$reg_raw_data

field_cell_sf <- 
  left_join(select(field, unique_cell_id), sample_dt_cell, by="unique_cell_id")%>%
    na.omit() %>%
    mutate(plot_id = ceiling(subplot_id/4)) %>%
    filter(padding==1)
   
saveRDS(field_cell_sf, here("Data/sample_field_cell_sf.rds"))


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

saveRDS(field_subplot_sf, here("Data/sample_field_subplot_sf.rds"))


#/*----------------------------------*/
#' ## (3) plot-level dataset without padding area  
#/*----------------------------------*/
field_plot_sf <- field_cell_sf%>%
    group_by(plot_id, strip_id, rate)%>%
    summarise()

saveRDS(field_plot_sf, here("Data/sample_field_plot_sf.rds"))



