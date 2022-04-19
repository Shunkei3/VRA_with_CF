# /*=================================================*/
#' # Preparation
# /*=================================================*/
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
library(parallel)
library(mapedit)


# === Load Functions === #
source(here("Codes/0_1_functions_gen_analysis_data.R"))

# === Load an Actual Field Boundary Data === #
field_boundary <- readRDS(here("Data/field_boundary.rds"))

# /*=================================================*/
#' # 1. Base Field 
# /*=================================================*/

# /*----------------------------------*/
#' ## Set parameters
# /*----------------------------------*/
#--- bbox ---#
wf_bbox <- st_bbox(field_boundary)

#--- create ab-line ---#
ab_line <- rbind(
  c(wf_bbox["xmin"], wf_bbox["ymin"]),
  c(wf_bbox["xmin"], wf_bbox["ymax"])
) %>%
  st_linestring()

starting_point <- c(wf_bbox["xmin"] - 100, wf_bbox["ymin"] - 100)

# use 60 ft design with 30 ft yield monitor
polygon_width <- conv_unit(60, "ft", "m")
cell_height <- conv_unit(10, "ft", "m")

# /*----------------------------------*/
#' ## Create polygons
# /*----------------------------------*/
# group: strip id
# basic_plot_id: ids for the small polygons
# plot_id: plot id

all_grids <- 
  make_trial_grids(
    field = field_boundary,
    ab_line = ab_line,
    starting_point = starting_point,
    plot_width = polygon_width,
    cell_height = cell_height
  )

field_base <- 
  all_grids %>%
  data.table() %>%
  .[, dummy := 1] %>%
  .[, cell_id := cumsum(dummy), by = strip_id] %>%
  .[, num_obs := .N, by = .(strip_id, plot_id)] %>%
  #--- get rid of short plots ---#
  .[num_obs == median(num_obs), ] %>%
  st_as_sf()

# /*=================================================*/
#' # 2. Expand the Base Filed by adding padding (for CNN)
# /*=================================================*/

#/*----------------------------------*/
#' ## Preparation: Expand field_base
#/*----------------------------------*/
# --- vertical line --- #
strip_bbox <- 
  field_base%>%
  filter(strip_id==1)%>%
  st_bbox(strip)

strip_width <- strip_bbox["xmax"] - strip_bbox["xmin"]


# --- horizontal line --- #
plot_bbox <- 
  field_base%>%
  filter(plot_id==1)%>%
  st_bbox()

plot_length <- plot_bbox["ymax"]-plot_bbox["ymin"]


#-- bbox of field_base  --#
bbox_base <- st_bbox(field_base) 


#-- expand the bbox --#
bbox_base[1] <- bbox_base["xmin"] - strip_width #xmin
bbox_base[3] <- bbox_base["xmax"] + strip_width #xmax
bbox_base[2] <- bbox_base["ymin"] - plot_length #ymin
bbox_base[4] <- bbox_base["ymax"] + plot_length #ymax

field_expd <- st_as_sfc(bbox_base)%>%st_as_sf()

# /*----------------------------------*/
#' ## Set parameters
# /*----------------------------------b */
#--- new bbox ---#
wf_bbox_expd <- st_bbox(field_expd)

#--- create ab-line ---#
ab_line_expd <- rbind(
  c(wf_bbox_expd["xmin"], wf_bbox_expd["ymin"]),
  c(wf_bbox_expd["xmin"], wf_bbox_expd["ymax"])
) %>%
  st_linestring()

starting_point_expd <- c(wf_bbox_expd["xmin"], wf_bbox_expd["ymin"])

# /*----------------------------------*/
#' ## Create polygons
# /*----------------------------------*/
all_grids_expd <- 
  make_trial_grids(
    field = field_expd,
    ab_line = ab_line_expd,
    starting_point = starting_point_expd,
    plot_width = polygon_width,
    cell_height = cell_height
  )

# === create cell_id and subplot_id === #
field_expd <- 
  all_grids_expd %>%
  data.table() %>%
  .[, dummy := 1] %>%
  .[, cell_id := cumsum(dummy), by = strip_id] %>%
  .[, num_obs := .N, by = .(strip_id, plot_id)] %>%
  #--- get rid of short plots ---#
  .[num_obs == median(num_obs), ] %>%
  # --- subplot_id and unique_cell_id --- #
  .[, subplot_id := ceiling(cell_id / 6), by = strip_id] %>% 
  .[, unique_cell_id := paste0(cell_id, "_",strip_id)] %>%
  .[!(strip_id==max(strip_id)),] %>%
  st_as_sf()

# /*====================================================*/
#' 3. Divide each subplot in field_expd into 6 cells
# /*====================================================*/
cell_list <- unique(field_expd$unique_cell_id)

field_grid_within_subplot <- 
  lapply(1:length(cell_list),
    function(x) 
      make_grid_within_subplot(
        field_base = field_expd,
        cell = cell_list[[x]])
  )%>%
  mapedit:::combine_list_of_sf()

# --- Merge with field_expd --- #
analysis_field_raw <- 
  left_join(
    field_grid_within_subplot, st_drop_geometry(field_expd), by="unique_cell_id")%>%
    cbind(., st_coordinates(st_centroid(.)))%>%
    mutate(
      unique_subplot_id = unique_cell_id,
      unique_cell_id = paste0(unique_subplot_id,"_",cell_in_cell.id)
  )

# ggplot()+
#   geom_sf(data=field_base, fill="green", size=0, alpha=0.6) +
#   geom_sf(data=analysis_field_raw, fill=NA)

# /*========================================*/
#' 4. Create "padding" indicator
# /*========================================*/

base_bdary <- st_union(field_base)

padding_idx <- 
  analysis_field_raw %>%
  st_centroid() %>%
  dplyr::select(unique_cell_id) %>%
  mutate(
    padding = ifelse(st_within(., base_bdary, sparse=FALSE), 1, 0)
  )%>%
  st_drop_geometry()%>%
  data.table()

analysis_field <- left_join(analysis_field_raw, padding_idx, by="unique_cell_id")

saveRDS(analysis_field, here("Data/analysis_field.rds"))
