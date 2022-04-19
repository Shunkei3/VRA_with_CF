# /*=================================================*/
#' # Make experiment grids (basic cell, plot, strip)
# /*=================================================*/
make_trial_grids <- function(field, ab_line, starting_point, plot_width, cell_height) {

  # /*=================================================*/
  #' # Define functions
  # /*=================================================*/
  # /*----------------------------------*/
  #' ## make polygons
  # /*----------------------------------*/
  make_polygon <- function(strt_point_new, multiplier, direction) {
    point_1 <- strt_point_new + cell_height * ab_xy_nml * (multiplier - 1)
    point_2 <- point_1 - plot_width * direction * ab_xy_nml_p90
    point_3 <- point_2 + ab_xy_nml * cell_height
    point_4 <- point_3 + plot_width * direction * ab_xy_nml_p90

    temp_polygon <- rbind(
      point_1,
      point_2,
      point_3,
      point_4,
      point_1
    ) %>%
      list() %>%
      st_polygon()

    return(temp_polygon)
  }

  # /*----------------------------------*/
  #' ## rotation matrix
  # /*----------------------------------*/
  rotate_mat_p90 <- matrix(
    c(
      cos(pi / 2),
      sin(pi / 2),
      -sin(pi / 2),
      cos(pi / 2)
    ),
    nrow = 2
  )

  # /*----------------------------------*/
  #' ## Make an extended line
  # /*----------------------------------*/
  # make line with starting point and a desired shift
  make_extended_line <- function(starting_point, step, multiplier, step_v) {
    extended_line <- rbind(
      starting_point + step * multiplier,
      starting_point + step_v * 1000 + step * multiplier
    ) %>%
      st_linestring() %>%
      list() %>%
      st_as_sfc() %>%
      st_set_crs(st_crs(field))

    return(extended_line)
  }

  # /*----------------------------------*/
  #' ## Check intersections
  # /*----------------------------------*/
  # check the intersection of the lines in the direction specified
  check_intersection <- function(field, step, direction, step_v) {
    is_int_ls <- rep(FALSE, 100)

    for (i in 1:100) {

      #--- shift 50 meter every time ---#
      line <- make_extended_line(
        starting_point,
        plot_width * step * direction,
        i,
        step_v
      )

      is_int_ls[i] <- st_intersects(field, line, sparse = FALSE)[1, 1]

      if (is_int_ls[i]) {
        break
      }
    }

    return(is_int_ls)
  }

  # /*----------------------------------*/
  #' ## vector of points of sf of points
  # /*----------------------------------*/
  vect_to_sf_point <- function(vec) {
    st_as_sfc(list(st_point(vec))) %>%
      st_set_crs(st_crs(field))
  }

  # /*----------------------------------*/
  #' ## Re-assign plot id based on observation numbers per plot
  # /*----------------------------------*/
  reassign_plot_id <- function(data, grp) {
    temp_data <- data[group == grp, ]

    if (max(temp_data$plot_id) == 1) {
      #--- if there is only one plot_id in the strip ---#
      return(temp_data[, .(id, plot_id, group, x)])
    }

    if (nrow(temp_data[too_short == TRUE, ]) == 0) {
      return(temp_data[, .(id, plot_id, group, x)])
    }

    num_obs_short <- temp_data[too_short == TRUE, obs_per_plot] %>%
      unique()

    short_plot_id <- temp_data[too_short == TRUE, plot_id] %>%
      unique()

    num_obs_short_1 <- temp_data[plot_id == (short_plot_id - 1), obs_per_plot] %>%
      unique()

    if (num_obs_short >= (2 * min_obs - mean_obs)) { # make the last two short

      first_obs_set <- ceiling((num_obs_short + mean_obs) / 2)

      temp_data[plot_id %in% c(short_plot_id, short_plot_id - 1), cum_num_reassign := cumsum(dummy)] %>%
        .[cum_num_reassign <= first_obs_set, plot_id := short_plot_id - 1]
    } else if ((max(temp_data$plot_id) >= 3) & num_obs_short >= (3 * min_obs - 2 * mean_obs)) {

      # make the last three short (there needs to be at least 3 plot ids)

      first_obs_set <- ceiling((num_obs_short + 2 * mean_obs) / 3)

      temp_data[plot_id %in% short_plot_id:(short_plot_id - 2), cum_num_reassign := cumsum(dummy)] %>%
        #--- third last i---#i
        .[plot_id %in% short_plot_id:(short_plot_id - 2), plot_id := short_plot_id] %>%
        .[cum_num_reassign <= first_obs_set, plot_id := short_plot_id - 2] %>%
        .[cum_num_reassign > first_obs_set & cum_num_reassign <= 2 * first_obs_set, plot_id := short_plot_id - 1]
    } else if (max(temp_data$plot_id) >= 3) {

      # make the 2nd and 3rd last longer (there needs to be at least 3 plot ids)

      first_obs_set <- ceiling((num_obs_short + 2 * mean_obs) / 2)

      temp_data[plot_id %in% short_plot_id:(short_plot_id - 2), cum_num_reassign := cumsum(dummy)] %>%
        .[plot_id %in% short_plot_id:(short_plot_id - 2), plot_id := short_plot_id - 1] %>%
        #--- third last ---#
        .[cum_num_reassign <= first_obs_set, plot_id := short_plot_id - 2]
    } else {

      # make the two into one (there needs to be at least 2 plot ids)
      temp_data[, plot_id := 1]
    }

    # temp_data[, .N, by = plot_id]

    return(temp_data[, .(id, plot_id, group, x)])
  }

  # /*=================================================*/
  #' # Main code
  # /*=================================================*/
  #--- get the vector (direction machines run)  ---#
  ab_xy <- st_geometry(ab_line)[[1]][2, ] - st_geometry(ab_line)[[1]][1, ]
  #--- distance of the vector ---#
  ab_length <- sqrt(sum(ab_xy^2))
  #--- normalize (distance == 1) ---#
  ab_xy_nml <- ab_xy / ab_length
  #--- create a vector that is perpendicular to ab_xy ---#
  ab_xy_nml_p90 <- ab_xy_nml %*% rotate_mat_p90

  # /*----------------------------------*/
  #' ## identify the number of subplots in a strip
  # /*----------------------------------*/
  f_bbox <- st_bbox(field)

  #--- maximum distance ---#
  max_dist <- sqrt(
    (f_bbox["xmax"] - f_bbox["xmin"])^2 +
      (f_bbox["ymax"] - f_bbox["ymin"])^2
  ) + 50

  max_dist_cover <- ceiling(max_dist / 10) * 10

  #--- number of subplots to create ---#
  num_subplots_in_a_strip <- ceiling(max_dist_cover / cell_height)

  # /*----------------------------------*/
  #' ## Detect which direction to go
  # /*----------------------------------*/
  is_int_p <- check_intersection(field, ab_xy_nml_p90, direction = 1, ab_xy_nml)

  if (any(is_int_p)) {
    direction <- 1
    #--- how many cells do you need to move to intersects with the field ---#
    how_many_in <- which(is_int_p)
  } else {
    direction <- -1
    #--- how many cells do you need to move to intersects with the field ---#
    how_many_in <- which(check_intersection(field, ab_xy_nml_p90, -1, ab_xy_nml))
  }

  #--- refresh the starting point ---#
  strt_point_new <- starting_point + how_many_in * plot_width * direction * ab_xy_nml_p90
  strt_point_new_sf <- vect_to_sf_point(strt_point_new)

  # /*----------------------------------*/
  #' ## Create strip of polygons strip by strip
  # /*----------------------------------*/
  is_intersecting <- TRUE

  exp_sf_ls <- list()
  group <- 1

  while (is_intersecting) {
    exp_sf_ls[[paste(group)]] <- lapply(
      1:num_subplots_in_a_strip,
      function(x) {
        make_polygon(
          strt_point_new + plot_width * direction * ab_xy_nml_p90 * (group - 1),
          x,
          direction
        )
      }
    ) %>%
      st_as_sfc() %>%
      st_set_crs(st_crs(field)) %>%
      st_as_sf() %>%
      mutate(
        group = group,
        id = 1:nrow(.)
      )

    is_intersecting <- st_intersects(exp_sf_ls[[paste(group)]], field, sparse = F)[, 1] %>% any()


    group <- group + 1
  }

  all_plygons <- do.call("rbind", exp_sf_ls) %>%
    .[field, ]

  # /*----------------------------------*/
  #' ## Reassign plot id
  # /*----------------------------------*/
  # group: strip id
  # id: subplot id
  # plot_id: plot id
  min_obs <- 20 # (200 feet)
  mean_obs <- 24 # (240 feet)
  max_obs <- 30 #  (300 feet)

  data <- all_plygons %>%
    data.table() %>%
    #--- observations per strip ---#
    .[, obs_per_strip := .N, by = .(group)] %>%
    #--- (initial) plot id ---#
    .[, dummy := 1] %>%
    .[, cum_num := cumsum(dummy), by = .(group)] %>%
    .[, plot_id := (cum_num - 1) %/% mean_obs + 1, by = .(group)] %>%
    #--- max number of plots per group ---#
    .[, max_plot_id := max(plot_id), by = .(group, plot_id)] %>%
    #--- number of subplots per plot ---#
    .[, obs_per_plot := .N, by = .(group, plot_id)] %>%
    .[, too_short := obs_per_plot <= min_obs]

  group_ls <- data$group %>% unique()

  final_ploygons <- lapply(group_ls, function(x) reassign_plot_id(data, x)) %>%
    rbindlist() %>%
    st_as_sf() %>%
    rename(geometry = x) %>%
    rename(strip_id = group) %>%
    mutate(cell_id := 1:nrow(.)) %>%
    dplyr::select(-id)

  return(final_ploygons)
}


# /*=================================================*/
#' # Tilt the field
# /*=================================================*/

st_tilt <- function(data_sf, angle) {
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

  wf_bbox <- st_bbox(data_sf)
  data_geom <- st_geometry(data_sf)

  base_point <- c(wf_bbox["xmax"], wf_bbox["ymin"]) %>%
    st_point() %>%
    st_sfc()

  data_tilted <- ((data_geom - base_point) * rot(angle / 180 * pi) + base_point) %>%
    st_set_crs(st_crs(data_sf))

  data_sf$geometry <- data_tilted

  return(data_sf)
}

# /*=================================================*/
#' # Shift the field
# /*=================================================*/

st_shift <- function(data_sf, shift) {
  data_geom <- st_geometry(data_sf)

  shift_sfc <- st_point(shift) %>% st_sfc()

  data_shifted <- data_geom + shift_sfc %>%
    st_set_crs(st_crs(data_sf))

  data_sf$geometry <- data_shifted

  return(data_sf)
}

# /*=================================================*/
#' # Create cluster id
# /*=================================================*/

gen_cluster <- function(data, num_levels, dim) {
  data[, strip_group_id := ceiling(strip_id / num_levels / dim)] %>%
    .[, cluster_in_strip_id := ceiling(subplot_id / 6 / dim)] %>%
    .[, cluster_id := paste0(cluster_in_strip_id, strip_group_id)] %>%
    .[, `:=`(
      strip_group_id = NULL,
      cluster_in_strip_id = NULL
    )] %>%
    .[, cluster_id := as.numeric(as.factor(cluster_id))] %>%
    setnames("cluster_id", paste0("cluster_id_", dim))

  return(data)
}

# /*=================================================*/
#' # Make grids within a subplot for base_field 
# /*=================================================*/
make_grid_within_subplot <- function(field_base, cell){
  # cell="2_1"
  temp_cell <- field_base%>%filter(unique_cell_id==cell)
  res <- st_make_grid(st_boundary(temp_cell),  n = c(6, 1))%>%
    st_as_sf()%>%
    mutate(unique_cell_id=cell, cell_in_cell.id=seq(from=1,to=6))

  return(res)
} 


# /*=================================================*/
#' # Assign rates to the trial design data
# /*=================================================*/
# data_sf <- st_as_sf(data)
# rates_ls <- N_levels


assign_rates <- function(data_sf, rates_ls, pattern = "fixed-latin-square", merge = TRUE) {
  gen_sequence <- function(length) {
    if (length %% 2 == 0) {
      seq_r <- c(seq(1, length, by = 2), seq(length, 2, by = -2))
    } else {
      seq_r <- c(seq(1, length, by = 2), seq(length - 1, 2, by = -2))
    }
    return(seq_r)
  }

  gen_rd_seq <- function(seq_element, num) {
    for (i in 1:num) {
      if (i == 1) {
        seq_return <- seq_element
      } else {
        if (runif(1) < 0.5) {
          # seq_return <- c(seq_return, rev(seq_element))
          seq_return <- c(seq_return, seq_element)
        } else {
          seq_return <- c(seq_return, seq_element)
        }
      }
    }
    return(seq_return)
  }

  get_seq_for_strip <- function(pattern, rate_ranks_seq, num_seq, exclude_ls = NULL) {
    seq_possible <- gen_rd_seq(rate_ranks_seq, num_seq)

    position_ls <- 1:rates_len
    remaining_positions <- position_ls[!(position_ls %in% exclude_ls)]

    if (pattern == "block_randomized") {
      if (length(remaining_positions) == 1) {
        position <- remaining_positions
      } else {
        position <- sample(remaining_positions, 1)
      }
    } else if (pattern == "sequential") {
      if (all(exclude_ls != 0)) {
        previous_position <- exclude_ls[length(exclude_ls)]
      } else {
        previous_position <- 0
      }
      position <- previous_position + 1
    } else if (pattern == "fixed-latin-square") {
      if (all(exclude_ls != 0)) {
        previous_position <- exclude_ls[length(exclude_ls)]
        which_furthest <- which.max(abs(rate_ranks_seq[remaining_positions] - rate_ranks_seq[previous_position]))
        position <- remaining_positions[which_furthest]
      } else {
        position <- 1
      }
    }

    return(seq_possible[position:(position + max_plot_id - 1)])
  }

  # /*=================================================*/
  #' # Assign rates
  # /*=================================================*/

  rates_data <- data.table(
    rate = rates_ls,
    rate_rank = seq_len(length(rates_ls))
  )

  rates_len <- nrow(rates_data)

  #--- create a sequence of rate ranks ---#
  rate_ranks_seq <- gen_sequence(rates_len)

  data_dt <- data.table(data_sf)

  strip_ls <- data_dt[, strip_id] %>%
    unique() %>%
    .[order(.)]

  design_data_ls <- list()

  # i <- 12
  for (i in strip_ls) {
    # for (i in 1:10) {

    max_plot_id <- data_dt[strip_id == i, max(plot_id)]
    num_seq <- ceiling(max_plot_id / rates_len + 1)

    if ((i %% rates_len) == 1) {
      if (i == 1) {
        #--- the very first ---#
        rates_seq <- get_seq_for_strip(pattern, rate_ranks_seq, num_seq, 0)
        init_rate_memeory <- c(which(rates_seq[1] == rate_ranks_seq))
      } else {
        rates_seq <- get_seq_for_strip(
          pattern = pattern,
          rate_ranks_seq = rate_ranks_seq,
          num_seq = num_seq,
          #--- avoid having the same rate right next to it in the previous block ---#
          exclude_ls = init_rate_memeory[rates_len]
        )
        init_rate_memeory <- c(which(rates_seq[1] == rate_ranks_seq))
      }
    } else {
      rates_seq <- get_seq_for_strip(
        pattern = pattern,
        rate_ranks_seq = rate_ranks_seq,
        num_seq = num_seq,
        exclude_ls = init_rate_memeory
      )
      init_rate_memeory <- c(init_rate_memeory, which(rates_seq[1] == rate_ranks_seq))
    }

    design_data_ls[[i]] <- data.table(
      plot_id = seq_len(max_plot_id),
      strip_id = i,
      rate_rank = rates_seq
    )

    # print(rates_seq)
    # print(init_rate_memeory)
  }

  design_data <- rbindlist(design_data_ls)

  # design_data[, .N, by = rate_rank]

  if (merge == TRUE) {
    data <- left_join(data_sf, design_data, by = c("plot_id", "strip_id")) %>%
      left_join(., rates_data, by = "rate_rank")
    return(data)
  } else {
    design_data <- left_join(design_data, rates_data, by = "rate_rank")
    return(design_data)
  }
}


#/*----------------------------------*/
#' ## Unconditional Gaussian geostatistical Simulation  
#/*----------------------------------*/

gen_coefs <- function(mean, psill, range, coef_name, nsim, xy) {
  
  g_N <- gstat(
    formula = z ~ 1,
    locations = ~ X + Y,
    dummy = T,
    beta = mean,
    model = vgm(
      psill = psill,
      range = range,
      nugget = 0,
      model = "Sph" # changed from "Exp", "Sph"

    ),
    nmax = 50 # number of nearest observations
  )

  b_sim <- predict(g_N, newdata = xy, nsim = nsim) %>%
    data.table()%>%
    melt(id.vars = c("X", "Y")) %>%
    # data.table() %>%
    setnames(c("variable", "value"), c("sim", coef_name)) %>%
    .[, sim := as.numeric(gsub("sim", "", sim))] %>%
    xy[., on = c("X", "Y")] %>%
    .[, c("unique_cell_id", "sim", coef_name), with = FALSE]

  return(b_sim)
}



gen_coefs_par <- function(B, geo_xy, sp_range, psill_merror){
  # geo_xy=xy; sp_range=400
  #/*--------------------------------------------------------*/
  #' ## Generate raw coefficients
  #/*--------------------------------------------------------*/
  # === ymax === #
  ymax <- gen_coefs(
    mean = 12000,
    psill = 2000000,
    range = sp_range,
    coef_name = "ymax",
    nsim = B,
    xy = geo_xy
  )

  # === alpha === #
  alpha <- gen_coefs(
    mean = -0.5,
    psill = 0.02,
    range = sp_range,
    coef_name = "alpha",
    nsim = B,
    xy = geo_xy
  )

  # === raw beta === #
  beta <- gen_coefs(
    mean = 0,
    psill = 1,
    range = sp_range,
    coef_name = "beta_raw",
    nsim = B,
    xy = geo_xy
  ) %>%
  .[, sd_beta := sd(beta_raw), by = sim] %>%
  .[, mean_beta := mean(beta_raw), by = sim] %>%
  .[, beta := pnorm(beta_raw, mean = mean_beta, sd = sd_beta)] %>%
  .[, beta := (beta * 1.8 - 2.8) * 0.01]

  # === m_error === #
  # --- error psill --- #
  #' roughly (on average),
  #' 0.002 means 500 sd ,
  #' 0.0035 means 650 sd
  #' 0.015 means 1300 sd,
  #' 0.028 means 2000 sd
  m_error <- gen_coefs(
    mean = 0,
    psill = psill_merror,
    range = sp_range,
    coef_name = "m_error_uncorrelated",
    nsim = B,
    xy = geo_xy
  )

  m_error_agg <- 
    m_error %>%
    .[, y_error := 12000*m_error_uncorrelated] %>%
    .[,.(mean_y_error = sd(y_error), by = sim)] %>%
    .[,.(mean_yield = mean(mean_y_error))]

  # === split_ratio === #
  split_ratio <- gen_coefs(
    mean = 0.5,
    psill = 0.005,
    range = sp_range,
    coef_name = "split_ratio",
    nsim = B,
    xy = geo_xy
  )

  # === This error term is used to correlate m_error with beta === #
  mu_1 <- gen_coefs(
    mean = 0,
    psill = 1,
    range = sp_range,
    coef_name = "mu_1",
    nsim = B,
    xy = geo_xy
  )

  # === This error term is used for theta_1 === #
  mu_2 <- gen_coefs(
    mean = 0,
    psill = 1,
    range = sp_range,
    coef_name = "mu_2",
    nsim = B,
    xy = geo_xy
  )

  # === This error term is used for theta_2 === #
  mu_3 <- gen_coefs(
    mean = 0,
    psill = 1,
    range = sp_range,
    coef_name = "mu_3",
    nsim = B,
    xy = geo_xy
  )

  # === Merge these coef data together === #
  coef_data <- ymax[alpha, on = c("sim", "unique_cell_id")] %>%
    beta[., on = c("sim", "unique_cell_id")] %>%
    m_error[., on = c("sim", "unique_cell_id")] %>%
    split_ratio[., on =c("sim", "unique_cell_id")]%>%
    mu_1[., on =c("sim", "unique_cell_id")]%>%
    mu_2[., on =c("sim", "unique_cell_id")]%>%
    mu_3[., on =c("sim", "unique_cell_id")]%>%
    .[,`:=`(
      alpha1 = alpha*split_ratio,
      alpha2 = alpha*(1-split_ratio),
      beta1  = beta*split_ratio,
      beta2  = beta*(1-split_ratio),
      ymax1  = ymax*split_ratio,
      ymax2  = ymax*(1-split_ratio),
      beta_raw = NULL,
      sd_beta = NULL,
      mean_beta = NULL
    )]

  #/*------------------------------------------------------*/
  #' ## Generate errors (make m_error correlated with beta) 
  #/*------------------------------------------------------*/

  coef_data[, beta_norm := (beta - mean(beta))/sd(beta)] %>% 
    .[, mu1_norm := (mu_1 - mean(mu_1))/sd(mu_1)]%>%
    .[, mu2_norm := (mu_2 - mean(mu_2))/sd(mu_2)]%>%
    .[, mu3_norm := (mu_3 - mean(mu_3))/sd(mu_3)]%>%
    .[, m_error_raw := 0.6 * beta_norm + sqrt(1 - 0.6 ^ 2) * mu1_norm] %>% 
    .[, m_error := m_error_raw * sd(m_error_uncorrelated)] %>% 
    ##== create irrelevant coefficints but are correlated with beta==##
    .[, theta_1 := 0.6 * beta_norm + sqrt(1 - 0.6 ^ 2) * mu2_norm] %>% 
    .[, theta_2 := 0.8 * beta_norm + sqrt(1 - 0.6 ^ 2) * mu3_norm] %>%
    .[, `:=`(
      beta_norm = NULL,
      m_error_raw = NULL,
      mu_1 = NULL,
      mu_2 = NULL,
      mu_3 = NULL,
      mu1_norm = NULL, 
      mu2_norm = NULL, 
      mu3_norm = NULL,
      split_ratio = NULL
      # m_error_uncorrelated = NULL
    )]

  return(coef_data)
}





# /*----------------------------------*/
#' ## generate yield
# /*----------------------------------*/
gen_yield_MB <- function(ymax, alpha, beta, N) {
  yield <- ymax * (1 - exp(alpha + beta * N))
  return(yield)
}



# /*----------------------------------*/
#' ## generate application error (+-20)
# /*----------------------------------*/
app_error_fn <- function(data){
 # data = data

  x <- rnorm(nrow(data), sd=5)
  x <- x[x > -20 & x < 20]

  if (length(x)!=nrow(data)){
    repeat {
      lack_no <- nrow(data) - length(x)
      y <- rnorm(lack_no, sd=5)
      y <- y[y > -20 & y < 20]
      if (length(y) == lack_no){
      break
      }
    }
  z <- c(x,y)
  } else {
  z <- x
  }
  return(z)
}



# /*----------------------------------*/
#' ## Economic parameters
# /*----------------------------------*/
price_table <- data.table(
  # === $/kg corn ===#
  pCorn = round(c(3.5, 3.5, 5) * 200 / 5080, digits = 3),
  # === $/kg N ===#
  pN = round(c(0.4, 0.6, 0.4) / 0.453592, digits = 3)
)



# /*=================================================*/
#' # Generate Cell-Level Field Data
# /*=================================================*/

prepare_raw_data <- function(i, field, coef_data_m, coef_data_t, app_error="no") {
  print(paste0("working on ", i, " th iteration."))
  # i=1; x=1
  # field=field; coef_data_m=coef_data[sim == x, ]; coef_data_t=coef_data[sim == ifelse(x + 1 >= max(sim), 1, x + 1), ]
  # app_error="no"

  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Data preparation
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  # === merge field data with the coefs data ===#
  data <- coef_data_m[data.table(field), on = "unique_cell_id"] %>%
    .[, opt_N := (log(pN) - log(-pCorn * ymax * beta) - alpha) / beta]
    

  test_data <- coef_data_t[data.table(field), on = "unique_cell_id"] %>%
    .[, opt_N := (log(pN) - log(-pCorn * ymax * beta) - alpha) / beta]

  # === check the spatial distribution of optimal N ===#
  # left_join(field,data[,.(unique_cell_id,opt_N)],by='unique_cell_id') %>%
  #   select(opt_N) %>%
  #   tm_shape() +
  #     tm_fill(col = "opt_N")

  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  #' treatment N rates 
  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  # === define experimental N rate ===#
  all_opt_N <- data[, opt_N]

  # --- for training ---#
  # hist(all_opt_N)
  N_levels <- seq(
    quantile(all_opt_N, prob = 0.05) - 20,
    quantile(all_opt_N, prob = 0.95) + 20,
    length = 5 
  ) %>%
    round()

  data <- assign_rates(st_as_sf(data), N_levels) %>% 
    data.table()%>%
    .[!(subplot_id %in% c(1:3, 54:56)),]

  test_data <- assign_rates(st_as_sf(test_data), N_levels) %>% 
    data.table()%>%
    .[!(subplot_id %in% c(1:3, 54:56)),]


  if(app_error=="no"){
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' No application error
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
    data[,aa_n := rate]
    test_data[,aa_n := rate]

  } else {
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' Include application error
  # /*~~~~~~~~~~~~~~~~~~~~~~*/
    data <- data %>% .[,aa_n := rate + app_error_fn(.)]
    test_data <- test_data %>% .[,aa_n := rate + app_error_fn(.)]
  }

  # ggplot(data)+
  #   geom_histogram(aes(x=aa_n), bins=100)

  # /*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Generate yield
  # /*~~~~~~~~~~~~~~~~~~~~~~*/

  data %<>% .[, det_yield := gen_yield_MB(ymax, alpha, beta, aa_n)] %>%
      # === error ===#
    .[, yield := det_yield * (1 + m_error)] %>%
    .[, yield_error := det_yield*m_error] %>%
      # === keep the relevant vars ===#
    .[, .(
      yield, opt_N, rate, aa_n, alpha, beta, ymax, m_error, yield_error, alpha1, alpha2, beta1, beta2, ymax1, ymax2,
      theta_1, theta_2, subplot_id, strip_id, padding, X, Y, unique_cell_id
    )]%>%
    .[,sim := i]

  test_data %<>% .[, det_yield := gen_yield_MB(ymax, alpha, beta, aa_n)] %>%
      # === error ===#
    .[, yield := det_yield * (1 + m_error)] %>%
      # === keep the relevant vars ===#
    .[, .(
      yield, opt_N, rate, aa_n, alpha, beta, m_error, ymax, alpha1, alpha2, beta1, beta2, ymax1, ymax2,
      theta_1, theta_2, subplot_id, strip_id, padding, X, Y, unique_cell_id
    )]%>%
    .[,sim := i]


  return(
    list(
      reg_raw_data = data,
      test_raw_data = test_data
    )
  )
}


# /*=================================================*/
#' # Aggregate Cell-Level Field Data to Subplot-Level
# /*=================================================*/
prepare_data_for_sim <- function(reg_raw_data, test_raw_data){

  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Aggregate data by analysis unit (by subplot)
  # /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

  # === by subplot ===#
  reg_data <- reg_raw_data[, .(
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
    theta_2 = mean(theta_2),
    padding = mean(padding),
    X = mean(X),
    Y = mean(Y)
  ), by = .(subplot_id, strip_id)]%>%
  .[,unique_subplot_id:=paste0(strip_id,"_",subplot_id)]%>%
  .[, `:=`(
    strip_id = NULL,
    subplot_id = NULL
  )]  

  test_agg_data <- test_raw_data[, .(
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
    theta_2 = mean(theta_2),
    padding = mean(padding),
    X = mean(X),
    Y = mean(Y)
  ), by = .(subplot_id, strip_id)]%>%
  .[,unique_subplot_id:=paste0(strip_id,"_",subplot_id)]%>%
  .[, `:=`(
    strip_id = NULL,
    subplot_id = NULL
  )]

  return(
    list(
      reg_data = reg_data,
      test_agg_data = test_agg_data
    )
  )
}