library(simmer)
library(yaml)
library(dplyr)
library(tidyr)
library(ggplot2)
# =========================================================
# Read YAML
# =========================================================

if (!file.exists("escenarios_iñaki.yaml")) {
  stop("No encuentro el archivo 'escenarios_iñaki.yaml' en esta carpeta: ", getwd())
}

cfg <- yaml::read_yaml("escenarios_iñaki.yaml")

# =========================================================
# YAML helpers
# =========================================================

build_rides_df <- function(cfg_rides) {
  rides <- do.call(rbind, lapply(cfg_rides, as.data.frame))
  rides$ride <- as.character(rides$ride)
  rides$min_height <- as.numeric(rides$min_height)
  rides$seats <- as.numeric(rides$seats)
  rides$queue_max <- as.numeric(rides$queue_max)
  rides$cycle_mean <- as.numeric(rides$cycle_mean)
  rides$n_trains <- as.numeric(rides$n_trains)
  rides$capacity_persons <- rides$seats * rides$n_trains
  rides
}

build_group_types_df <- function(cfg_group_types) {
  df <- data.frame(
    name = sapply(cfg_group_types, function(x) x$name),
    prob = as.numeric(sapply(cfg_group_types, function(x) x$prob)),
    size_lambda = as.numeric(sapply(cfg_group_types, function(x) x$size_lambda)),
    minheight_lo = as.numeric(sapply(cfg_group_types, function(x) x$minheight_range[[1]])),
    minheight_hi = as.numeric(sapply(cfg_group_types, function(x) x$minheight_range[[2]])),
    max_wait_lo = as.numeric(sapply(cfg_group_types, function(x) x$max_wait_range[[1]])),
    max_wait_hi = as.numeric(sapply(cfg_group_types, function(x) x$max_wait_range[[2]])),
    max_rides_lo = as.numeric(sapply(cfg_group_types, function(x) x$max_rides_range[[1]])),
    max_rides_hi = as.numeric(sapply(cfg_group_types, function(x) x$max_rides_range[[2]])),
    max_park_time_lo = as.numeric(sapply(cfg_group_types, function(x) x$max_park_time_range[[1]])),
    max_park_time_hi = as.numeric(sapply(cfg_group_types, function(x) x$max_park_time_range[[2]])),
    stringsAsFactors = FALSE
  )
  df
}

get_group_type <- function(cfg, type_idx) {
  cfg$group_types[[type_idx]]
}

get_scenario <- function(cfg, scenario_name) {
  sc <- Filter(function(x) x$name == scenario_name, cfg$scenarios)
  if (length(sc) == 0) stop(paste("Scenario not found:", scenario_name))
  sc[[1]]
}

make_interarrival <- function(sim, arrivals_cfg) {
  function() {
    t <- now(sim)
    for (block in arrivals_cfg) {
      if (t < block$until_time) {
        return(rexp(1, 1 / block$mean_iat))
      }
    }
    rexp(1, 1 / tail(arrivals_cfg, 1)[[1]]$mean_iat)
  }
}


weighted_mean <- function(x, w) {
  if (length(x) == 0 || length(w) == 0 || sum(w) == 0) return(NA_real_)
  sum(x * w) / sum(w)
}

last_attr_per_group <- function(att, keyname) {
  tmp <- att[att$key == keyname, c("name", "time", "value"), drop = FALSE]
  if (nrow(tmp) == 0) return(NULL)
  tmp <- tmp[order(tmp$name, tmp$time), ]
  tmp <- tmp[!duplicated(tmp$name, fromLast = TRUE), ]
  tmp[, c("name", "value"), drop = FALSE]
}

sum_last_attr <- function(att, keyname) {
  tmp <- last_attr_per_group(att, keyname)
  if (is.null(tmp)) return(0)
  sum(tmp$value, na.rm = TRUE)
}

summarise_mean_sd <- function(df, id_cols = c("scenario")) {
  num_cols <- setdiff(names(df), id_cols)
  num_cols <- num_cols[sapply(df[num_cols], is.numeric)]
  
  out <- unique(df[id_cols])
  out <- out[order(out$scenario), , drop = FALSE]
  
  for (cname in num_cols) {
    out[[paste0(cname, "_mean")]] <- tapply(df[[cname]], df$scenario, mean, na.rm = TRUE)[out$scenario]
    out[[paste0(cname, "_sd")]]   <- tapply(df[[cname]], df$scenario, sd,   na.rm = TRUE)[out$scenario]
  }
  out
}

draw_positive_poisson <- function(lambda) {
  x <- 0
  while (x < 1) {
    x <- rpois(1, lambda)
  }
  x
}

# =========================================================
# Main simulation
# =========================================================

run_scenario <- function(
    cfg,
    scenario,
    seed = 123,
    override_n_trains = NULL
) {
  policy <- scenario$policy
  alpha <- scenario$alpha
  breakdown <- scenario$breakdown
  until <- scenario$until
  p_repeat <- scenario$p_repeat
  
  set.seed(seed)
  
  rides <- build_rides_df(cfg$rides)
  group_types_df <- build_group_types_df(cfg$group_types)
  
  if (!is.null(override_n_trains)) {
    if (length(override_n_trains) != nrow(rides)) {
      stop("override_n_trains must have same length as number of rides")
    }
    rides$n_trains <- override_n_trains
    rides$capacity_persons <- rides$seats * rides$n_trains
  }
  
  park_capacity <- cfg$general$park_capacity
  
  # Environment + resources
  sim <- simmer("park") %>%
    add_resource("park", capacity = park_capacity, queue_size = Inf)
  
  for (i in 1:nrow(rides)) {
    sim <- sim %>% add_resource(
      name = rides$ride[i],
      capacity = rides$capacity_persons[i],
      queue_size = Inf
    )
  }
  
  # globals in persons
  for (r in rides$ride) {
    sim <- sim %>% add_global(paste0("q_", r), 0)
    sim <- sim %>% add_global(paste0("s_", r), 0)
  }
  
  # ride aux funcs
  ride_cycle_time <- function(ride_name) rides$cycle_mean[match(ride_name, rides$ride)]
  ride_min_height <- function(ride_name) rides$min_height[match(ride_name, rides$ride)]
  ride_queue_max  <- function(ride_name) rides$queue_max[match(ride_name, rides$ride)]
  ride_capacity   <- function(ride_name) rides$capacity_persons[match(ride_name, rides$ride)]
  
  # =========================================================
  # Group generators from YAML
  # =========================================================
  
  draw_group_type <- function() {
    sample(seq_len(nrow(group_types_df)), 1, prob = group_types_df$prob)
  }
  
  draw_g_size <- function(type_idx) {
    gt <- get_group_type(cfg, type_idx)
    draw_positive_poisson(gt$size_lambda)
  }
  
  draw_g_minheight <- function(type_idx) {
    gt <- get_group_type(cfg, type_idx)
    sample(gt$minheight_range[[1]]:gt$minheight_range[[2]], 1)
  }
  
  draw_max_wait <- function(type_idx) {
    gt <- get_group_type(cfg, type_idx)
    runif(1, gt$max_wait_range[[1]], gt$max_wait_range[[2]])
  }
  
  draw_balk_q <- function(type_idx) {
    gt <- get_group_type(cfg, type_idx)
    sample(unlist(gt$balk_q_values), 1)
  }
  
  draw_max_rides <- function(type_idx) {
    gt <- get_group_type(cfg, type_idx)
    sample(gt$max_rides_range[[1]]:gt$max_rides_range[[2]], 1)
  }
  
  draw_max_park_time <- function(type_idx) {
    gt <- get_group_type(cfg, type_idx)
    runif(1, gt$max_park_time_range[[1]], gt$max_park_time_range[[2]])
  }
  
  base_pref <- function(type_idx) {
    gt <- get_group_type(cfg, type_idx)
    as.numeric(unlist(gt$base_pref))
  }
  
  draw_prefs <- function(type_idx) {
    b <- base_pref(type_idx)
    noise <- runif(length(b), 0.7, 1.3)
    w <- b * noise
    w / sum(w)
  }
  
  get_pref_component <- function(comp_idx) {
    function() {
      s <- get_attribute(sim, "pref_seed")
      type_idx <- get_attribute(sim, "g_type")
      old <- .Random.seed
      set.seed(s)
      v <- draw_prefs(type_idx)
      if (!is.null(old)) .Random.seed <<- old
      v[comp_idx]
    }
  }
  
  choose_ride_idx <- function() {
    prefs <- c(
      get_attribute(sim, "pref_a1"),
      get_attribute(sim, "pref_a2"),
      get_attribute(sim, "pref_a3"),
      get_attribute(sim, "pref_a4"),
      get_attribute(sim, "pref_a5")
    )
    
    gmh <- get_attribute(sim, "g_minheight")
    eligible <- as.numeric(gmh >= rides$min_height)
    w <- prefs * eligible
    
    already_rode <- c(
      get_attribute(sim, "rode_a1") > 0,
      get_attribute(sim, "rode_a2") > 0,
      get_attribute(sim, "rode_a3") > 0,
      get_attribute(sim, "rode_a4") > 0,
      get_attribute(sim, "rode_a5") > 0
    )
    
    repeat_factor <- ifelse(already_rode, p_repeat, 1)
    w <- w * repeat_factor
    
    if (policy == "informed") {
      qs_persons <- sapply(rides$ride, function(r) get_global(sim, paste0("q_", r)))
      w <- w * exp(-alpha * qs_persons)
      
      cap_ratio <- rides$capacity_persons / rides$seats
      w <- w * cap_ratio
    }
    
    if (sum(w) <= 0) {
      if (sum(eligible) > 0) return(sample(which(eligible == 1), 1))
      return(0)
    }
    
    sample(seq_len(nrow(rides)), 1, prob = w)
  }
  
  ride_action <- function(ride_name) {
    trajectory(paste0("action_", ride_name)) %>%
      branch(
        option = function() {
          ok <- get_attribute(sim, "g_minheight") >= ride_min_height(ride_name)
          as.integer(ok) + 1L
        },
        continue = c(TRUE, TRUE),
        
        trajectory() %>%
          set_attribute("height_blocked", function() get_attribute(sim, "height_blocked") + 1) %>%
          timeout(0),
        
        trajectory() %>%
          branch(
            option = function() {
              g  <- get_attribute(sim, "g_size")
              qp <- get_global(sim, paste0("q_", ride_name))
              
              cap_now <- get_capacity(sim, ride_name)
              if (cap_now <= 0) return(2L)
              
              if (g > ride_capacity(ride_name)) return(1L)
              
              ok_personal <- (qp <= get_attribute(sim, "balk_q"))
              ok_fisico   <- ((qp + g) <= ride_queue_max(ride_name))
              ok <- ok_personal && ok_fisico
              
              if (!ok) return(2L) else return(3L)
            },
            continue = c(TRUE, TRUE, TRUE),
            
            trajectory() %>%
              set_attribute("balked", function() get_attribute(sim, "balked") + 1) %>%
              set_attribute(paste0("balk_", ride_name), function() get_attribute(sim, paste0("balk_", ride_name)) + 1) %>%
              set_attribute("balk_impossible", function() get_attribute(sim, "balk_impossible") + 1) %>%
              timeout(0),
            
            trajectory() %>%
              set_attribute("balked", function() get_attribute(sim, "balked") + 1) %>%
              set_attribute(paste0("balk_", ride_name), function() get_attribute(sim, paste0("balk_", ride_name)) + 1) %>%
              timeout(0),
            
            trajectory() %>%
              set_global(paste0("q_", ride_name),
                         function() get_attribute(sim, "g_size"),
                         mod = "+") %>%
              renege_in(
                function() get_attribute(sim, "max_wait"),
                out = trajectory() %>%
                  set_global(paste0("q_", ride_name),
                             function() -get_attribute(sim, "g_size"),
                             mod = "+") %>%
                  set_attribute("reneged", function() get_attribute(sim, "reneged") + 1) %>%
                  set_attribute(paste0("ren_", ride_name),
                                function() get_attribute(sim, paste0("ren_", ride_name)) + 1)
              ) %>%
              seize(ride_name, amount = function() get_attribute(sim, "g_size")) %>%
              set_global(paste0("q_", ride_name),
                         function() -get_attribute(sim, "g_size"),
                         mod = "+") %>%
              set_global(paste0("s_", ride_name),
                         function() get_attribute(sim, "g_size"),
                         mod = "+") %>%
              renege_abort() %>%
              timeout(function() {
                ct <- ride_cycle_time(ride_name)
                t <- now(sim)
                rem <- t %% ct
                wait_next <- if (rem == 0) 0 else (ct - rem)
                wait_next + ct
              }) %>%
              set_global(paste0("s_", ride_name),
                         function() -get_attribute(sim, "g_size"),
                         mod = "+") %>%
              release(ride_name, amount = function() get_attribute(sim, "g_size")) %>%
              set_attribute("rode", function() get_attribute(sim, "rode") + 1) %>%
              set_attribute(paste0("rode_", ride_name),
                            function() get_attribute(sim, paste0("rode_", ride_name)) + 1) %>%
              set_attribute("n_done", function() get_attribute(sim, "n_done") + 1)
          )
      )
  }
  
  attempt_traj <- trajectory("attempt") %>%
    set_attribute("ride", function() choose_ride_idx()) %>%
    branch(
      option = function() {
        x <- get_attribute(sim, "ride")
        if (x == 0) nrow(rides) + 1L else x
      },
      continue = rep(TRUE, nrow(rides) + 1),
      ride_action("a1"),
      ride_action("a2"),
      ride_action("a3"),
      ride_action("a4"),
      ride_action("a5"),
      trajectory() %>% timeout(0)
    )
  
  traj <- trajectory("group") %>%
    set_attribute("g_type", function() draw_group_type()) %>%
    set_attribute("g_size", function() draw_g_size(get_attribute(sim, "g_type"))) %>%
    set_attribute("g_minheight", function() draw_g_minheight(get_attribute(sim, "g_type"))) %>%
    set_attribute("max_wait", function() draw_max_wait(get_attribute(sim, "g_type"))) %>%
    set_attribute("balk_q", function() draw_balk_q(get_attribute(sim, "g_type"))) %>%
    set_attribute("balked", 0) %>%
    set_attribute("reneged", 0) %>%
    set_attribute("rode", 0) %>%
    set_attribute("height_blocked", 0) %>%
    set_attribute("balk_impossible", 0)
  
  for (prefix in c("balk_", "ren_", "rode_")) {
    for (i in 1:nrow(rides)) {
      traj <- traj %>% set_attribute(paste0(prefix, "a", i), 0)
    }
  }
  
  traj <- traj %>%
    set_attribute("pref_seed", function() sample.int(1e9, 1))
  
  for (i in 1:nrow(rides)) {
    traj <- traj %>% set_attribute(paste0("pref_a", i), get_pref_component(i))
  }
  
  traj <- traj %>%
    seize("park", amount = function() get_attribute(sim, "g_size")) %>%
    set_attribute("t_enter", function() now(sim)) %>%
    set_attribute("max_park_time", function() draw_max_park_time(get_attribute(sim, "g_type"))) %>%
    set_attribute("max_rides", function() draw_max_rides(get_attribute(sim, "g_type"))) %>%
    set_attribute("n_done", 0) %>%
    join(attempt_traj) %>%
    timeout(function() runif(1, 60, 300)) %>%
    branch(
      option = function() {
        done_by_rides <- get_attribute(sim, "n_done") >= get_attribute(sim, "max_rides")
        done_by_time  <- (now(sim) - get_attribute(sim, "t_enter")) >= get_attribute(sim, "max_park_time")
        as.integer(!(done_by_rides || done_by_time)) + 1L
      },
      continue = c(TRUE, TRUE),
      trajectory() %>% timeout(0),
      trajectory() %>% rollback(target = 3, times = Inf)
    ) %>%
    release("park", amount = function() get_attribute(sim, "g_size"))
  
  # Arrivals from YAML
  arrivals_cfg <- if (!is.null(scenario$arrivals)) scenario$arrivals else cfg$arrivals
  interarrival <- make_interarrival(sim, arrivals_cfg)
  sim <- sim %>% add_generator("G", traj, interarrival, mon = 2)
  
  # Breakdown
  if (!is.null(breakdown)) {
    bd_ride  <- breakdown$ride
    bd_start <- breakdown$start
    bd_dur   <- breakdown$duration
    bd_mode  <- breakdown$mode
    cap0 <- ride_capacity(bd_ride)
    if (is.null(bd_mode)) bd_mode <- "close"
    
    if (bd_mode == "close") {
      bd_traj <- trajectory("breakdown") %>%
        timeout(bd_start) %>%
        set_capacity(bd_ride, 0) %>%
        timeout(bd_dur) %>%
        set_capacity(bd_ride, cap0)
    } else if (bd_mode == "reduce") {
      fac <- breakdown$factor
      if (is.null(fac)) fac <- 0.5
      cap_red <- max(0, floor(cap0 * fac))
      bd_traj <- trajectory("breakdown") %>%
        timeout(bd_start) %>%
        set_capacity(bd_ride, cap_red) %>%
        timeout(bd_dur) %>%
        set_capacity(bd_ride, cap0)
    } else {
      stop("breakdown$mode must be 'close' or 'reduce'")
    }
    
    sim <- sim %>% add_generator(paste0("BD_", bd_ride), bd_traj, at(0), mon = 0)
  }
  
  sim %>% run(until)
  
  # Outputs
  arr <- get_mon_arrivals(sim)
  res <- get_mon_resources(sim)
  att <- get_mon_attributes(sim)
  
  ride_keys <- rides$ride
  globals <- att[att$name == "" & att$key %in% c(paste0("q_", ride_keys), paste0("s_", ride_keys)),
                 c("time", "key", "value"), drop = FALSE]
  globals <- globals[order(globals$key, globals$time), ]
  
  if (nrow(globals) > 0) {
    globals$dt <- ave(globals$time, globals$key, FUN = function(t) c(diff(t), until - tail(t, 1)))
    keys <- sort(unique(globals$key))
    
    agg_by_key <- data.frame(key = keys, mean_time = NA_real_, max_value = NA_real_, stringsAsFactors = FALSE)
    for (i in seq_along(keys)) {
      k <- keys[i]
      tmp <- globals[globals$key == k, , drop = FALSE]
      agg_by_key$mean_time[i] <- weighted_mean(tmp$value, tmp$dt)
      agg_by_key$max_value[i] <- max(tmp$value, na.rm = TRUE)
    }
    
    get_stat <- function(prefix, ride, field) {
      k <- paste0(prefix, ride)
      row <- agg_by_key[agg_by_key$key == k, , drop = FALSE]
      if (nrow(row) == 0) return(NA_real_)
      row[[field]]
    }
    
    agg_time <- data.frame(
      ride = ride_keys,
      mean_queue_persons_time = NA_real_,
      mean_system_persons_time = NA_real_,
      max_queue_persons = NA_real_,
      max_system_persons = NA_real_,
      max_service_persons = NA_real_,
      stringsAsFactors = FALSE
    )
    
    for (i in 1:nrow(agg_time)) {
      r <- agg_time$ride[i]
      q_mean <- get_stat("q_", r, "mean_time")
      s_mean <- get_stat("s_", r, "mean_time")
      q_max  <- get_stat("q_", r, "max_value")
      s_max  <- get_stat("s_", r, "max_value")
      agg_time$mean_queue_persons_time[i]  <- q_mean
      agg_time$mean_system_persons_time[i] <- q_mean + s_mean
      agg_time$max_queue_persons[i]        <- q_max
      agg_time$max_system_persons[i]       <- q_max + s_max
      agg_time$max_service_persons[i]      <- s_max
    }
  } else {
    agg_time <- data.frame(
      ride = ride_keys,
      mean_queue_persons_time = NA_real_,
      mean_system_persons_time = NA_real_,
      max_queue_persons = NA_real_,
      max_system_persons = NA_real_,
      max_service_persons = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  
  gsize <- last_attr_per_group(att, "g_size"); if (!is.null(gsize)) names(gsize)[2] <- "g_size"
  balk  <- last_attr_per_group(att, "balked"); if (!is.null(balk))  names(balk)[2]  <- "balked"
  ren   <- last_attr_per_group(att, "reneged"); if (!is.null(ren))  names(ren)[2]   <- "reneged"
  rode  <- last_attr_per_group(att, "rode"); if (!is.null(rode))    names(rode)[2]  <- "rode"
  
  attr_tables <- list(gsize, balk, ren, rode)
  
  for (r in ride_keys) {
    tmp1 <- last_attr_per_group(att, paste0("balk_", r)); if (!is.null(tmp1)) names(tmp1)[2] <- paste0("balk_", r)
    tmp2 <- last_attr_per_group(att, paste0("ren_", r));  if (!is.null(tmp2)) names(tmp2)[2] <- paste0("ren_", r)
    tmp3 <- last_attr_per_group(att, paste0("rode_", r)); if (!is.null(tmp3)) names(tmp3)[2] <- paste0("rode_", r)
    attr_tables <- c(attr_tables, list(tmp1, tmp2, tmp3))
  }
  
  grp_list <- Filter(Negate(is.null), attr_tables)
  
  if (length(grp_list) == 0) {
    grp <- NULL
  } else {
    grp <- Reduce(function(x, y) merge(x, y, by = "name", all = TRUE), grp_list)
  }
  
  if (is.null(grp) || nrow(grp) == 0) {
    grp <- data.frame(name = character(0), g_size = numeric(0), balked = numeric(0),
                      reneged = numeric(0), rode = numeric(0), stringsAsFactors = FALSE)
    for (r in ride_keys) {
      grp[[paste0("balk_", r)]] <- numeric(0)
      grp[[paste0("ren_", r)]]  <- numeric(0)
      grp[[paste0("rode_", r)]] <- numeric(0)
    }
    pct_balk <- NA_real_
    pct_ren <- NA_real_
    total_rides_persons <- 0
    total_balked_persons <- 0
    total_reneged_persons <- 0
  } else {
    grp[is.na(grp)] <- 0
    pct_balk <- mean(grp$balked > 0) * 100
    pct_ren  <- mean(grp$reneged > 0) * 100
    total_rides_persons <- sum(grp$rode * grp$g_size)
    total_balked_persons <- sum(grp$balked * grp$g_size)
    total_reneged_persons <- sum(grp$reneged * grp$g_size)
  }
  
  tab_by_ride <- data.frame(
    ride = ride_keys,
    balked_persons = sapply(ride_keys, function(r) sum(grp[[paste0("balk_", r)]] * grp$g_size)),
    reneged_persons = sapply(ride_keys, function(r) sum(grp[[paste0("ren_", r)]] * grp$g_size)),
    rode_persons = sapply(ride_keys, function(r) sum(grp[[paste0("rode_", r)]] * grp$g_size)),
    stringsAsFactors = FALSE
  )
  
  arr$time_in_system <- arr$end_time - arr$start_time
  mean_time_in_system <- mean(arr$time_in_system, na.rm = TRUE)
  p95_time_in_system  <- as.numeric(quantile(arr$time_in_system, 0.95, na.rm = TRUE))
  
  kpis <- data.frame(
    policy = policy,
    alpha = alpha,
    total_groups = nrow(arr),
    total_persons_arrived = sum(grp$g_size),
    mean_time_in_system = mean_time_in_system,
    p95_time_in_system = p95_time_in_system,
    pct_groups_balked = pct_balk,
    pct_groups_reneged = pct_ren,
    total_balked_persons = total_balked_persons,
    total_reneged_persons = total_reneged_persons,
    total_rides_persons = total_rides_persons,
    rides_persons_per_arrived_person = ifelse(sum(grp$g_size) > 0,
                                              total_rides_persons / sum(grp$g_size),
                                              NA_real_),
    rides_persons_per_hour = total_rides_persons / (until / 3600),
    mean_queue_persons_time_overall = mean(agg_time$mean_queue_persons_time, na.rm = TRUE),
    max_queue_persons_overall = max(agg_time$max_queue_persons, na.rm = TRUE),
    max_service_persons_overall = max(agg_time$max_service_persons, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  
  list(
    agg_time = agg_time,
    tab_by_ride = tab_by_ride,
    kpis = kpis,
    arr = arr,
    res = res,
    att = att
  )
}

# =========================================================
# Replications
# =========================================================

run_replications <- function(
    cfg,
    scenario_name,
    override_n_trains = NULL,
    override_alpha = NULL
) {
  scenario <- get_scenario(cfg, scenario_name)
  
  if (!is.null(override_alpha)) {
    scenario$alpha <- override_alpha
  }
  
  R <- scenario$R
  seed0 <- scenario$seed0
  
  rows <- vector("list", R)
  
  for (r in 1:R) {
    out <- run_scenario(
      cfg = cfg,
      scenario = scenario,
      seed = seed0 + r,
      override_n_trains = override_n_trains
    )
    k <- out$kpis
    k$scenario <- scenario_name
    k$rep <- r
    rows[[r]] <- k
  }
  
  do.call(rbind, rows)
}

run_replications_full <- function( #to do good graphs we need kpis agg_time and tab_by_ride per attraction and kpis
    cfg,
    scenario_name,
    override_n_trains = NULL,
    override_alpha = NULL
) {
  scenario <- get_scenario(cfg, scenario_name)
  
  if (!is.null(override_alpha)) {
    scenario$alpha <- override_alpha
  }
  
  R <- scenario$R
  seed0 <- scenario$seed0
  
  out_list <- vector("list", R)
  
  for (r in 1:R) {
    out <- run_scenario(
      cfg = cfg,
      scenario = scenario,
      seed = seed0 + r,
      override_n_trains = override_n_trains
    )
    
    out$kpis$scenario <- scenario_name
    out$kpis$rep <- r
    
    out$agg_time$scenario <- scenario_name
    out$agg_time$rep <- r
    
    out$tab_by_ride$scenario <- scenario_name
    out$tab_by_ride$rep <- r
    
    out$arr$scenario <- scenario_name
    out$arr$rep <- r
    
    out_list[[r]] <- out
  }
  
  out_list
}

collect_all_results <- function(cfg) { #combine results of the scenarios
  scenario_names <- sapply(cfg$scenarios, function(x) x$name)
  
  all_full <- lapply(scenario_names, function(sc_name) {
    run_replications_full(cfg, sc_name)
  })
  
  flat <- unlist(all_full, recursive = FALSE)
  
  kpis_df <- do.call(rbind, lapply(flat, function(x) x$kpis))
  agg_time_df <- do.call(rbind, lapply(flat, function(x) x$agg_time))
  tab_by_ride_df <- do.call(rbind, lapply(flat, function(x) x$tab_by_ride))
  arrivals_df <- do.call(rbind, lapply(flat, function(x) x$arr))
  
  list(
    kpis_df = kpis_df,
    agg_time_df = agg_time_df,
    tab_by_ride_df = tab_by_ride_df,
    arrivals_df = arrivals_df
  )
}

export_results <- function(results_list) { #export to csv
  kpis_df <- results_list$kpis_df
  agg_time_df <- results_list$agg_time_df
  tab_by_ride_df <- results_list$tab_by_ride_df
  arrivals_df <- results_list$arrivals_df
  
  final_table <- summarise_mean_sd(kpis_df, id_cols = c("scenario"))
  final_table_round <- final_table
  
  num_cols <- names(final_table_round)[sapply(final_table_round, is.numeric)]
  final_table_round[num_cols] <- lapply(final_table_round[num_cols], function(x) round(x, 3))
  
  write.csv(kpis_df, "kpis_by_replication.csv", row.names = FALSE)
  write.csv(agg_time_df, "agg_time_by_ride.csv", row.names = FALSE)
  write.csv(tab_by_ride_df, "tab_by_ride.csv", row.names = FALSE)
  write.csv(arrivals_df, "arrivals_monitor.csv", row.names = FALSE)
  write.csv(final_table_round, "final_summary_table.csv", row.names = FALSE)
  
  final_table_round
}

# Usage

results_all <- collect_all_results(cfg)
final_table_round <- export_results(results_all)

print(final_table_round)

# Tests

sc_inf <- get_scenario(cfg, "informed_low_load")
out <- run_scenario(cfg = cfg, scenario = sc_inf, seed = 123)

att <- out$att
globals_qs <- att[att$name == "" & grepl("^q_a", att$key), ]
min_q <- if (nrow(globals_qs) == 0) NA_real_ else min(globals_qs$value, na.rm = TRUE)
cat("Minimum queue value:", min_q, "\n")

rides_df <- build_rides_df(cfg$rides)
rides_capacity <- setNames(rides_df$capacity_persons, rides_df$ride)

globals_s <- att[att$name == "" & grepl("^s_a", att$key), ]

check_capacity <- function(ride) {
  vals <- globals_s$value[globals_s$key == paste0("s_", ride)]
  max_s <- if (length(vals) == 0) 0 else max(vals, na.rm = TRUE)
  cap <- rides_capacity[ride]
  c(max_service = max_s, capacity = cap)
}

for (r in names(rides_capacity)) {
  print(c(ride = r, check_capacity(r)))
}
cmp_normal_capacity <- run_replications(
  cfg,
  "informed_high_load",
  override_n_trains = c(1, 1, 1, 1, 1)
)

cmp_add_train_a3 <- run_replications(
  cfg,
  "informed_high_load",
  override_n_trains = c(1, 1, 2, 1, 1)
)

cat("Throughput normal capacity mean (persons):",
    mean(cmp_normal_capacity$total_rides_persons), "\n")
cat("Throughput add train a3 mean (persons):",
    mean(cmp_add_train_a3$total_rides_persons), "\n")

cmp_a1 <- run_replications(
  cfg,
  "informed_low_load",
  override_alpha = 0.01
)

cmp_a2 <- run_replications(
  cfg,
  "informed_low_load",
  override_alpha = 0.05
)

cat("Mean max queue alpha=0.01 (persons):", mean(cmp_a1$max_queue_persons_overall), "\n")
cat("Mean max queue alpha=0.05 (persons):", mean(cmp_a2$max_queue_persons_overall), "\n")

cmp_normal <- run_replications(cfg, "informed_high_load")
cmp_broken <- run_replications(cfg, "informed_high_load_break_a3_close")

cat("Throughput normal mean (persons):", mean(cmp_normal$total_rides_persons), "\n")
cat("Throughput broken mean (persons):", mean(cmp_broken$total_rides_persons), "\n")

normal_one <- run_scenario(
  cfg = cfg,
  scenario = get_scenario(cfg, "informed_high_load"),
  seed = 123
)

broken_one <- run_scenario(
  cfg = cfg,
  scenario = get_scenario(cfg, "informed_high_load_break_a3_close"),
  seed = 123
)

cat("Rides PERSONS a3 normal (one run):",
    normal_one$tab_by_ride$rode_persons[normal_one$tab_by_ride$ride == "a3"], "\n")
cat("Rides PERSONS a3 broken (one run):",
    broken_one$tab_by_ride$rode_persons[broken_one$tab_by_ride$ride == "a3"], "\n\n")

print(normal_one$tab_by_ride)
print(broken_one$tab_by_ride)
