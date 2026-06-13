source("simulation_park.R")

# Muted palette: less saturated and more neutral.
# Avoids strong pink, purple, turquoise and bright blue.
soft_palette <- c(
  "#6B705C",  # olive grey
  "#A5A58D",  # muted sage
  "#B7B7A4",  # warm grey
  "#CB997E",  # soft terracotta
  "#DDBEA9",  # beige
  "#8A817C",  # taupe
  "#C2C5AA",  # pale green-grey
  "#A68A64"   # muted brown
)

common_theme <- theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 17, face = "bold"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 13, face = "bold"),
    panel.grid.minor = element_blank()
  )

# All boxplots are horizontal.
# Numeric variable goes on the x-axis.
# Categorical variable goes on the y-axis.
horizontal_boxplot <- function(data, category, metric) {
  ggplot(data, aes(x = {{ metric }}, y = {{ category }})) +
    geom_boxplot(width = 0.65, fill = "#B7B7A4", colour = "#4A4A4A") +
    common_theme
}

# ---------------------------------------------------------
# 1) Collect all results
# ---------------------------------------------------------

results_all <- collect_all_results(cfg)

kpis_df <- results_all$kpis_df
agg_time_df <- results_all$agg_time_df
tab_by_ride_df <- results_all$tab_by_ride_df
arrivals_df <- results_all$arrivals_df

final_summary <- summarise_mean_sd(kpis_df, id_cols = c("scenario"))
print(final_summary)

# ---------------------------------------------------------
# 2) Pretty summary table
# ---------------------------------------------------------

summary_table_pretty <- final_summary %>%
  transmute(
    scenario,
    total_persons_arrived_mean = round(total_persons_arrived_mean, 2),
    total_persons_arrived_sd = round(total_persons_arrived_sd, 2),
    mean_time_in_system_mean = round(mean_time_in_system_mean, 2),
    mean_time_in_system_sd = round(mean_time_in_system_sd, 2),
    p95_time_in_system_mean = round(p95_time_in_system_mean, 2),
    p95_time_in_system_sd = round(p95_time_in_system_sd, 2),
    pct_groups_balked_mean = round(pct_groups_balked_mean, 2),
    pct_groups_balked_sd = round(pct_groups_balked_sd, 2),
    pct_groups_reneged_mean = round(pct_groups_reneged_mean, 2),
    pct_groups_reneged_sd = round(pct_groups_reneged_sd, 2),
    rides_persons_per_hour_mean = round(rides_persons_per_hour_mean, 2),
    rides_persons_per_hour_sd = round(rides_persons_per_hour_sd, 2),
    mean_queue_persons_time_overall_mean = round(mean_queue_persons_time_overall_mean, 2),
    mean_queue_persons_time_overall_sd = round(mean_queue_persons_time_overall_sd, 2),
    max_queue_persons_overall_mean = round(max_queue_persons_overall_mean, 2),
    max_queue_persons_overall_sd = round(max_queue_persons_overall_sd, 2)
  )

print(summary_table_pretty)
write.csv(summary_table_pretty, "summary_table_pretty.csv", row.names = FALSE)

# ---------------------------------------------------------
# 3) Plot 1: Mean time in system by scenario
# ---------------------------------------------------------

g1 <- horizontal_boxplot(kpis_df, scenario, mean_time_in_system) +
  labs(
    title = "Mean time in system by scenario",
    x = "Mean time in system",
    y = "Scenario"
  )

print(g1)
ggsave("g1_mean_time_in_system_boxplot.png", g1, width = 11, height = 6.5)

# ---------------------------------------------------------
# 4) Plot 2: P95 time in system by scenario
# ---------------------------------------------------------

g2 <- horizontal_boxplot(kpis_df, scenario, p95_time_in_system) +
  labs(
    title = "P95 time in system by scenario",
    x = "P95 time in system",
    y = "Scenario"
  )

print(g2)
ggsave("g2_p95_time_in_system_boxplot.png", g2, width = 11, height = 6.5)

# ---------------------------------------------------------
# 5) Plot 3: Balking and reneging by scenario
# ---------------------------------------------------------

kpis_long_abandon <- kpis_df %>%
  select(scenario, rep, pct_groups_balked, pct_groups_reneged) %>%
  pivot_longer(
    cols = c(pct_groups_balked, pct_groups_reneged),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      pct_groups_balked = "Balking",
      pct_groups_reneged = "Reneging"
    )
  )

g3 <- ggplot(kpis_long_abandon, aes(x = value, y = scenario)) +
  geom_boxplot(width = 0.65, fill = "#B7B7A4", colour = "#4A4A4A") +
  facet_wrap(~ metric, scales = "free_x") +
  common_theme +
  labs(
    title = "Balking and reneging by scenario",
    x = "Percentage",
    y = "Scenario"
  )

print(g3)
ggsave("g3_balking_reneging.png", g3, width = 12, height = 6.5)

# ---------------------------------------------------------
# 6) Plot 4: Throughput by scenario
# ---------------------------------------------------------

g4 <- horizontal_boxplot(kpis_df, scenario, rides_persons_per_hour) +
  labs(
    title = "Throughput by scenario",
    x = "Rides persons per hour",
    y = "Scenario"
  )

print(g4)
ggsave("g4_throughput_boxplot.png", g4, width = 11, height = 6.5)

# ---------------------------------------------------------
# 7) Summary by ride
# ---------------------------------------------------------

agg_time_summary <- agg_time_df %>%
  group_by(scenario, ride) %>%
  summarise(
    mean_queue_persons_time = mean(mean_queue_persons_time, na.rm = TRUE),
    mean_system_persons_time = mean(mean_system_persons_time, na.rm = TRUE),
    max_queue_persons = mean(max_queue_persons, na.rm = TRUE),
    max_system_persons = mean(max_system_persons, na.rm = TRUE),
    max_service_persons = mean(max_service_persons, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(agg_time_summary, "agg_time_summary.csv", row.names = FALSE)

tab_by_ride_summary <- tab_by_ride_df %>%
  group_by(scenario, ride) %>%
  summarise(
    rode_persons = mean(rode_persons, na.rm = TRUE),
    balked_persons = mean(balked_persons, na.rm = TRUE),
    reneged_persons = mean(reneged_persons, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(tab_by_ride_summary, "tab_by_ride_summary.csv", row.names = FALSE)

# ---------------------------------------------------------
# 8) Plot 5: Mean queue by ride and scenario
# ---------------------------------------------------------

g5 <- ggplot(
  agg_time_summary,
  aes(x = mean_queue_persons_time, y = ride, fill = scenario)
) +
  geom_col(position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values = soft_palette) +
  common_theme +
  labs(
    title = "Mean queue persons over time by ride and scenario",
    x = "Mean queue persons over time",
    y = "Ride",
    fill = "Scenario"
  )

print(g5)
ggsave("g5_mean_queue_by_ride.png", g5, width = 12, height = 6.5)

# ---------------------------------------------------------
# 9) Plot 6: Rode persons by ride and scenario
# ---------------------------------------------------------

g6 <- ggplot(
  tab_by_ride_summary,
  aes(x = rode_persons, y = ride, fill = scenario)
) +
  geom_col(position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values = soft_palette) +
  common_theme +
  labs(
    title = "Rode persons by ride and scenario",
    x = "Mean rode persons",
    y = "Ride",
    fill = "Scenario"
  )

print(g6)
ggsave("g6_rode_persons_by_ride.png", g6, width = 12, height = 6.5)

# ---------------------------------------------------------
# 10) Base vs informed comparison
# ---------------------------------------------------------

compare_base_informed <- kpis_df %>%
  filter(scenario %in% c(
    "base_low_load",
    "informed_low_load",
    "base_high_load",
    "informed_high_load"
  ))

g7 <- horizontal_boxplot(compare_base_informed, scenario, mean_time_in_system) +
  labs(
    title = "Base vs informed: mean time in system",
    x = "Mean time in system",
    y = "Scenario"
  )

print(g7)
ggsave("g7_base_vs_informed_mean_time.png", g7, width = 10, height = 5.5)

# ---------------------------------------------------------
# 11) Breakdown comparison
# ---------------------------------------------------------

compare_breakdowns <- kpis_df %>%
  filter(scenario %in% c(
    "informed_high_load",
    "informed_high_load_break_a3_close",
    "informed_high_load_break_a3_reduce_50"
  ))

g8 <- horizontal_boxplot(compare_breakdowns, scenario, rides_persons_per_hour) +
  labs(
    title = "Effect of breakdowns on throughput",
    x = "Rides persons per hour",
    y = "Scenario"
  )

print(g8)
ggsave("g8_breakdowns_throughput.png", g8, width = 11, height = 5.5)

# ---------------------------------------------------------
# 12) Sensitivity analysis for alpha
# ---------------------------------------------------------

alphas <- c(0, 0.01, 0.02, 0.05, 0.10)

alpha_runs <- lapply(alphas, function(a) {
  tmp <- run_replications(cfg, "informed_high_load", override_alpha = a)
  tmp$alpha_test <- a
  tmp
})

alpha_df <- do.call(rbind, alpha_runs)
write.csv(alpha_df, "alpha_sensitivity_raw.csv", row.names = FALSE)

g9 <- ggplot(alpha_df, aes(x = mean_time_in_system, y = factor(alpha_test))) +
  geom_boxplot(width = 0.65, fill = "#B7B7A4", colour = "#4A4A4A") +
  common_theme +
  labs(
    title = "Sensitivity analysis for alpha",
    x = "Mean time in system",
    y = "Alpha"
  )

print(g9)
ggsave("g9_alpha_sensitivity_mean_time.png", g9, width = 10, height = 5.5)

g10 <- ggplot(alpha_df, aes(x = rides_persons_per_hour, y = factor(alpha_test))) +
  geom_boxplot(width = 0.65, fill = "#B7B7A4", colour = "#4A4A4A") +
  common_theme +
  labs(
    title = "Sensitivity analysis for alpha: throughput",
    x = "Rides persons per hour",
    y = "Alpha"
  )

print(g10)
ggsave("g10_alpha_sensitivity_throughput.png", g10, width = 10, height = 5.5)

# ---------------------------------------------------------
# 13) Add one train to a3
# ---------------------------------------------------------

normal_capacity <- run_replications_full(
  cfg,
  "informed_high_load",
  override_n_trains = c(1, 1, 1, 1, 1)
)

add_train_a3 <- run_replications_full(
  cfg,
  "informed_high_load",
  override_n_trains = c(1, 1, 2, 1, 1)
)

normal_kpis <- do.call(rbind, lapply(normal_capacity, function(x) x$kpis))
normal_agg  <- do.call(rbind, lapply(normal_capacity, function(x) x$agg_time))

train_kpis <- do.call(rbind, lapply(add_train_a3, function(x) x$kpis))
train_agg  <- do.call(rbind, lapply(add_train_a3, function(x) x$agg_time))

normal_kpis$config <- "normal"
train_kpis$config <- "a3_two_trains"

normal_agg$config <- "normal"
train_agg$config <- "a3_two_trains"

cap_compare_kpis <- rbind(normal_kpis, train_kpis)
cap_compare_agg  <- rbind(normal_agg, train_agg)

write.csv(cap_compare_kpis, "cap_compare_kpis.csv", row.names = FALSE)
write.csv(cap_compare_agg, "cap_compare_agg.csv", row.names = FALSE)

g11 <- horizontal_boxplot(cap_compare_kpis, config, rides_persons_per_hour) +
  labs(
    title = "Effect of adding one train to a3 on throughput",
    x = "Rides persons per hour",
    y = "Configuration"
  )

print(g11)
ggsave("g11_add_train_a3_throughput.png", g11, width = 8.5, height = 5.5)

g12 <- horizontal_boxplot(
  cap_compare_agg %>% filter(ride == "a3"),
  config,
  mean_queue_persons_time
) +
  labs(
    title = "Effect of adding one train to a3 on mean queue at a3",
    x = "Mean queue persons at a3",
    y = "Configuration"
  )

print(g12)
ggsave("g12_add_train_a3_queue_a3.png", g12, width = 8.5, height = 5.5)

# ---------------------------------------------------------
# 14) Mathematical analysis: ride physical parameters
# ---------------------------------------------------------

rides_df <- build_rides_df(cfg$rides)

rides_math <- rides_df %>%
  mutate(
    service_rate_persons_per_sec = capacity_persons / cycle_mean,
    service_rate_persons_per_hour = 3600 * service_rate_persons_per_sec
  ) %>%
  select(
    ride, seats, n_trains, capacity_persons, cycle_mean,
    service_rate_persons_per_sec, service_rate_persons_per_hour
  )

print(rides_math)
write.csv(rides_math, "rides_math_table.csv", row.names = FALSE)

# ---------------------------------------------------------
# 15) Helper: scenario horizon in hours
# ---------------------------------------------------------

scenario_hours_df <- data.frame(
  scenario = sapply(cfg$scenarios, function(x) x$name),
  horizon_hours = sapply(cfg$scenarios, function(x) x$until / 3600),
  stringsAsFactors = FALSE
)

print(scenario_hours_df)
write.csv(scenario_hours_df, "scenario_hours.csv", row.names = FALSE)

# ---------------------------------------------------------
# 16) Estimate arrival rates by ride from simulation
# ---------------------------------------------------------

arrival_estimation_by_ride <- tab_by_ride_df %>%
  mutate(
    attempted_persons = rode_persons + balked_persons + reneged_persons
  ) %>%
  left_join(scenario_hours_df, by = "scenario") %>%
  group_by(scenario, ride) %>%
  summarise(
    attempted_persons_mean = mean(attempted_persons, na.rm = TRUE),
    rode_persons_mean = mean(rode_persons, na.rm = TRUE),
    balked_persons_mean = mean(balked_persons, na.rm = TRUE),
    reneged_persons_mean = mean(reneged_persons, na.rm = TRUE),
    horizon_hours = first(horizon_hours),
    lambda_hat_persons_per_hour = mean(attempted_persons, na.rm = TRUE) / first(horizon_hours),
    .groups = "drop"
  )

print(arrival_estimation_by_ride)
write.csv(arrival_estimation_by_ride, "arrival_estimation_by_ride.csv", row.names = FALSE)

# ---------------------------------------------------------
# 17) M/M/1 simplified model
# ---------------------------------------------------------

mm1_compare <- arrival_estimation_by_ride %>%
  left_join(
    rides_math %>% select(ride, service_rate_persons_per_hour),
    by = "ride"
  ) %>%
  rename(
    lambda = lambda_hat_persons_per_hour,
    mu = service_rate_persons_per_hour
  ) %>%
  mutate(
    rho = lambda / mu,
    Lq_mm1 = ifelse(rho < 1, rho^2 / (1 - rho), NA_real_),
    Wq_mm1_hours = ifelse(rho < 1 & lambda > 0, Lq_mm1 / lambda, NA_real_),
    Wq_mm1_seconds = 3600 * Wq_mm1_hours
  )

print(mm1_compare)
write.csv(mm1_compare, "mm1_compare_raw.csv", row.names = FALSE)

# ---------------------------------------------------------
# 18) Compare theoretical queue vs simulated queue
# ---------------------------------------------------------

sim_queue_by_ride <- agg_time_df %>%
  group_by(scenario, ride) %>%
  summarise(
    sim_mean_queue = mean(mean_queue_persons_time, na.rm = TRUE),
    sim_max_queue = mean(max_queue_persons, na.rm = TRUE),
    .groups = "drop"
  )

theory_vs_sim <- mm1_compare %>%
  left_join(sim_queue_by_ride, by = c("scenario", "ride")) %>%
  mutate(
    abs_error_Lq = abs(sim_mean_queue - Lq_mm1),
    rel_error_Lq = abs(sim_mean_queue - Lq_mm1) / pmax(abs(sim_mean_queue), 1e-8)
  )

print(theory_vs_sim)
write.csv(theory_vs_sim, "theory_vs_sim.csv", row.names = FALSE)

# ---------------------------------------------------------
# 19) Low-load validation only
# ---------------------------------------------------------

theory_vs_sim_low <- theory_vs_sim %>%
  filter(scenario %in% c("base_low_load", "informed_low_load"))

print(theory_vs_sim_low)
write.csv(theory_vs_sim_low, "theory_vs_sim_low_load.csv", row.names = FALSE)

# ---------------------------------------------------------
# 20) Error by scenario
# ---------------------------------------------------------

error_by_scenario <- theory_vs_sim %>%
  group_by(scenario) %>%
  summarise(
    mean_abs_error_Lq = mean(abs_error_Lq, na.rm = TRUE),
    mean_rel_error_Lq = mean(rel_error_Lq, na.rm = TRUE),
    mean_rho = mean(rho, na.rm = TRUE),
    .groups = "drop"
  )

print(error_by_scenario)
write.csv(error_by_scenario, "error_by_scenario.csv", row.names = FALSE)

# ---------------------------------------------------------
# 21) Plot 13: Observed utilization by ride and scenario
# ---------------------------------------------------------

g13 <- ggplot(
  throughput_by_ride,
  aes(x = utilization_from_service, y = ride, fill = scenario)
) +
  geom_col(
    position = position_dodge2(width = 0.8, preserve = "single"),
    colour = "#4A4A4A",
    linewidth = 0.2
  ) +
  scale_fill_manual(values = soft_palette) +
  common_theme +
  labs(
    title = "Observed utilization by ride and scenario",
    x = "Observed utilization",
    y = "Ride",
    fill = "Scenario"
  )

print(g13)
ggsave("g13_utilization_by_ride_and_scenario.png", g13, width = 12, height = 6.5)

# ---------------------------------------------------------
# 22) Plot 14: Maximum observed utilization by scenario
# ---------------------------------------------------------

max_utilization_by_scenario <- throughput_by_ride %>%
  group_by(scenario) %>%
  summarise(
    max_observed_utilization = max(utilization_from_service, na.rm = TRUE),
    .groups = "drop"
  )

g14 <- ggplot(
  max_utilization_by_scenario,
  aes(x = max_observed_utilization, y = scenario)
) +
  geom_col(
    fill = "#A5A58D",
    colour = "#4A4A4A",
    width = 0.7
  ) +
  common_theme +
  labs(
    title = "Maximum observed utilization by scenario",
    x = "Maximum observed utilization",
    y = "Scenario"
  )

print(g14)
ggsave("g14_max_utilization_by_scenario.png", g14, width = 11, height = 6.5)

# ---------------------------------------------------------
# 23) Extra 1: throughput by ride vs theoretical capacity
# ---------------------------------------------------------

throughput_by_ride <- tab_by_ride_df %>%
  left_join(scenario_hours_df, by = "scenario") %>%
  group_by(scenario, ride) %>%
  summarise(
    rode_persons_per_hour_sim = mean(rode_persons / horizon_hours, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    rides_math %>% select(ride, service_rate_persons_per_hour),
    by = "ride"
  ) %>%
  mutate(
    utilization_from_service = rode_persons_per_hour_sim / service_rate_persons_per_hour
  )

print(throughput_by_ride)
write.csv(throughput_by_ride, "throughput_by_ride_vs_capacity.csv", row.names = FALSE)

# ---------------------------------------------------------
# 24) Extra 2: automatic interpretation by scenario
# ---------------------------------------------------------

scenario_interpretation <- error_by_scenario %>%
  mutate(
    interpretation = case_when(
      mean_rel_error_Lq < 0.25 ~ "Approximation reasonably good",
      mean_rel_error_Lq < 0.60 ~ "Approximation moderate",
      TRUE ~ "Approximation poor"
    )
  )

print(scenario_interpretation)
write.csv(scenario_interpretation, "scenario_interpretation.csv", row.names = FALSE)

# ---------------------------------------------------------
# 25) Extra 3: estimated arrival rates by ride, base vs informed
# ---------------------------------------------------------

attempted_base_vs_informed <- arrival_estimation_by_ride %>%
  filter(scenario %in% c(
    "base_low_load",
    "informed_low_load",
    "base_high_load",
    "informed_high_load"
  ))

g15 <- ggplot(
  attempted_base_vs_informed,
  aes(x = lambda_hat_persons_per_hour, y = ride, fill = scenario)
) +
  geom_col(position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values = soft_palette) +
  common_theme +
  labs(
    title = "Estimated arrival rates by ride: base vs informed",
    x = "Estimated lambda (persons/hour)",
    y = "Ride",
    fill = "Scenario"
  )

print(g15)
ggsave("g15_arrival_rate_base_vs_informed.png", g15, width = 12, height = 6.5)

# ---------------------------------------------------------
# 26) Optional compact tables for quick inspection
# ---------------------------------------------------------

alpha_summary <- alpha_df %>%
  group_by(alpha_test) %>%
  summarise(
    mean_time_in_system = mean(mean_time_in_system, na.rm = TRUE),
    p95_time_in_system = mean(p95_time_in_system, na.rm = TRUE),
    pct_groups_balked = mean(pct_groups_balked, na.rm = TRUE),
    pct_groups_reneged = mean(pct_groups_reneged, na.rm = TRUE),
    rides_persons_per_hour = mean(rides_persons_per_hour, na.rm = TRUE),
    mean_queue_persons_time_overall = mean(mean_queue_persons_time_overall, na.rm = TRUE),
    max_queue_persons_overall = mean(max_queue_persons_overall, na.rm = TRUE),
    .groups = "drop"
  )

print(alpha_summary)
write.csv(alpha_summary, "alpha_summary.csv", row.names = FALSE)

train_summary <- cap_compare_kpis %>%
  group_by(config) %>%
  summarise(
    mean_time_in_system = mean(mean_time_in_system, na.rm = TRUE),
    p95_time_in_system = mean(p95_time_in_system, na.rm = TRUE),
    rides_persons_per_hour = mean(rides_persons_per_hour, na.rm = TRUE),
    pct_groups_balked = mean(pct_groups_balked, na.rm = TRUE),
    pct_groups_reneged = mean(pct_groups_reneged, na.rm = TRUE),
    .groups = "drop"
  )

print(train_summary)
write.csv(train_summary, "train_summary.csv", row.names = FALSE)

# ---------------------------------------------------------
# 27) Final message in console
# ---------------------------------------------------------

cat("\nAnalysis completed.\n")
cat("Main outputs created:\n")
cat("- summary_table_pretty.csv\n")
cat("- error_by_scenario.csv\n")
cat("- theory_vs_sim_low_load.csv\n")
cat("- rides_math_table.csv\n")
cat("- throughput_by_ride_vs_capacity.csv\n")
cat("- scenario_interpretation.csv\n")
cat("- Multiple PNG plots g1 ... g15\n")
