here::i_am("R/04g_siren_thesis_data.R")

library(here)
library(tidyverse)

load(here("Data/SIREN/siren_processed_interim4_vax_final.RData"))
load(here("Data/siren_cohort.RData"))
load(here("Data/siren_expected_pos.RData"))
load(here("Data/siren_expected_pos_ft.RData"))
load(here("Data/siren_ppass.RData"))

# filter to participants in the current cohort
interim4_cohort <- siren_cohort |>
    filter(study_id %in% siren_df_interim4$study_id)

vaccine_uptake <- interim4_cohort |>
    mutate(vaccine = if_else(!is.na(vaccine_date4), "Fourth dose", "Waned third dose")) |>
    left_join(
        siren_df_interim4 |>
            arrange(study_id, time) |>
            select(study_id, months_since_pos) |>
            distinct(study_id, .keep_all = TRUE),
        by = "study_id"
    ) |>
    filter(months_since_pos != "No evidence of infection")

prop_vu_65 <- round(100 *
    (vaccine_uptake |> filter(agegr == "65+", vaccine == "Fourth dose") |> count() |> as.numeric()) /
    (vaccine_uptake |> filter(agegr == "65+") |> count() |> as.numeric()), 1)

prop_vu_2534 <- round(100 *
    (vaccine_uptake |> filter(agegr == "25-34", vaccine == "Fourth dose") |> count() |> as.numeric()) /
    (vaccine_uptake |> filter(agegr == "25-34") |> count() |> as.numeric()), 1)

prop_inf_naive <- round(100 *
    (vaccine_uptake |> filter(months_since_pos == "Confirmed naive", vaccine == "Fourth dose") |> count() |> as.numeric()) /
    (vaccine_uptake |> filter(months_since_pos == "Confirmed naive") |> count() |> as.numeric()), 1)

prop_inf_06 <- round(100 *
    (vaccine_uptake |> filter(months_since_pos == "0-6 months", vaccine == "Fourth dose") |> count() |> as.numeric()) /
    (vaccine_uptake |> filter(months_since_pos == "0-6 months") |> count() |> as.numeric()), 1)

prop_ft_wt <- round(100 *
    fortnightly_testing |>
        filter(variant_period == "Wild type", on_schedule == "Yes") |>
        count() |>
        as.numeric() /
    fortnightly_testing |>
        filter(variant_period == "Wild type") |>
        count() |>
        as.numeric(), 1)

prop_ft_alpha <- round(100 *
    fortnightly_testing |>
        filter(variant_period == "Alpha", on_schedule == "Yes") |>
        count() |>
        as.numeric() /
    fortnightly_testing |>
        filter(variant_period == "Alpha") |>
        count() |>
        as.numeric(), 1)

prop_ft_delta <- round(100 *
    fortnightly_testing |>
        filter(variant_period == "Delta", on_schedule == "Yes") |>
        count() |>
        as.numeric() /
    fortnightly_testing |>
        filter(variant_period == "Delta") |>
        count() |>
        as.numeric(), 1)

prop_ft_bq <- round(100 *
    fortnightly_testing |>
        filter(variant_period == "BQ.1/XBB.1.5", on_schedule == "Yes") |>
        count() |>
        as.numeric() /
    fortnightly_testing |>
        filter(variant_period == "BQ.1/XBB.1.5") |>
        count() |>
        as.numeric(), 1)

write(paste0("prop_vu_65, ", prop_vu_65), here("thesis_data.dat"), append = TRUE)
write(paste0("prop_vu_2534, ", prop_vu_2534), here("thesis_data.dat"), append = TRUE)
write(paste0("prop_inf_naive, ", prop_inf_naive), here("thesis_data.dat"), append = TRUE)
write(paste0("prop_inf_06, ", prop_inf_06), here("thesis_data.dat"), append = TRUE)

write(paste0("prop_ft_wt, ", prop_ft_wt), here("thesis_data.dat"), append = TRUE)
write(paste0("prop_ft_alpha, ", prop_ft_alpha), here("thesis_data.dat"), append = TRUE)
write(paste0("prop_ft_delta, ", prop_ft_delta), here("thesis_data.dat"), append = TRUE)
write(paste0("prop_ft_bq, ", prop_ft_bq), here("thesis_data.dat"), append = TRUE)

expected_infections <- rbind(
    "Bi-weekly sampling scheme" = make_quantiles(expected_sim_ft) - 157,
    "Observed sampling scheme" = make_quantiles(expected_sim) - 157
) |>
    as_tibble(rownames = "method") |>
    mutate(
        method = factor(method, levels = c("Observed sampling scheme", "Bi-weekly sampling scheme"))
    ) |>
    mutate(
        mean = round(mean, 0),
        lower = round(lower, 0),
        upper = round(upper, 0),
        estimate = paste0(mean, " (95\\% CI ", lower, " to ", upper, ")"),
        method = factor(method, levels = c("Observed sampling scheme", "Bi-weekly sampling scheme"))
    ) |>
    select(method, estimate)

exp_obs <- expected_infections |>
    filter(method == "Observed sampling scheme") |>
    pull(estimate)
exp_biw <- expected_infections |>
    filter(method == "Bi-weekly sampling scheme") |>
    pull(estimate)

inf_averted_vec <- outer(waned_ppass, boosted_ppass, "-") |>
    quantile(c(0.025, 0.5, 0.975)) |>
    round(0)
inf_averted <- paste0(inf_averted_vec[2], " (95\\% CI ", inf_averted_vec[1], " to ", inf_averted_vec[3], ")")

inf_averted_obs_vec <- outer(unlist(waned_sim), unlist(boosted_sim), "-") |>
    quantile(c(0.025, 0.5, 0.975)) |>
    round(0)
inf_averted_obs <- paste0(inf_averted_obs_vec[2], " (95\\% CI ", inf_averted_obs_vec[1], " to ", inf_averted_obs_vec[3], ")")

inf_averted_biw_vec <- outer(unlist(waned_sim_ft), unlist(boosted_sim_ft), "-") |>
    quantile(c(0.025, 0.5, 0.975)) |>
    round(0)
inf_averted_biw <- paste0(inf_averted_biw_vec[2], " (95\\% CI ", inf_averted_biw_vec[1], " to ", inf_averted_biw_vec[3], ")")

pct_averted_vec <- round(100 * (1 - outer(boosted_ppass, waned_ppass, "/")), 1) |>
    quantile(c(0.025, 0.5, 0.975)) |>
    round(1)
pct_averted <- paste0(pct_averted_vec[2], "\\% (95\\% CI ", pct_averted_vec[1], " to ", pct_averted_vec[3], "\\%)")

write(paste0("exp_obs, ", exp_obs), here("thesis_data.dat"), append = TRUE)
write(paste0("exp_biw, ", exp_biw), here("thesis_data.dat"), append = TRUE)
write(paste0("inf_averted, ", inf_averted), here("thesis_data.dat"), append = TRUE)
write(paste0("pct_averted, ", pct_averted), here("thesis_data.dat"), append = TRUE)
write(paste0("inf_averted_obs, ", inf_averted_obs), here("thesis_data.dat"), append = TRUE)
write(paste0("inf_averted_biw, ", inf_averted_biw), here("thesis_data.dat"), append = TRUE)
