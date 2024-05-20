# data processing

here::i_am("R/02a_hiv_data_preparation.R")

library(readstata13)
library(tidyverse)
library(arrow)
library(here)

nd_archive <- read.dta13("C:/Users/peter.kirwan/Documents/hivdata/ND_Archive_20230901.dta")
nd_hars_match <- read.dta13("C:/Users/peter.kirwan/Documents/hivdata/ND_HARS_Match_20230830.dta")
hars_arvstart <- read_parquet("C:/Users/peter.kirwan/Documents/hivdata/hars_arvstart_2023.parquet")

# rita_archive <- read.dta13("C:/Users/peter.kirwan/Documents/hivdata/RITA_ND_Archive_20230901.dta")

msm <- nd_archive |>
    filter(
        pexpgp == "MSM", agediag > 14, is.na(dxabroadflag),
        !(pheregionres %in% c("", "Channel Islands", "Isle of Man", "Scotland"))
    ) |>
    left_join(nd_hars_match |> select(harspid, ndpid), by = "ndpid") |>
    left_join(hars_arvstart |> select(harspid, arvstart), by = "harspid") |>
    mutate(
        assay_test = if_else(!is.na(lag_result) | !is.na(avidity_result), "Assay result available", "Assay result not available"),
        test_type = if_else(!is.na(lag_result) & earliesteventyear > 2012, "LAg", if_else(!is.na(avidity_result) & earliesteventyear < 2014, "AxSYM", "")),
        ttcd4 = cd4date - earliesteventdate,
        ttcd4 = if_else(ttcd4 > 91 | ttcd4 < -14, as.difftime("NA", units = "days"), ttcd4),
        cd4date = if_else(is.na(ttcd4), NA_Date_, cd4date),
        cd4 = if_else(is.na(ttcd4), NA_real_, cd4),
        cd4cat = cut(cd4,
            breaks = c(0, 200, 350, 500, Inf), right = FALSE,
            labels = c("<200", "200-350", "350-500", "500+")
        ),
        cd4cat = fct_na_value_to_level(cd4cat, "No CD4"),
        ttlasttest = earliesteventdate - datelastneg,
        lasttestyear = case_when(
            ttlasttest < 183 ~ "Negative test <6 months",
            ttlasttest < 366 ~ "Negative test 6-12 months",
            ttlasttest >= 366 ~ "Negative test outside 12 months",
            TRUE ~ "No last negative test"
        ),
        lasttestyear = factor(lasttestyear, levels = c("Negative test <6 months", "Negative test 6-12 months", "Negative test outside 12 months", "No last negative test")),
        lastneg = if_else(is.na(ttlasttest), "No previous negative test", "Previous negative test"),
        lastneg = fct_relevel(lastneg, "Previous negative test", "No previous negative test"),
        ttvl = case_when(
            arvstart < vldate ~ as.difftime("NA", units = "days"),
            !is.na(vldate) ~ vldate - earliesteventdate,
            TRUE ~ as.difftime("NA", units = "days")
        ),
        ttvl = if_else(ttvl > 91 | ttvl < -14, as.difftime("NA", units = "days"), ttvl),
        vl_avail = if_else(!is.na(ttvl), "Baseline VL available", "Baseline VL not available"),
        vl = if_else(is.na(ttvl), NA_real_, vl),
        vlcat = cut(vl,
            breaks = c(0, 50, 10000, 100000, Inf), right = FALSE,
            labels = c("<50", "50-10,000", "10,000-100,000", "100,000+")
        ),
        pheregionres = factor(pheregionres, levels = c("London", "Midlands and East of England", "North of England", "South of England", "Northern Ireland", "Wales")),
        recent = case_when(
            vl < 400 & recent == "Y" ~ "N",
            cd4 < 50 & recent == "Y" ~ "N",
            TRUE ~ recent
        )
    )

save(msm, file = here("Data/hiv_msm_2023.RData"))

load(here("Data/HIV/ProcOutput_ai_1995_2014_data6.RData"))
load(here("Data/HIV/ProcOutput_ai_1995_2015_data6.RData"))
load(here("Data/HIV/ProcOutput_ai_1995_2016_data6.RData"))
load(here("Data/HIV/ProcOutput_ai_1995_2017_data6.RData"))
load(here("Data/HIV/ProcOutput_ai_1995_2018_data6.RData"))
load(here("Data/HIV/ProcOutput_ai_1995_2019_data6.RData"))
load(here("Data/HIV/ProcOutput_ai_1995_2017_npd_data6.RData"))
load(here("Data/HIV/ProcOutput_ai_1995_2018_npd_data6.RData"))
load(here("Data/HIV/ProcOutput_ai_1995_2019_npd_data6.RData"))
load(here("Data/HIV/ProcOutput_ai_1995_2020_data6.RData"))
load(here("Data/HIV/ProcOutput_ai_1995_2021_data6.RData"))
load(here("Data/HIV/ProcOutput_ai_1995_2022_data6.RData"))
load(here("Data/HIV/ProcOutput_ai_1995_2023_data6.RData"))

estimates <- grep("hiv_", ls(), value = TRUE)
save(list = estimates, file = here("Data/hiv_estimates.RData"))

load(here("Data/HIV/ProcOutput_rita_2011_2019_data6.RData"))
load(here("Data/HIV/ProcOutput_ai_2011_2019_data6.RData"))
load(here("Data/HIV/ProcOutput_ai_2011_2019_data6_reclass.RData"))
load(here("Data/HIV/rita_ai_2011_2019_data6.RData"))

save(hiv_2019_spl, hiv_2019_rita, hiv_2019_reclass, stan_data, file = here("Data/hiv_rita_estimates.RData"))

###################
# Simulated data
###################
load(here("Data/HIV/ProcOutput_sim.RData"))

inf_lambda <- tibble(
    x = seq(1, 128),
    y = c(
        seq(350, 450, length.out = 16),
        seq(450, 400, length.out = 28),
        seq(400, 600, length.out = 36),
        seq(600, 600, length.out = 10),
        seq(600, 250, length.out = 28),
        seq(250, 200, length.out = 10)
    ),
    year = seq(0, 31.75, by = 0.25),
    group = "lambda"
)

inf_poisson <- tibble(
    x = seq(1, 128),
    y = infs,
    group = "Pois(lambda)"
)

## Diagnosis probabilities (must be positive)
nd <- 128 / 2 - 10

diag_mu <- tibble(
    x = seq(1, 128),
    d1 = c(
        seq(0.15, 0.2, length.out = nd),
        rep(0.2, 20),
        seq(0.2, 0.15, length.out = nd)
    ),
    d2 = c(
        seq(0.05, 0.07, length.out = nd),
        rep(0.07, 20),
        seq(0.07, 0.05, length.out = nd)
    ),
    d3 = c(
        seq(0.08, 0.1, length.out = nd),
        rep(0.1, 20),
        seq(0.1, 0.08, length.out = nd)
    ),
    d4 = c(
        seq(0.11, 0.13, length.out = nd),
        rep(0.13, 20),
        seq(0.13, 0.11, length.out = nd)
    ),
    d5 = c(
        seq(0.25, 0.3, length.out = nd),
        rep(0.3, 20),
        seq(0.3, 0.25, length.out = nd)
    ),
    group = "delta"
)

diag_normal <- tibble(
    x = seq(1, 128),
    d1 = d1,
    d2 = d2,
    d3 = d3,
    d4 = d4,
    d5 = d5,
    group = "N(delta, 0.001^2)"
) |>
    slice(which(row_number() %% 8 == 1)) |>
    pivot_longer(names_to = "strata", values_to = "value", -c(x, group)) |>
    mutate(strata = factor(strata, labels = c("d[1]", "d[2]", "d[3]", "d[4]", "d[5]")))

## extract the simulated data to plot
undiag_prev <- tibble(
    value = rowSums(epi$lat),
    year = seq(0, 31.75, by = 0.25),
    group = "Simulated values"
)

incidence <- tibble(
    value = infs,
    year = seq(0, 31.75, by = 0.25),
    group = "Simulated values"
)

incidence_annual <- incidence |>
    mutate(year = floor(year)) |>
    group_by(year) |>
    summarise(value = sum(value)) |>
    mutate(group = "Simulated values")

diag_prob <- tibble(
    value = c(d1, d2, d3, d4, d5),
    year = rep(seq(0, 31.75, by = 0.25), 5),
    strata = rep(c("Recently acquired", "CD4>500", "CD4 350-500", "CD4 200-350", "CD4<200"), each = 128),
    group = "Simulated values"
) |>
    mutate(strata = factor(strata, levels = c("CD4<200", "CD4 200-350", "CD4 350-500", "CD4>500", "Recently acquired")))

diagnoses <- tibble(
    value = rowSums(epi$diags),
    year = seq(0, 31.75, by = 0.25),
    group = "Simulated values"
)

# parameter estimates
q <- c(0.0976, 0.115, 0.116, 0.148)

incidence_par <- tibble(
    value = inf_lambda$y,
    year = seq(0, 31.75, by = 0.25),
    group = "Parameter values"
)

diag_prob_par <- tibble(
    value = c(diag_mu$d1, diag_mu$d2, diag_mu$d3, diag_mu$d4, diag_mu$d5),
    year = rep(seq(0, 31.75, by = 0.25), 5),
    strata = rep(c("Recently acquired", "CD4>500", "CD4 350-500", "CD4 200-350", "CD4<200"), each = 128),
    group = "Parameter values"
) |>
    mutate(strata = factor(strata, levels = c("CD4<200", "CD4 200-350", "CD4 350-500", "CD4>500", "Recently acquired")))

Q_par <- lapply(1:128, function(x) Q_mat(x, diag_mu$d1, diag_mu$d2, diag_mu$d3, diag_mu$d4, diag_mu$d5, q, 7))
D_par <- lapply(1:128, function(x) D_mat(x, diag_mu$d1, diag_mu$d2, diag_mu$d3, diag_mu$d4, diag_mu$d5, q, 7, 6))
epi <- epi_mat(inf_lambda$y, Q_par, D_par, 7, 6, nquar = 128)

undiag_prev_par <- tibble(
    value = rowSums(epi$lat),
    year = seq(0, 31.75, by = 0.25),
    group = "Parameter values"
)

diagnoses_par <- tibble(
    value = rowSums(epi$diags),
    year = seq(0, 31.75, by = 0.25),
    group = "Parameter values"
)

# extract list elements from hiv_misclass[[i]]$incidence
list_extract <- function(list, name) {
    bind_cols(
        list[[1]][name],
        lapply(list, function(x) x[[name]]$value)
    ) |>
        select(-value) |>
        pivot_longer(names_to = "model", values_to = "value", cols = starts_with("...")) |>
        mutate(quarter = year * 4)
}

rm(hiv_misclass, hiv_rita, hiv_naive)
load(here("Data/HIV/hiv_misclass.RData"))
load(here("Data/HIV/hiv_naive.RData"))
load(here("Data/HIV/hiv_rita.RData"))

misclass_incidence <- list_extract(hiv_misclass, "incidence")
misclass_undiag_prev <- list_extract(hiv_misclass, "undiag_prev")
misclass_diag_prob <- list_extract(hiv_misclass, "diag_prob")
misclass_diagnoses <- list_extract(hiv_misclass, "diagnoses")

rita_incidence <- list_extract(hiv_rita, "incidence")
rita_undiag_prev <- list_extract(hiv_rita, "undiag_prev")
rita_diag_prob <- list_extract(hiv_rita, "diag_prob")
rita_diagnoses <- list_extract(hiv_rita, "diagnoses")

reclass_incidence <- list_extract(hiv_naive, "incidence")
reclass_undiag_prev <- list_extract(hiv_naive, "undiag_prev")
reclass_diag_prob <- list_extract(hiv_naive, "diag_prob")
reclass_diagnoses <- list_extract(hiv_naive, "diagnoses")

save(
    inf_lambda, inf_poisson, diag_mu, diag_normal,
    undiag_prev, undiag_prev_par, incidence, incidence_par, diag_prob, diag_prob_par, diagnoses, diagnoses_par,
    misclass_incidence, misclass_undiag_prev, misclass_diag_prob, misclass_diagnoses,
    rita_incidence, rita_undiag_prev, rita_diag_prob, rita_diagnoses,
    reclass_incidence, reclass_undiag_prev, reclass_diag_prob, reclass_diagnoses,
    file = here("data/hiv_sim_estimates.RData")
)

rm(hiv_misclass, hiv_rita, hiv_naive)
# load(here("Data/HIV/ProcOutput_sim_missing.RData"))
load(here("Data/HIV/hiv_misclass_missing.RData"))
load(here("Data/HIV/hiv_naive_missing.RData"))
load(here("Data/HIV/hiv_rita_missing.RData"))

misclass_incidence_miss <- list_extract(hiv_misclass, "incidence")
misclass_undiag_prev_miss <- list_extract(hiv_misclass, "undiag_prev")
misclass_diag_prob_miss <- list_extract(hiv_misclass, "diag_prob")
misclass_diagnoses_miss <- list_extract(hiv_misclass, "diagnoses")

rita_incidence_miss <- list_extract(hiv_rita, "incidence")
rita_undiag_prev_miss <- list_extract(hiv_rita, "undiag_prev")
rita_diag_prob_miss <- list_extract(hiv_rita, "diag_prob")
rita_diagnoses_miss <- list_extract(hiv_rita, "diagnoses")

reclass_incidence_miss <- list_extract(hiv_naive, "incidence")
reclass_undiag_prev_miss <- list_extract(hiv_naive, "undiag_prev")
reclass_diag_prob_miss <- list_extract(hiv_naive, "diag_prob")
reclass_diagnoses_miss <- list_extract(hiv_naive, "diagnoses")

save(
    misclass_incidence_miss, misclass_undiag_prev_miss, misclass_diag_prob_miss, misclass_diagnoses_miss,
    rita_incidence_miss, rita_undiag_prev_miss, rita_diag_prob_miss, rita_diagnoses_miss,
    reclass_incidence_miss, reclass_undiag_prev_miss, reclass_diag_prob_miss, reclass_diagnoses_miss,
    file = here("data/hiv_sim_estimates_missing.RData")
)


##################
# RITA data from paper
##################
nd_archive_raw <- read.dta13("C:/Users/peter.kirwan/Documents/hivdata/ND_Archive_20201012.dta")

# set up datasets
nd_archive <- nd_archive_raw %>%
    filter(
        earliesteventyear >= 2011 & earliesteventyear < 2020,
        agediag >= 15,
        pheregiondiag %in% c("Midlands and East of England", "South of England", "North of England", "London", "Northern Ireland", "Wales"),
        is.na(dxabroadflag)
    ) %>%
    mutate(
        lnflag = if_else(!is.na(datelastneg),
            if_else(earliesteventdate - datelastneg < 366, "<1 year",
                if_else(earliesteventdate - datelastneg < 732, "<2 years", ">2 years")
            ),
            "No test"
        ),
        reclassify = if_else(ldflag == "CD4 <350 (-14 to 91 days)", "Not reclassified", if_else(ldflag == "CD4>=350 (-14 to 91 days)", "CD4 >= 350", NA_character_)),
        reclassify = if_else(ldflag == "CD4 <350 (-14 to 91 days)" & recent == "Y" & lnflag %in% c("<1 year", "<2 years"), "Reclassified (RITA + Negative test)",
            if_else(ldflag == "CD4 <350 (-14 to 91 days)" & recent == "Y", "Reclassified (RITA)",
                if_else(ldflag == "CD4 <350 (-14 to 91 days)" & lnflag %in% c("<1 year", "<2 years"), "Reclassified (Negative test)", reclassify)
            )
        ),
        recent = if_else(recent == "Y", "RITA recent",
            if_else(recent == "N", "RITA not recent", "No RITA result")
        ),
        pexpgp = case_when(
            pexpgp == "Heterosexual Contact" & genderid == "Male (including trans man)" ~ "Heterosexual contact (men)",
            pexpgp == "Heterosexual Contact" & genderid == "Female (including trans woman)" ~ "Heterosexual contact (women)",
            pexpgp == "MSM" ~ "Sex between men"
        ),
        reclassify = factor(reclassify, levels = c("CD4 >= 350", "Reclassified (RITA + Negative test)", "Reclassified (RITA)", "Reclassified (Negative test)", "Not reclassified")),
        reclassifygp = fct_collapse(reclassify, "Reclassified" = c("Reclassified (RITA + Negative test)", "Reclassified (RITA)", "Reclassified (Negative test)"))
    )


# data frame for mixed scales
df2 <- expand_grid(earliesteventyear = seq(2011, 2019), pexpgp = c("All", "Sex between men", "Heterosexual contact (men)", "Heterosexual contact (women)"))
df2 <- df2 %>% mutate(
    value = case_when(
        pexpgp %in% c("All", "Sex between men") ~ 280,
        pexpgp %in% c("Heterosexual contact (men)", "Heterosexual contact (women)") ~ 50
    ),
    pexpgp = factor(pexpgp, levels = c("All", "Sex between men", "Heterosexual contact (men)", "Heterosexual contact (women)"))
)

totals <- nd_archive %>%
    filter(
        !is.na(reclassifygp),
        !is.na(pexpgp)
    ) %>%
    group_by(earliesteventyear, pexpgp) %>%
    summarise(total = n(), .groups = "drop_last") %>%
    bind_rows(
        nd_archive %>%
            filter(!is.na(reclassifygp)) %>%
            group_by(earliesteventyear) %>%
            summarise(total = n(), .groups = "drop_last") %>%
            mutate(pexpgp = "All")
    ) %>%
    mutate(pexpgp = factor(pexpgp, levels = c("All", "Sex between men", "Heterosexual contact (men)", "Heterosexual contact (women)")))

proportions <- nd_archive %>%
    filter(
        !is.na(reclassifygp),
        !is.na(pexpgp)
    ) %>%
    group_by(earliesteventyear, pexpgp, reclassifygp) %>%
    summarise(total = n(), .groups = "drop_last") %>%
    pivot_wider(names_from = reclassifygp, values_from = total) %>%
    bind_rows(
        nd_archive %>%
            filter(!is.na(reclassifygp)) %>%
            group_by(earliesteventyear, reclassifygp) %>%
            summarise(total = n(), .groups = "drop_last") %>%
            pivot_wider(names_from = reclassifygp, values_from = total) %>%
            mutate(pexpgp = "All")
    ) %>%
    left_join(totals, by = c("earliesteventyear", "pexpgp")) %>%
    mutate(
        `CD4 >= 350` = `CD4 >= 350` / total,
        Reclassified = Reclassified / total,
        `Not reclassified` = `Not reclassified` / total,
        pexpgp = factor(pexpgp, levels = c("All", "Sex between men", "Heterosexual contact (men)", "Heterosexual contact (women)")),
    ) %>%
    select(-total) %>%
    pivot_longer(c(`CD4 >= 350`, Reclassified, `Not reclassified`), names_to = "reclassifygp", values_to = "prop") %>%
    mutate(reclassifygp = if_else(reclassifygp == "Not reclassified", "zNot reclassified", reclassifygp))

counts <- nd_archive %>%
    filter(!is.na(pexpgp), reclassifygp == "Reclassified") %>%
    bind_rows(nd_archive %>% mutate(pexpgp = "All") %>% filter(reclassifygp == "Reclassified")) %>%
    group_by(earliesteventyear, pexpgp, reclassify) %>%
    summarise(total = n(), .groups = "drop_last") %>%
    mutate(
        pexpgp = factor(pexpgp, levels = c("All", "Sex between men", "Heterosexual contact (men)", "Heterosexual contact (women)")),
        reclassify = factor(reclassify, levels = c("Reclassified (RITA + Negative test)", "Reclassified (RITA)", "Reclassified (Negative test)"))
    )

save(nd_archive, df2, totals, proportions, counts, file = here("Data/hiv_ld_paper.RData"))

#########
# HIV projections
#########

# load data
load(here("Data/HIV/forecast_1995_2018_data6.RData"))

projection_2018 <- as_tibble(as.table(rfv.gam.tot.byyear), .name_repair = "unique") |>
    rename(freq = n) |>
    filter(...2 >= 2016) |>
    mutate(
        year = as.numeric(...2),
        year = factor(year),
        inc_group = if_else(year %in% c(2016, 2017, 2018), "Estimated incidence", "Projected incidence")
    ) |>
    select(year, freq, inc_group) |>
    filter(!freq > 10000)

load(here("Data/HIV/forecast_1995_2019_data6.RData"))

projection_2019 <- as_tibble(as.table(rfv.gam.tot.byyear), .name_repair = "unique") |>
    rename(freq = n) |>
    filter(...2 >= 2016) |>
    mutate(
        year = as.numeric(...2),
        year = factor(year),
        inc_group = if_else(year %in% c(2016, 2017, 2018, 2019), "Estimated incidence", "Projected incidence")
    ) |>
    select(year, freq, inc_group) |>
    filter(!freq > 10000)

load(here("Data/HIV/forecast_1995_2021_data6.RData"))
projection_2021 <- as_tibble(as.table(rfv.gam.tot.byyear), .name_repair = "unique") |>
    rename(freq = n) |>
    filter(...2 >= 2016) |>
    mutate(
        year = as.numeric(...2),
        year = factor(year),
        inc_group = if_else(year %in% c(2016, 2017, 2018, 2019, 2020, 2021), "Estimated incidence", "Projected incidence")
    ) |>
    select(year, freq, inc_group) |>
    filter(!freq > 10000)

load(here("Data/HIV/forecast_1995_2022_data6.RData"))
projection_2022 <- as_tibble(as.table(rfv.gam.tot.byyear), .name_repair = "unique") |>
    rename(freq = n) |>
    filter(...2 >= 2016) |>
    mutate(
        year = as.numeric(...2),
        year = factor(year),
        inc_group = if_else(year %in% c(2016, 2017, 2018, 2019, 2020, 2021, 2022), "Estimated incidence", "Projected incidence")
    ) |>
    select(year, freq, inc_group) |>
    filter(!freq > 10000)

save(projection_2018, projection_2019, projection_2021, projection_2022, file = here("Data/hiv_forecast.RData"))
