# Figures and tables for Chapter 2

# Load libraries
here::i_am("R/02e_hiv_thesis_data.R")

library(here)
library(tidyverse)

# RITA test availability by CD4 strata
load(here("Data/hiv_msm_2023.RData"))

# extract proportion for RITA positive
prop_recent <- round(100 *
    (msm |> filter(earliesteventyear > 2010, earliesteventyear < 2020, recent == "Y") |> count() |> as.numeric()) /
    (msm |> filter(earliesteventyear > 2010, earliesteventyear < 2020, recent == "N") |> count() |> as.numeric()), 1)

# proportion with CD4<350 among 15-24 year olds with recent infection
prop_cd4_1524 <- round(100 *
    (msm |> filter(earliesteventyear > 2010, earliesteventyear < 2020, recent == "Y", agegpdiag == "15-24", cd4cat %in% c("<200", "200-350")) |> count() |> as.numeric()) /
    (msm |> filter(earliesteventyear > 2010, earliesteventyear < 2020, recent == "Y", agegpdiag == "15-24", cd4cat != "No CD4") |> count() |> as.numeric()), 1)

# proportion with CD4<350 among 25-34 year olds with recent infection
prop_cd4_2534 <- round(100 *
    (msm |> filter(earliesteventyear > 2010, earliesteventyear < 2020, recent == "Y", agegpdiag == "25-34", cd4cat %in% c("<200", "200-350")) |> count() |> as.numeric()) /
    (msm |> filter(earliesteventyear > 2010, earliesteventyear < 2020, recent == "Y", agegpdiag == "25-34", cd4cat != "No CD4") |> count() |> as.numeric()), 1)

# proportion with CD4<350 among 35-49 year olds with recent infection
prop_cd4_3549 <- round(100 *
    (msm |> filter(earliesteventyear > 2010, earliesteventyear < 2020, recent == "Y", agegpdiag == "35-49", cd4cat %in% c("<200", "200-350")) |> count() |> as.numeric()) /
    (msm |> filter(earliesteventyear > 2010, earliesteventyear < 2020, recent == "Y", agegpdiag == "35-49", cd4cat != "No CD4") |> count() |> as.numeric()), 1)

# proportion with CD4<350 among 50-64 year olds with recent infection
prop_cd4_5064 <- round(100 *
    (msm |> filter(earliesteventyear > 2010, earliesteventyear < 2020, recent == "Y", agegpdiag == "50-64", cd4cat %in% c("<200", "200-350")) |> count() |> as.numeric()) /
    (msm |> filter(earliesteventyear > 2010, earliesteventyear < 2020, recent == "Y", agegpdiag == "50-64", cd4cat != "No CD4") |> count() |> as.numeric()), 1)

# proportion with CD4<350 among 65+ year olds with recent infection
prop_cd4_65 <- round(100 *
    (msm |> filter(earliesteventyear > 2010, earliesteventyear < 2020, recent == "Y", agegpdiag == "65+", cd4cat %in% c("<200", "200-350")) |> count() |> as.numeric()) /
    (msm |> filter(earliesteventyear > 2010, earliesteventyear < 2020, recent == "Y", agegpdiag == "65+", cd4cat != "No CD4") |> count() |> as.numeric()), 1)

# availability of assay data
assay_avail_low <- round(100 *
    (msm |> filter(earliesteventyear == 2019, assay_test == "Assay result available") |> count() |> as.numeric()) /
    (msm |> filter(earliesteventyear == 2019) |> count() |> as.numeric()), 1)

assay_avail_high <- round(100 *
    (msm |> filter(earliesteventyear == 2014, assay_test == "Assay result available") |> count() |> as.numeric()) /
    (msm |> filter(earliesteventyear == 2014) |> count() |> as.numeric()), 1)

# append to thesis_data.dat
write(paste0("prop_recent, ", prop_recent), here("thesis_data.dat"), append = TRUE)
write(paste0("prop_cd4_1524, ", prop_cd4_1524), here("thesis_data.dat"), append = TRUE)
write(paste0("prop_cd4_2534, ", prop_cd4_2534), here("thesis_data.dat"), append = TRUE)
write(paste0("prop_cd4_3549, ", prop_cd4_3549), here("thesis_data.dat"), append = TRUE)
write(paste0("prop_cd4_5064, ", prop_cd4_5064), here("thesis_data.dat"), append = TRUE)
write(paste0("prop_cd4_65, ", prop_cd4_65), here("thesis_data.dat"), append = TRUE)
write(paste0("assay_avail_low, ", assay_avail_low), here("thesis_data.dat"), append = TRUE)
write(paste0("assay_avail_high, ", assay_avail_high), here("thesis_data.dat"), append = TRUE)

# probability of elimination
load(here("Data/hiv_forecast.RData"))
prob_250_2018 <- round(100 *
    (projection_2018 |> filter(year == 2025, freq <= 250) |> count() |> as.numeric()) /
    (projection_2018 |> filter(year == 2025) |> count() |> as.numeric()), 1)

prob_50_2018 <- round(100 *
    (projection_2018 |> filter(year == 2030, freq <= 50) |> count() |> as.numeric()) /
    (projection_2018 |> filter(year == 2030) |> count() |> as.numeric()), 1)

prob_250_2022 <- round(100 *
    (projection_2022 |> filter(year == 2025, freq <= 250) |> count() |> as.numeric()) /
    (projection_2022 |> filter(year == 2025) |> count() |> as.numeric()), 1)

prob_50_2022 <- round(100 *
    (projection_2022 |> filter(year == 2030, freq <= 50) |> count() |> as.numeric()) /
    (projection_2022 |> filter(year == 2030) |> count() |> as.numeric()), 1)

hiv_2030 <- projection_2022 |> filter(year == 2030) |> summarise(med = median(freq), lower = quantile(freq, 0.05), upper = quantile(freq, 0.95))
est_hiv_2030 <- paste0(round(hiv_2030$med, 0), " (90\\% CrI ", round(hiv_2030$lower, 0), " to ", round(hiv_2030$upper, 0), ")")
hiv_2025 <- projection_2022 |> filter(year == 2025) |> summarise(med = median(freq), lower = quantile(freq, 0.05), upper = quantile(freq, 0.95))
est_hiv_2025 <- paste0(round(hiv_2025$med, 0), " (90\\% CrI ", round(hiv_2025$lower, 0), " to ", round(hiv_2025$upper, 0), ")")

# append to thesis_data.dat
write(paste0("prob_250_2018, ", prob_250_2018), here("thesis_data.dat"), append = TRUE)
write(paste0("prob_50_2018, ", prob_50_2018), here("thesis_data.dat"), append = TRUE)
write(paste0("prob_250_2022, ", prob_250_2022), here("thesis_data.dat"), append = TRUE)
write(paste0("prob_50_2022, ", prob_50_2022), here("thesis_data.dat"), append = TRUE)
write(paste0("est_hiv_2030, ", est_hiv_2030), here("thesis_data.dat"), append = TRUE)
write(paste0("est_hiv_2025, ", est_hiv_2025), here("thesis_data.dat"), append = TRUE)