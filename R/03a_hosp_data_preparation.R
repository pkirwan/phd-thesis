# Figures and tables for Chapter 4

# Load libraries
here::i_am("R/03a_hosp_data_preparation.R")
library(here)
library(janitor)
library(flexsurv)
library(tidyverse)
library(survival)
library(finalfit)

cases <- read_csv(here("data/Hosp/data_2023-Dec-14.csv"))

save(cases, file = here("data/hosp_cases.RData"))

load(here("Data/Hosp/chess_week_feb.RData"))
load(here("Data/Hosp/chess_mixcure_apr.RData"))

# hospsen <- hosp |> filter(cohort2 == "Sentinel clinic")
hospsen <- hospsen |>
    filter(
        !(outcomestatus %in% c(
            "Discharge (no date)",
            "Death (no date)",
            "Transfer (no date)"
        )),
        !(icustatus == "ICU (date unknown)"),
        !is.na(agegroup),
        !sex == "Unknown"
    ) |>
    mutate(
        icu = case_when(
            icu == TRUE ~ "Admitted to ICU",
            icu == FALSE ~ "Not admitted to ICU"
        ),
        monthyear = paste0(monthofadmission, " ", yearofadmission),
        monthyear = factor(monthyear,
            levels = c("Mar 2020", "Apr 2020", "May 2020", "Jun/Jul/Aug 2020", "Sep 2020", "Oct 2020", "Nov 2020", "Dec 2020", "Jan 2021", "Feb 2021"),
            labels = c("Mar 2020", "Apr 2020", "May 2020", "Jun-Aug 2020", "Sep 2020", "Oct 2020", "Nov 2020", "Dec 2020", "Jan 2021", "Feb 2021")
        ),
        # monthyear = fct_collapse(monthyear, "Jun/Jul/Aug 2020" = c("Jun 2020", "Jul 2020", "Aug 2020")),
        # pheregion = fct_collapse(pheregion, "London/South of England" = c("London", "South of England"))
        ethgp = case_when(
            ethgp == "White (British/Irish/Other white)" ~ "White",
            ethgp == "Asian (Indian/Pakistani/Bangl/OtherAsian/Chinese)" ~ "Asian",
            ethgp == "Black (African/Carribean/OtherBlack)" ~ "Black",
            ethgp %in% c("Mixed ethnic group", "Other ethnic group") ~ "Mixed/Other",
            ethgp == "0" ~ "Unreported"
        )
    )

save(hospsen, file = here("Data/hosp_sentinel_data.RData"))

# # pre-processing
# ajh_month <- ajfit_flexsurvmix(fmh_month, start = "Hospital", B = 1000)
# aji_month <- ajfit_flexsurvmix(fmi_month, start = "Hospital", B = 1000)
# ajh_sex <- ajfit_flexsurvmix(fmh_month_sex, start = "Hospital", B = 1000)
# aji_sex <- ajfit_flexsurvmix(fmi_month_sex, start = "Hospital", B = 1000)

# # nd datasets
# nd <- tibble(monthofadmission = c("Mar", "Apr", "May", "Jun/Jul/Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb"))
# nd_sex <- expand_grid(nd, sex = c("Male", "Female"))
# nd_age <- expand_grid(nd, agegroup = c("[15,45)", "[45,65)", "[65,75)", "[75,110)"))
# nd_comorbid <- expand_grid(nd, comorbid_multip = c("0", "1", "2", "3"))

# wnd <- tibble(week = seq(1, 52), weeksine = sin(2 * pi * week / 52), weekcos = cos(2 * pi * week / 52))
# wnd_sex <- expand_grid(wnd, sex = c("Male", "Female"))
# wnd_age <- expand_grid(wnd, agegroup = c("[15,45)", "[45,65)", "[65,75)", "[75,110)"))
# wnd_comorbid <- expand_grid(wnd, comorbid_multip = c("0", "1", "2", "3"))

# ph_month <- probs_month(x = fmh_month, nd = nd)
# ph_sex <- probs_month(x = fmh_month_sex, nd = nd_sex)
# ph_age <- probs_month(x = fmh_month_age, nd = nd_age)
# ph_comorbid <- probs_month(x = fmh_month_comorbid, nd = nd_comorbid)

# pi_month <- probs_month(x = fmi_month, nd = nd)
# pi_sex <- probs_month(x = fmi_month_sex, nd = nd_sex)
# pi_age <- probs_month(x = fmi_month_age, nd = nd_age)
# pi_comorbid <- probs_month(x = fmi_month_comorbid, nd = nd_comorbid)

# pm_month <- probs_mix(x = fmh_month, y = fmi_month, nd = nd)
# pm_sex <- probs_mix(x = fmh_month_sex, y = fmi_month_sex, nd = nd_sex)
# pm_age <- probs_mix(x = fmh_month_age, y = fmi_month_age, nd = nd_age)
# pm_comorbid <- probs_mix(x = fmh_month_comorbid, y = fmi_month_comorbid, nd = nd_comorbid)

# qh_month <- quantile_month(x = fmh_month, nd = nd)
# qh_sex <- quantile_month(x = fmh_month_sex, nd = nd_sex)
# qh_age <- quantile_month(x = fmh_month_age, nd = nd_age)
# qh_comorbid <- quantile_month(x = fmh_month_comorbid, nd = nd_comorbid)

# qi_month <- quantile_month(x = fmi_month, nd = nd)
# qi_sex <- quantile_month(x = fmi_month_sex, nd = nd_sex)
# qi_age <- quantile_month(x = fmi_month_age, nd = nd_age)
# qi_comorbid <- quantile_month(x = fmi_month_comorbid, nd = nd_comorbid)

# pwh_week <- probs_week(x = fmh_week, nd = wnd)
# pwh_sex <- probs_week(x = fmh_week_sex, nd = wnd_sex)
# pwh_age <- probs_week(x = fmh_week_age, nd = wnd_age)
# pwh_comorbid <- probs_week(x = fmh_week_comorbid, nd = wnd_comorbid)

# pwi_week <- probs_week(x = fmi_week, nd = wnd)
# pwi_sex <- probs_week(x = fmi_week_sex, nd = wnd_sex)
# pwi_age <- probs_week(x = fmi_week_age, nd = wnd_age)
# pwi_comorbid <- probs_week(x = fmi_week_comorbid, nd = wnd_comorbid)

# fit_list <- c(
#     grep("^ajh_", ls(), value = TRUE),
#     grep("^aji_", ls(), value = TRUE),
#     grep("^ph_", ls(), value = TRUE),
#     grep("^pi_", ls(), value = TRUE),
#     grep("^pm_", ls(), value = TRUE),
#     grep("^qh_", ls(), value = TRUE),
#     grep("^qi_", ls(), value = TRUE),
#     grep("^pwh_", ls(), value = TRUE),
#     grep("^pwi_", ls(), value = TRUE)
# )

# # now doing the pre-processing in this file
# flexsurv_mix <- c(grep("fmh", ls(), value = TRUE), grep("fmi", ls(), value = TRUE))

# save(list = fit_list, file = here("Data/hosp_mixcure.RData"))

# # SUS data
# load(here("Data/Hosp/sus_ecds_processed.RData"))

# # ordered factor data
# level_order <- c(
#     "Mar 2020", "Apr 2020", "May 2020", "Jun 2020", "Jul 2020", "Aug 2020",
#     "Sep 2020", "Oct 2020", "Nov 2020", "Dec 2020", "Jan 2021", "Feb 2021",
#     "Mar 2021", "Apr 2021", "May 2021", "Jun 2021", "Jul 2021", "Aug 2021",
#     "Sep 2021", "Oct 2021", "Nov 2021", "Dec 2021", "Jan 2022", "Feb 2022",
#     "Mar 2022", "Apr 2022"
# )

# level_order_thinned <- c(
#     "Mar 2020", "May 2020", "Jul 2020", "Sep 2020", "Nov 2020", "Jan 2021",
#     "Mar 2021", "May 2021", "Jul 2021", "Sep 2021", "Nov 2021", "Jan 2022",
#     "Mar 2022"
# )

# age_order <- c("0-14", "15-24", "25-44", "45-64", "65-74", "75-84", "85+")

# # relevel vaccine
# hosp <- hosp |>
#     #filter(!monthyear %in% c("Mar 2022", "Apr 2022")) |>
#     mutate(
#         vaccine_long = case_when(
#             specimen_date - 84 > Third ~ ">12 weeks after third dose",
#             specimen_date - 42 > Third & specimen_date - 84 <= Third ~ "6-12 weeks after third dose",
#             specimen_date - 14 > Third & specimen_date - 42 <= Third ~ "2-6 weeks after third dose",
#             specimen_date >= Third ~ "0-2 weeks after third dose",
#             specimen_date - 84 > Second ~ ">12 weeks after second dose",
#             specimen_date - 42 > Second & specimen_date - 84 <= Second ~ "6-12 weeks after second dose",
#             specimen_date - 14 > Second & specimen_date - 42 <= Second ~ "2-6 weeks after second dose",
#             specimen_date >= Second ~ "0-2 weeks after second dose",
#             specimen_date - 84 > First ~ ">12 weeks after first dose",
#             specimen_date - 42 > First & specimen_date - 84 <= First ~ "6-12 weeks after first dose",
#             specimen_date - 21 > First & specimen_date - 42 <= First ~ "3-6 weeks after first dose",
#             specimen_date >= First ~ "0-3 weeks after first dose",
#             TRUE ~ "Unvaccinated"
#         ),
#         vaccine_long = factor(vaccine_long,
#             levels = c(
#                 "Unvaccinated",
#                 "0-3 weeks after first dose",
#                 "3-6 weeks after first dose",
#                 "6-12 weeks after first dose",
#                 ">12 weeks after first dose",
#                 "0-2 weeks after second dose",
#                 "2-6 weeks after second dose",
#                 "6-12 weeks after second dose",
#                 ">12 weeks after second dose",
#                 "0-2 weeks after third dose",
#                 "2-6 weeks after third dose",
#                 "6-12 weeks after third dose",
#                 ">12 weeks after third dose"
#             )
#         ),
#         monthyear = fct_drop(monthyear)
#     )


# # # make pdata for plots
# fg_data <- hosp |> mutate(
#     eventm = as.numeric(eventm),
#     eventm = if_else(is.na(eventm), 0, eventm),
#     eventm = factor(eventm, 0:2, labels = c("Censor", "Death", "Discharge"))
# )

# pdata <- finegray(Surv(fg_data$time1m, fg_data$eventm) ~ ., data = fg_data)

# pdata$ageGrp7 <- relevel(pdata$ageGrp7, ref = "15-24")
# pdata$monthyear <- relevel(pdata$monthyear, ref = "Jun 2020")
# pdata$charlson_index <- relevel(pdata$charlson_index, ref = "0")
# pdata$imd_quintile <- relevel(pdata$imd_quintile, ref = "5")
# pdata$sexHC <- relevel(pdata$sex, ref = "Female")
# pdata$vaccine_long <- relevel(pdata$vaccine_long, ref = "Unvaccinated")

# # generate expanded tibbles
# nd_month <- tibble(monthyear = level_order)
# nd_month_sex <- expand_grid(nd_month, sex = c("Male", "Female"))
# nd_month_age <- expand_grid(nd_month, ageGrp7 = age_order)
# nd_month_ethn <- expand_grid(nd_month, ethGrp4 = c("White", "Asian", "Black", "Mixed/Other"))
# nd_month_cci <- expand_grid(nd_month, charlson_index = c("0", "1-2", "3-4", "5+"))
# nd_month_load <- expand_grid(nd_month, bed_occupancy_cat = c("0-20%", "20-40%", "40-60%", "60-80%", "80-90%", "90-100%"))

# ajsus_month <- aj_fit(hosp, nd = nd_month)
# ajsus_age <- aj_fit(hosp, nd = nd_month_age)
# ajsus_load <- aj_fit(hosp, nd = nd_month_load)
# ajsus_sex <- aj_fit(hosp, nd = nd_month_sex)
# ajsus_ethn <- aj_fit(hosp, nd = nd_month_ethn)
# ajsus_cci <- aj_fit(hosp, nd = nd_month_cci)

# # fg fit
# fg_fit <- coxph(Surv(fgstart, fgstop, fgstatus) ~ monthyear,
#     weight = fgwt,
#     data = pdata
# )

# # generate a survival summary for fg_fit
# sf <- survfit(fg_fit, newdata = nd_month)

# number_order <- c(
#     "Mar 2020", "Dec 2020", "Jan 2021", "Feb 2021", "Mar 2021",
#     "Apr 2021", "May 2021", "Jun 2021", "Jul 2021", "Aug 2021",
#     "Sep 2021", "Apr 2020", "Oct 2021", "Nov 2021", "Dec 2021",
#     "Jan 2022", "Feb 2022", "Mar 2022", "Apr 2022", "May 2020", 
#     "Jun 2020", "Jul 2020", "Aug 2020", "Sep 2020", "Oct 2020", 
#     "Nov 2020"
# )

# st <- tidy(sf, state = "Death") |>
#     pivot_longer(-c(time, n.risk, n.event, n.censor),
#         names_to = c("est", "monthyear"), values_to = "val",
#         names_pattern = "([A-Za-z]+).([0-9]+)"
#     ) |>
#     pivot_wider(names_from = "est", values_from = "val") |>
#     mutate(
#         monthyear = factor(monthyear, labels = number_order)
#     ) |>
#     mutate(val = 1 - estimate, upper = 1 - low, lower = 1 - high)

# fit_list <- c(
#     grep("ajsus_", ls(), value = TRUE),
#     grep("nd_month", ls(), value = TRUE),
#     "st", "level_order", "number_order", "level_order_thinned"
# )

# save(list = fit_list, file = here("Data/hosp_sus.RData"))

# save(hosp, pdata, level_order, number_order, level_order_thinned, file = here("Data/hosp_pdata.RData"))

# # Re-do fg_fit_vaccine
# load(here("Data/hosp_pdata.RData"))

# dependent <- "Surv(fgstart, fgstop, fgstatus)"
# explanatory_multi <- c("vaccine_long", "sex", "strata(ageGrp7)", "strata(phec_name)", "strata(monthyear)", "ethGrp4", "imd_quintile", "charlson_index", "bed_occupancy_cat")

# pdata <- pdata |> filter(
#     grepl("2021", monthyear) | grepl("2022", monthyear)
# )

# coxfit_multi <- pdata |>
#     coxphmulti(dependent, explanatory_multi, weight = pdata$fgwt)

# save(coxfit_multi, file = here("Data/Hosp/coxfit_multi_vaccine.RData"))

# # Re-do fg_fit_month
# load(here("Data/hosp_pdata.RData"))

# dependent <- "Surv(fgstart, fgstop, fgstatus)"
# explanatory_multi <- c("monthyear", "sex", "strata(ageGrp7)", "strata(phec_name)", "strata(vaccine_long)", "ethGrp4", "imd_quintile", "charlson_index", "bed_occupancy_cat")

# coxfit_multi <- pdata |>
#     coxphmulti(dependent, explanatory_multi, weight = pdata$fgwt)

# save(coxfit_multi, file = here("Data/Hosp/coxfit_multi_month.RData"))


# # Re-do fg_fit_shift
# load(here("Data/hosp_pdata.RData"))

# # fg_shift datasets, take a very long time
# fg_fit_shift_0 <- fg_shift(hosp, shift = 0)
# fg_fit_shift_1 <- fg_shift(hosp, shift = 1)
# fg_fit_shift_2 <- fg_shift(hosp, shift = 2)
# fg_fit_shift_3 <- fg_shift(hosp, shift = 3)
# fg_fit_shift_4 <- fg_shift(hosp, shift = 4)

# tidy_fg_fit_shift <- bind_rows(
#   broom::tidy(fg_fit_shift_0, conf.int = TRUE, exponentiate = TRUE) |> head(n = 25) |> mutate(shift = "0 days"),
#   broom::tidy(fg_fit_shift_1, conf.int = TRUE, exponentiate = TRUE) |> head(n = 25) |> mutate(shift = "1 day"),
#   broom::tidy(fg_fit_shift_2, conf.int = TRUE, exponentiate = TRUE) |> head(n = 25) |> mutate(shift = "2 days"),
#   broom::tidy(fg_fit_shift_3, conf.int = TRUE, exponentiate = TRUE) |> head(n = 25) |> mutate(shift = "3 days"),
#   broom::tidy(fg_fit_shift_4, conf.int = TRUE, exponentiate = TRUE) |> head(n = 25) |> mutate(shift = "4 days"),
# )

# save(tidy_fg_fit_shift, file = here("Data/hosp_fg_shift.RData"))

## Nosocomial

# load(here("Data/Hosp/sfp_noso_dat_2022-01-09.RData"))

# noso <- sfp_nosocomial |>
#     select(specimen_date, hospital_in, hospital_out, phec_name, age, sexHC, ethnicity_final, phec_name, imd_decile, date_of_death, linkset) |>
#     filter(
#         !is.na(specimen_date),
#         specimen_date >= as_date("2020-03-01"),
#         specimen_date <= as_date("2021-09-30")
#     ) |>
#     filter(
#         hospital_out <= as_date("2022-02-28") | is.na(hospital_out),
#         !is.na(phec_name)
#     ) |>
#     mutate(
#         hosp_interval = as.numeric(hospital_out - specimen_date),
#         ttdeath = as.numeric(date_of_death - specimen_date),
#         eventm = if_else(!is.na(date_of_death) & as.numeric(date_of_death - specimen_date) <= 90 & linkset == "CoV:ECDS", "Death",
#             if_else(!is.na(date_of_death) & as.numeric(date_of_death - specimen_date) <= 90 & linkset != "CoV:ECDS" & as.numeric(date_of_death - hospital_out) <= 14, "Death",
#                 if_else(as.numeric(hospital_out - specimen_date) <= 90 & linkset != "CoV:ECDS", "Discharge",
#                     "Unknown final outcome"
#                 )
#             )
#         ),
#         ## Event or right censoring time.
#         timem = if_else(eventm == "Death", ttdeath,
#             if_else(eventm == "Discharge", hosp_interval,
#                 if_else(!is.na(hosp_interval) & (hosp_interval < ttdeath | is.na(ttdeath)) & linkset != "CoV:ECDS", hosp_interval, 0.5)
#             )
#         ),

#         ## Censoring status. Either exact+event known, interval censored + event known, or right censored  + event unknown
#         ## Unknown outcomes considered right censored (used for times to death or ICU)
#         statusm = if_else(eventm %in% c("Death", "Discharge"), 1, 3),

#         ## Event time for time to death or time to ICU components.
#         ## Exact time (time1,time1), right cens (time1,Inf), or int cens (0.5, time2)
#         time1m = timem,
#         time2m = if_else(statusm == 3, Inf, timem),
#         time2m = if_else(time2m == Inf & !is.na(date_of_death), ttdeath, time2m),
#         time1m = if_else(time1m == 0, 0.5, time1m),
#         time1m = if_else(time1m > 90 & eventm == "Unknown final outcome", 90, time1m),
#         time2m = if_else(time2m == 0, 0.5, time2m),
#         eventm = factor(eventm, levels = c("Death", "Discharge"), ordered = FALSE),
#         ethnicity_final = factor(ethnicity_final, ordered = FALSE),
#         ethGrp4 = fct_collapse(ethnicity_final,
#             Asian = c("Any other Asian background", "Bangladeshi (Asian or Asian British)", "Indian (Asian or Asian British)", "Indian (Asian or Asian British)", "Pakistani (Asian or Asian British)"),
#             Black = c("African (Black or Black British)", "Any other Black background", "Caribbean (Black or Black British)"),
#             `Mixed/Other/Unknown` = c("Any other ethnic group", "Any other Mixed background", "Chinese (other ethnic group)", "White and Asian (Mixed)", "White and Black African (Mixed)", "White and Black Caribbean (Mixed)", "Unknown"),
#             White = c("Any other White background", "British (White)", "Irish (White)")
#         ),
#         ethGrp4 = fct_relevel(ethGrp4, "White", "Asian", "Black", "Mixed/Other/Unknown"),
#         ageGrp7 = cut(age,
#             breaks = c(0, 15, 25, 45, 65, 75, 85, Inf),
#             right = FALSE,
#             include.lowest = TRUE, ordered.result = TRUE
#         ),
#         pheRegion = factor(phec_name, levels = c("London", "East Midlands", "East of England", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire and Humber"), ordered = FALSE),
#         imd_quintile = ceiling(imd_decile / 2) |> factor(),
#         monthyear = paste0(lubridate::month(specimen_date, label = TRUE), " ", year(specimen_date)),
#         monthyear = factor(monthyear, levels = level_order)
#     ) |>
#     filter(!is.na(time1m))

# save(noso, file = here("Data/hosp_sus_noso.RData"))
