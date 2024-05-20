# Figures and tables for Chapter 4

# Load libraries
here::i_am("R/04a_siren_data_preparation.R")
library(here)
library(janitor)
library(msm)
library(tidyverse)
library(readstata13)
library(readxl)

# load data
pt <- read.dta13("C:/Users/peter.kirwan/Documents/coviddata/PT_20230118.dta", convert.factors = TRUE, nonint.factors = TRUE) |>
    clean_names()

pcr <- read.dta13("C:/Users/peter.kirwan/Documents/coviddata/PCR_20230421_clean.dta") |>
    clean_names()

fu <- read.dta13("C:/Users/peter.kirwan/Documents/coviddata/FollowUp_20230421.dta") |>
    clean_names()

full_cohort <- pt |>
    filter(
        (vaccine_date1 < vaccine_date2 | is.na(vaccine_date2) | is.na(vaccine_date1)),
        (vaccine_date1 < vaccine_date3 | is.na(vaccine_date3) | is.na(vaccine_date1)),
        (vaccine_date2 < vaccine_date3 | is.na(vaccine_date3) | is.na(vaccine_date2)),
        !gender %in% c("Non_binary", "Prefer_not"),
        ethnicity != "Prefer_not"
    ) |>
    mutate(
        agegr = factor(agegr, labels = c("<25", "25-34", "35-44", "45-54", "55-64", "65+")),
        gender = factor(gender, labels = c("Male", "Female")),
        ethnicity = factor(ethnicity, labels = c("White", "Asian", "Black", "Mixed", "Other")),
        region = factor(region),
        occupation_setting = if_else(is.na(occupation_setting_ext), occupation_setting, occupation_setting_ext),
        occupation_setting = if_else(is.na(occupation_setting), "Other", as.character(occupation_setting)),
        occupation_setting = factor(occupation_setting, labels = c(
            "Ambulance/Emergency department/Inpatient wards",
            "Intensive care",
            "Maternity/Labour ward",
            "Office",
            "Other",
            "Outpatient",
            "Patient-facing (non-clinical)",
            "Theatres"
        )),
        occupation_setting = fct_relevel(occupation_setting, c(
            "Ambulance/Emergency department/Inpatient wards",
            "Intensive care",
            "Maternity/Labour ward",
            "Office",
            "Outpatient",
            "Patient-facing (non-clinical)",
            "Theatres",
            "Other"
        )),
        staff_type = as.double(staff_type),
        staff_type = if_else(staff_type == 11 & patient_contact == "No", 12, staff_type), # incorrectly coded in source data
        staff_type = factor(staff_type, levels = 1:12, labels = c(
            "Administrative/Executive (office based)", "Doctor", "Nursing", "Healthcare Assistant", "Midwife", "Healthcare Scientist", "Pharmacist",
            "Physiotherapist/Occupational Therapist/SALT", "Student (Medical/Nursing/Midwifery/Other)", "Estates/Porters/Security",
            "Other (non-patient facing)", "Other (patient facing)"
        )),
        imd = factor(imd_5, labels = c("Most deprived (1)", "Deprivation 2", "Deprivation 3", "Deprivation 4", "Least deprived (5)")),
        household = factor(household),
        household = factor(household,
            levels = c("Lives_alone", "Lives_with_others_nochild", "Lives_with_others_with_child"),
            labels = c("Lives alone", "Lives with others (no children)", "Lives with others (including children)")
        ),
        household = fct_relevel(household, "Lives with others (including children)", "Lives with others (no children)", "Lives alone"),
        medical_group = factor(medical_group, labels = c("No medical conditions", "Immunosuppression", "Chronic respiratory condition", "Chronic non-respiratory condition")),
        patient_contact = factor(patient_contact, labels = c("Yes", "No")),
    )

full_cohort |> count() # n = 43,996 records

symptom_window <- 14

symptoms <- fu |>
    mutate(
        covid_symptoms = rowSums(across(cough_fu:itchy_red_patches_fu), na.rm = TRUE),
        covid_symptoms = if_else(covid_symptoms > 0 | !is.na(sx_onset_fu), 1, 0),
        covid_symptoms = if_else((!is.na(sorethroat_fu) | !is.na(cough_fu) | !is.na(fever_fu) | !is.na(anosmia_fu) | !is.na(dygeusia_fu)), 2, covid_symptoms),
        symptom_start = case_when(
            covid_symptoms > 0 & !is.na(sx_onset_fu) ~ sx_onset_fu - symptom_window,
            covid_symptoms > 0 & is.na(sx_onset_fu) ~ date_survey_fu - symptom_window,
            TRUE ~ rep_start
        ),
        symptom_end = case_when(
            covid_symptoms > 0 & !is.na(sx_onset_fu) ~ sx_onset_fu + symptom_window,
            covid_symptoms > 0 & is.na(sx_onset_fu) ~ date_survey_fu + symptom_window,
            TRUE ~ rep_end
        )
    ) |>
    full_join(
        pcr |> filter(prim == 1 | rein == 1) |> select(study_id, specimen_date),
        by = "study_id",
        relationship = "many-to-many"
    ) |>
    filter(
        specimen_date > symptom_start,
        specimen_date < symptom_end
    ) |>
    arrange(study_id, specimen_date, -covid_symptoms) |>
    distinct(study_id, specimen_date, .keep_all = TRUE) |>
    select(study_id, specimen_date, covid_symptoms) |>
    mutate(covid_symptoms = factor(covid_symptoms, levels = c(0, 1, 2), labels = c("Asymptomatic", "Other symptoms", "COVID symptoms")))

fortnightly_testing <- pcr |>
    left_join(
        full_cohort |>
            select(study_id, date_enrolled, study_end_date) |>
            distinct(study_id, .keep_all = TRUE),
        by = "study_id"
    ) |>
    filter(
        !is.na(date_enrolled),
        specimen_date >= date_enrolled,
        specimen_date <= study_end_date
    ) |>
    mutate(
        time = specimen_date - date_enrolled,
        fortnight = floor(time / 14) |> as.integer(),
        cohort = case_when(
            specimen_date - date_enrolled <= 370 ~ "Initial cohort",
            specimen_date - date_enrolled <= 733 ~ "First extension",
            TRUE ~ "Second extension"
        ),
        variant_period = case_when(
            specimen_date < as_date("2021-01-01") ~ "Wild type",
            specimen_date < as_date("2021-05-01") ~ "Alpha",
            specimen_date < as_date("2021-08-01") ~ NA_character_,
            specimen_date < as_date("2021-12-01") ~ "Delta",
            specimen_date < as_date("2022-03-01") ~ "BA.1",
            specimen_date < as_date("2022-06-01") ~ "BA.2",
            specimen_date < as_date("2022-10-01") ~ "BA.4/5",
            specimen_date < as_date("2023-03-31") ~ "BQ.1/XBB.1.5",
            TRUE ~ NA_character_
        ),
        variant_period = fct_relevel(variant_period, "Wild type", "Alpha", "Delta", "BA.1", "BA.2", "BA.4/5", "BQ.1/XBB.1.5")
    ) |>
    distinct(study_id, fortnight, variant_period) |>
    group_by(study_id, variant_period) |>
    summarise(
        tests = n(),
        fortnights = max(fortnight) - min(fortnight)
    ) |>
    ungroup() |>
    mutate(
        on_schedule = if_else(tests >= (fortnights * 0.85), "Yes", "No"),
        on_schedule = factor(on_schedule, levels = c("Yes", "No"))
    )

save(full_cohort, pcr, symptoms, fortnightly_testing, file = here("Data/siren_cohort.RData"))

seq <- read_xlsx(here("Data/SIREN/Interim6_positives_seq.xlsx")) |>
    clean_names()

seq_df <- seq |>
    filter(!is.na(variant_name)) |>
    group_by(variant_name) |>
    summarise(
        n = n(),
        pct = n() / nrow(seq |> filter(!is.na(variant_name))),
        .groups = "drop"
    ) |>
    arrange(desc(n)) |>
    mutate(variant_name = factor(variant_name, levels = variant_name))

save(seq_df, file = here("Data/siren_seq_df.RData"))

# extract hazards
load(here("Data/SIREN/msm_models_interim4_confnaive_sep12_final.RData"))
load(here("Data/SIREN/msm_models_interim4_confnaive_sep12_noev_final.RData"))
load(here("Data/SIREN/msm_models_interim4_confnaive_sep12_sym_final.RData"))

hazards_mllvrahgo <- forest_plot(mllvrahgo, table = TRUE, covars = "vaccine") |>
    bind_rows(
        forest_plot(msvrahgo, table = TRUE, covars = "vaccine_short")
    ) |>
    distinct(name, .keep_all = TRUE) |>
    mutate(
        est = 1 - est,
        lower = 1 - lower,
        upper = 1 - upper,
        name = factor(name,
            levels = c("Waned third dose", "Fourth dose", "Fourth dose 0-2 months", "Fourth dose 2-4 months", "Fourth dose 4+ months"),
            labels = c("Waned\nthird\ndose", "Fourth\ndose", "Fourth dose\n0-2 months", "Fourth dose\n2-4 months", "Fourth dose\n4-6 months")
        )
    )

hazards_mmsvrahgo_noev <- forest_plot(mmsvrahgo_noev, table = TRUE, covars = "months_since_pos") |>
    mutate(
        est = 1 - est,
        lower = 1 - lower,
        upper = 1 - upper,
        name = factor(name,
            levels = c("Confirmed naive", "2+ years", "1-2 years", "6-12 months", "0-6 months"),
            labels = c("Confirmed\nnaive", "2+\nyears", "1-2\nyears", "6-12\nmonths", "0-6\nmonths")
        )
    )

hazards_mmsvrahgo_vacc <- forest_plot(mmsvrahgo_noev, table = TRUE, covars = "vaccine_short") |>
    mutate(
        est = 1 - est,
        lower = 1 - lower,
        upper = 1 - upper,
        name = factor(name,
            levels = c("Waned third dose", "Fourth dose"),
            labels = c("Waned\nthird\ndose", "Fourth\ndose")
        )
    )

hazards_mmsvrahgo_interaction_noev <- forest_plot(mmsvrahgo_interaction_noev, table = TRUE, covars = "months_since_pos:vaccine_short") |>
    mutate(
        est = 1 - est,
        lower = 1 - lower,
        upper = 1 - upper,
        name = gsub(":Fourth dose", "", name),
        name = factor(name,
            levels = c("Confirmed naive", "2+ years", "1-2 years", "6-12 months", "0-6 months")
            # labels = c("Confirmed\nnaive", "2+\nyears", "1-2\nyears", "6-12\nmonths", "0-6\nmonths")
        )
    )

hazards_msvrahgo_sym <- forest_plot(msvrahgo_sym, table = TRUE, transition2 = "State 1 - State 3", covars = "vaccine_short") |>
    mutate(
        est = 1 - est,
        lower = 1 - lower,
        upper = 1 - upper,
        name = factor(name,
            levels = c("Waned third dose", "Fourth dose"),
            labels = c("Waned\nthird\ndose", "Fourth\ndose")
        )
    )

hazards_mllvrahgo_sym <- forest_plot(mllvrahgo_sym, table = TRUE, transition2 = "State 1 - State 3", covars = "vaccine") |>
    mutate(
        est = 1 - est,
        lower = 1 - lower,
        upper = 1 - upper,
        name = factor(name,
            levels = c("Waned third dose", "Fourth dose 0-2 months", "Fourth dose 2-4 months", "Fourth dose 4+ months"),
            labels = c("Waned\nthird dose", "Fourth dose\n0-2 months", "Fourth dose\n2-4 months", "Fourth dose\n4-6 months")
        )
    )

hazards_mmsvrahgo_sym <- forest_plot(mmsvrahgo_sym, table = TRUE, transition2 = "State 1 - State 3", covars = "months_since_pos") |>
    mutate(
        est = 1 - est,
        lower = 1 - lower,
        upper = 1 - upper,
        name = factor(name,
            levels = c("Confirmed naive", "2+ years", "1-2 years", "6-12 months", "0-6 months"),
            labels = c("Confirmed\nnaive", "2+\nyears", "1-2\nyears", "6-12\nmonths", "0-6\nmonths")
        )
    )

save(hazards_mllvrahgo, hazards_mmsvrahgo_noev, hazards_mmsvrahgo_interaction_noev, hazards_msvrahgo_sym, hazards_mllvrahgo_sym, hazards_mmsvrahgo_sym, file = here("Data/siren_hazards.RData"))
save(mllvrahgo, msvrahgo, mmsvrahgo_noev, mmsvrahgo_interaction_noev, msvrahgo_sym, mllvrahgo_sym, mmsvrahgo_sym, mmllvrahgo_sym, file = here("Data/siren_models.RData"))

# time consuming functions, recorded in case of need to rerun
# model diagnostics
# load(here("Data/SIREN/siren_processed_interim4_vax_final.RData"))
# load(here("Data/SIREN/msm_models_interim4_confnaive_sep12_noev_final.RData"))

# start_date <- as_date("2022-09-12")

# study_ids <- siren_df_interim4 |>
#     select(study_id, state, time, specimen_date, months_since_pos) |>
#     filter(state != 99, specimen_date >= start_date) |>
#     add_count(study_id) |>
#     filter(n > 1) |>
#     distinct()

# siren_df <- siren_df_interim4 |>
#     filter(
#         study_id %in% study_ids$study_id,
#         state != 99,
#         specimen_date >= start_date
#     ) |>
#     mutate(
#         monthyear = fct_drop(monthyear),
#         prev_var = fct_drop(prev_var)
#     )

# diagnostics <- model_results(mmsvrahgo_noev,
#     dataset = siren_df,
#     mintime = 0,
#     timezero = 0,
#     maxtime = 28.571428571428,
#     piecewise.times = c(2.71428571428571, 7.14285714285714, 11.4285714285714, 15.8571428571429, 20.2857142857143, 24.2857142857143),
#     piecewise.covariates = list(list(monthyear = "Sep 2022"), list(monthyear = "Oct 2022"), list(monthyear = "Nov 2022"), list(monthyear = "Dec 2022"), list(monthyear = "Jan 2023"), list(monthyear = "Feb 2023"), list(monthyear = "Mar 2023"))
# )

# # Pearson-type goodness-of-fit test using updated R function
# source(here("R/00_pearson_msm.R"))
# deviance <- pearson_msm(mmsvrahgo_noev, timegroups = 2, intervalgroups = 2, covgroups = 2, boot = FALSE) # no bootstrapping

# save(diagnostics, deviance, file = here("Data/siren_diagnostics.RData"))

# Sojourn time

# load(here("Data/SIREN/msm_models_interim4_confnaive_sep12_rev_final.RData"))
# load(here("Data/SIREN/msm_models_interim4_confnaive_sep12_sym_final.RData"))

# # to get the covariate values
# # debug(qmatrix.msm)
# # qmatrix.msm(msvag_just_rev, covariates = list(vaccine_short = "Waned third dose"))
# # step through using Return
# # query covlist to get the values for the below
# # Q to exit
# # undebug(qmatrix.msm)

# #sojourn time overall
# S9 <- sojourn.msm(msvag_just_rev)

# # sojorn time for symptomatic
# S8 <- sojourn.msm(mmsvag_sym_rev)

# # trace("qmatrix.msm", edit = TRUE)
# # comment out the
# # covlist <- msm.parse.covariates(x, covariates, x$qcmodel)
# # argument on line 8 of the file
# # ensure to undo this after running the function

# # sojorn time for waned third dose
# covlist <- list(`vaccine_shortFourth dose` = -0.5947409)
# S1 <- sojourn.msm(msvag_just_rev)

# # sojorn time for fourth dose
# covlist <- list(`vaccine_shortFourth dose` = 1 - 0.5947409)
# S2 <- sojourn.msm(msvag_just_rev)

# # sojorn time for 2+ years, waned third dose
# covlist <- list(
#     `months_since_pos1-2 years` = -0.1472338,
#     `months_since_pos6-12 months` = -0.3529958,
#     `months_since_pos0-6 months` = -0.2316094,
#     `months_since_posConfirmed naive` = -0.1055653
# )
# S3 <- sojourn.msm(msvag_just_rev)

# # sojorn time for 1-2 years, waned third dose
# covlist <- list(
#     `months_since_pos1-2 years` = 1 - 0.1472338,
#     `months_since_pos6-12 months` = -0.3529958,
#     `months_since_pos0-6 months` = -0.2316094,
#     `months_since_posConfirmed naive` = -0.1055653
# )
# S4 <- sojourn.msm(msvag_just_rev)

# # sojorn time for 6-12 mo, waned third dose
# covlist <- list(
#     `months_since_pos1-2 years` = -0.1472338,
#     `months_since_pos6-12 months` = 1 - 0.3529958,
#     `months_since_pos0-6 months` = -0.2316094,
#     `months_since_posConfirmed naive` = -0.1055653
# )
# S5 <- sojourn.msm(msvag_just_rev)

# # sojorn time for 0-6mo, waned third dose
# covlist <- list(
#     `months_since_pos1-2 years` = -0.1472338,
#     `months_since_pos6-12 months` = -0.3529958,
#     `months_since_pos0-6 months` = 1 - 0.2316094,
#     `months_since_posConfirmed naive` = -0.1055653
# )
# S6 <- sojourn.msm(msvag_just_rev)

# # sojorn time for naive, waned third dose
# covlist <- list(
#     `months_since_pos1-2 years` = -0.1472338,
#     `months_since_pos6-12 months` = -0.3529958,
#     `months_since_pos0-6 months` = -0.2316094,
#     `months_since_posConfirmed naive` = 1 - 0.1055653
# )
# S7 <- sojourn.msm(msvag_just_rev)

# # table with sojourn times for each category
# sojourn_times <- tibble(
#     cat = c("Whole population", "Waned third dose", "Fourth dose", "2+ years", "1-2 years", "6-12 months", "0-6 months", "Confirmed naive", "COVID symptoms", "Non-COVID symptoms\nor asymptomatic"),
#     sojourn = c(S9$estimates[2] * 7, S1$estimates[2] * 7, S2$estimates[2] * 7, S3$estimates[2] * 7, S4$estimates[2] * 7, S5$estimates[2] * 7, S6$estimates[2] * 7, S7$estimates[2] * 7, S8$estimates[2] * 7, S8$estimates[3] * 7),
#     lower = c(S9$L[2] * 7, S1$L[2] * 7, S2$L[2] * 7, S3$L[2] * 7, S4$L[2] * 7, S5$L[2] * 7, S6$L[2] * 7, S7$L[2] * 7, S8$L[2] * 7, S8$L[3] * 7),
#     upper = c(S9$U[2] * 7, S1$U[2] * 7, S2$U[2] * 7, S3$U[2] * 7, S4$U[2] * 7, S5$U[2] * 7, S6$U[2] * 7, S7$U[2] * 7, S8$U[2] * 7, S8$U[3] * 7)
# ) |>
#     mutate(
#         cat = factor(cat, levels = c("Whole population", "Waned third dose", "Fourth dose", "Confirmed naive", "2+ years", "1-2 years", "6-12 months", "0-6 months", "COVID symptoms", "Non-COVID symptoms\nor asymptomatic"))
#     )

# save(sojourn_times, file = here("Data/siren_sojourn_times.RData"))

# Table of AIC and Log Likelihood from SIREN models
# load(here("Data/SIREN/siren_model_selection.RData"))
# load(here("Data/SIREN/siren_model_selection_sym.RData"))

# model_selection <- tibble(
#     model = c("mvt_int", "mvtr_int", "mvtra_int", "mvtrah_int", "mvtrahg_int", "mvtraho_int", "mvtrahm_int", "mvtrahs_int", "mvtrahe_int", "mvtrahp_int", "mvtrahgo_int", "mvtrahgm_int", "mvtrahgs_int", "mvtrahge_int", "mvtrahgp_int", "mvtrahgom_int", "mvtrahgos_int", "mvtrahgoe_int", "mvtrahgop_int"),
#     loglik = c(msm:::logLik.msm(mvt_int), msm:::logLik.msm(mvtr_int), msm:::logLik.msm(mvtra_int), msm:::logLik.msm(mvtrah_int), msm:::logLik.msm(mvtrahg_int), msm:::logLik.msm(mvtraho_int), msm:::logLik.msm(mvtrahm_int), msm:::logLik.msm(mvtrahs_int), msm:::logLik.msm(mvtrahe_int), msm:::logLik.msm(mvtrahp_int), msm:::logLik.msm(mvtrahgo_int), msm:::logLik.msm(mvtrahgm_int), msm:::logLik.msm(mvtrahgs_int), msm:::logLik.msm(mvtrahge_int), msm:::logLik.msm(mvtrahgp_int), msm:::logLik.msm(mvtrahgom_int), msm:::logLik.msm(mvtrahgos_int), msm:::logLik.msm(mvtrahgoe_int), msm:::logLik.msm(mvtrahgop_int)),
#     aic = c(AIC(mvt_int), AIC(mvtr_int), AIC(mvtra_int), AIC(mvtrah_int), AIC(mvtrahg_int), AIC(mvtraho_int), AIC(mvtrahm_int), AIC(mvtrahs_int), AIC(mvtrahe_int), AIC(mvtrahp_int), AIC(mvtrahgo_int), AIC(mvtrahgm_int), AIC(mvtrahgs_int), AIC(mvtrahge_int), AIC(mvtrahgp_int), AIC(mvtrahgom_int), AIC(mvtrahgos_int), AIC(mvtrahgoe_int), AIC(mvtrahgop_int))
# )

# sym_selection <- tibble(
#     model = c("mvt_sym", "mvtr_sym", "mvtra_sym", "mvtrah_sym", "mvtrahg_sym", "mvtraho_sym", "mvtrahm_sym", "mvtrahs_sym", "mvtrahe_sym", "mvtrahp_sym", "mvtrahgo_sym", "mvtrahgom_sym"),
#     loglik = c(msm:::logLik.msm(mvt_sym), msm:::logLik.msm(mvtr_sym), msm:::logLik.msm(mvtra_sym), msm:::logLik.msm(mvtrah_sym), msm:::logLik.msm(mvtrahg_sym), msm:::logLik.msm(mvtraho_sym), msm:::logLik.msm(mvtrahm_sym), msm:::logLik.msm(mvtrahs_sym), msm:::logLik.msm(mvtrahe_sym), msm:::logLik.msm(mvtrahp_sym), msm:::logLik.msm(mvtrahgo_sym), msm:::logLik.msm(mvtrahgom_sym)),
#     aic = c(AIC(mvt_sym), AIC(mvtr_sym), AIC(mvtra_sym), AIC(mvtrah_sym), AIC(mvtrahg_sym), AIC(mvtraho_sym), AIC(mvtrahm_sym), AIC(mvtrahs_sym), AIC(mvtrahe_sym), AIC(mvtrahp_sym), AIC(mvtrahgo_sym), AIC(mvtrahgom_sym))
# )

# save(model_selection, sym_selection, file = here("Data/siren_cov_selection.RData"))
