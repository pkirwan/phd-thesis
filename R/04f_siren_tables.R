# Figures and tables for Chapter 4

# Load libraries
here::i_am("R/04f_siren_tables.R")
library(here)
library(tidyverse)
library(kableExtra)
library(gtsummary)
library(epitools)
library(msm)

load(here("Data/SIREN/siren_processed_interim4_vax_final.RData"))
load(here("Data/siren_cohort.RData"))
load(here("data/SIREN/siren_cox_interim4_vax_final_table.RData"))
load(here("Data/siren_hazards.RData"))
load(here("Data/siren_sojourn_times.RData"))

load(here("Data/siren_expected_pos.RData"))
load(here("Data/siren_expected_pos_ft.RData"))
load(here("Data/siren_ppass.RData"))

# demographics table
siren_demographics <- full_cohort |>
    mutate(
        vaccine = case_when(
            is.na(vaccine_date1) ~ "Unvaccinated",
            is.na(vaccine_date3) | (vaccine_date3 > study_end_date) ~ "First and second dose",
            is.na(vaccine_date4) | (vaccine_date4 > study_end_date) ~ "Third dose",
            TRUE ~ "Fourth dose"
        ),
        vaccine = factor(vaccine, levels = c("Unvaccinated", "First and second dose", "Third dose", "Fourth dose")),
        # timeinstudy = study_end_date - date_enrolled,
        cohort = case_when(
            study_end_date - date_enrolled <= 370 ~ "Initial cohort",
            study_end_date - date_enrolled <= 733 ~ "First extension",
            TRUE ~ "Second extension"
        ),
        cohort = factor(cohort, levels = c("Initial cohort", "First extension", "Second extension"))
    ) |>
    select(gender, agegr, ethnicity, medical_group, staff_type, occupation_setting, patient_contact, imd, region, household, vaccine, cohort) |>
    tbl_summary(
        type = everything() ~ "categorical",
        percent = "col",
        by = cohort,
        label = c(
            gender ~ "Gender",
            agegr ~ "Age group",
            ethnicity ~ "Ethnicity",
            medical_group ~ "Medical condition",
            staff_type ~ "Staff type",
            occupation_setting ~ "Occupation setting",
            patient_contact ~ "Patient contact",
            imd ~ "Index of multiple deprivation",
            region ~ "Region",
            household ~ "Household structure",
            vaccine ~ "Vaccination status at study exit"
        ),
        digits = list(everything() ~ c(0, 1))
    ) |>
    # add_overall() |>
    modify_footnote(
        update = everything() ~ NA
    ) |>
    modify_header(
        all_stat_cols() ~ "**{level}**  \n N = {format(n, big.mark = ',')}"
    ) |>
    as_kable_extra(
        format = "latex", booktabs = T, linesep = "", longtable = T,
        caption = "Demographic characteristics of SIREN study participants, by recruitment status.",
        label = "siren_demographics"
    ) |>
    column_spec(1, width = "6cm") |>
    kable_styling(position = "center") |>
    kable_styling(latex_options = c("hold_position"))

# additional formatting for linebreaks
siren_demographics <- sub("Administrative/Executive \\(office based\\)", "\\\\makecell[l]\\{Administrative/Executive (office\\\\\\\\based)\\}", siren_demographics)
siren_demographics <- sub("Physiotherapist/Occupational Therapist/SALT", "\\\\makecell[l]\\{Physiotherapist/Occupational\\\\\\\\Therapist/SALT\\}", siren_demographics)
siren_demographics <- sub("Student \\(Medical/Nursing/Midwifery/Other\\)", "\\\\makecell[l]\\{Student (Medical/\\\\\\\\Nursing/Midwifery/Other)\\}", siren_demographics)
siren_demographics <- sub("Ambulance/Emergency department/Inpatient wards", "\\\\makecell[l]\\{Ambulance/Emergency\\\\\\\\department/Inpatient wards\\}", siren_demographics)
siren_demographics <- sub("Lives with others \\(including children\\)", "\\\\makecell[l]\\{Lives with others\\\\\\\\(including children)\\}", siren_demographics)

save_kable(siren_demographics, here("04_SIREN/Tables/siren_demographics.tex"))

# fourth dose uptake table
siren_vaccine_uptake <- full_cohort |>
    filter(study_id %in% siren_df_interim4$study_id) |>
    mutate(
        vaccine = if_else(is.na(vaccine_date4), "Waned third dose", "Fourth dose"),
        vaccine = factor(vaccine, levels = c("Waned third dose", "Fourth dose"))
    ) |>
    select(gender, agegr, ethnicity, medical_group, staff_type, occupation_setting, patient_contact, imd, region, household, vaccine) |>
    tbl_summary(
        type = everything() ~ "categorical",
        percent = "col",
        by = vaccine,
        label = c(
            gender ~ "Gender",
            agegr ~ "Age group",
            ethnicity ~ "Ethnicity",
            medical_group ~ "Medical condition",
            staff_type ~ "Staff type",
            occupation_setting ~ "Occupation setting",
            patient_contact ~ "Patient contact",
            imd ~ "Index of multiple deprivation",
            region ~ "Region",
            household ~ "Household structure"
        ),
        digits = list(everything() ~ c(0, 1))
    ) |>
    add_overall() |>
    modify_footnote(
        update = everything() ~ NA
    ) |>
    modify_header(
        all_stat_cols() ~ "**{level}**  \n N = {format(n, big.mark = ',')}"
    ) |>
    modify_spanning_header(c("stat_1", "stat_2") ~ "**Fourth dose vaccine coverage**") |>
    as_kable_extra(
        format = "latex", booktabs = T, linesep = "", longtable = T,
        caption = "Demographic characteristics of SIREN study participants enrolled between 12 September 2022 and 31 March 2023, by fourth dose vaccine coverage.",
        label = "siren_vaccine_uptake"
    ) |>
    column_spec(1, width = "6cm") |>
    kable_styling(position = "center") |>
    kable_styling(latex_options = c("hold_position"))

# additional formatting for linebreaks
siren_vaccine_uptake <- sub("Administrative/Executive \\(office based\\)", "\\\\makecell[l]\\{Administrative/Executive (office\\\\\\\\based)\\}", siren_vaccine_uptake)
siren_vaccine_uptake <- sub("Physiotherapist/Occupational Therapist/SALT", "\\\\makecell[l]\\{Physiotherapist/Occupational\\\\\\\\Therapist/SALT\\}", siren_vaccine_uptake)
siren_vaccine_uptake <- sub("Student \\(Medical/Nursing/Midwifery/Other\\)", "\\\\makecell[l]\\{Student (Medical/\\\\\\\\Nursing/Midwifery/Other)\\}", siren_vaccine_uptake)
siren_vaccine_uptake <- sub("Ambulance/Emergency department/Inpatient wards", "\\\\makecell[l]\\{Ambulance/Emergency\\\\\\\\department/Inpatient wards\\}", siren_vaccine_uptake)
siren_vaccine_uptake <- sub("Lives with others \\(including children\\)", "\\\\makecell[l]\\{Lives with others\\\\\\\\(including children)\\}", siren_vaccine_uptake)

save_kable(siren_vaccine_uptake, here("04_SIREN/Tables/siren_vaccine_uptake.tex"))

# crude exposure and results table
crude_inc <- function(data, variable) {
    data |>
        group_by({{ variable }}) |>
        summarise(
            participants = n_distinct(substr(study_id, 1, 8)),
            events = sum(event),
            exposure = round(sum(tstop - tstart) * 7, 0),
            incidence = events / exposure * 10000,
            lower = pois.exact(x = events, pt = exposure, conf.level = 0.95)$lower * 10000,
            upper = pois.exact(x = events, pt = exposure, conf.level = 0.95)$upper * 10000,
            incidence = paste0(round(incidence, 2), " (", round(lower, 2), ", ", round(upper, 2), ")")
        ) |>
        select(-lower, -upper) |>
        rename(group = {{ variable }}) |>
        left_join(
            siren_df_interim4 |>
                mutate(
                    eligible = "Whole population",
                    covid_symptoms = case_when(
                        covid_symptoms %in% c("Asymptomatic", "Other symptoms") ~ "asymptomatic",
                        covid_symptoms == "COVID symptoms" ~ "symptomatic",
                        TRUE ~ covid_symptoms
                    ),
                    covid_symptoms = fct_relevel(covid_symptoms, "symptomatic")
                ) |>
                filter(specimen_date == episode_start, !is.na(covid_symptoms)) |>
                count({{ variable }}, covid_symptoms) |>
                pivot_wider(names_from = covid_symptoms, values_from = n) |>
                rename(group = {{ variable }}),
            by = "group"
        )
}

hazard_format <- function(data, sym = 0) {
    df <- data |>
        mutate(
            name = gsub("Fourth dose\\n", "", name),
            name = gsub("\\n", " ", name),
            est = round(est * 100, 2),
            lower = round(lower * 100, 2),
            upper = round(upper * 100, 2),
            est = paste0(est, "% (", upper, ", ", lower, ")"),
            est = if_else(est == "0% (NA, NA)", "Baseline", est)
        )
    if (sym) {
        df <- df |>
            select(name, value_id, est) |>
            pivot_wider(names_from = value_id, values_from = est) |>
            rename(symptomatic_ve = "State 1 - State 2", asymptomatic_ve = "State 1 - State 3") |>
            select(name, symptomatic_ve, asymptomatic_ve)
            #arrange(name)
    } else {
        df <- df |>
            select(name, est)
            #arrange(name)
    }
    return(df)
}

siren_split <- siren_split |>
    mutate(
        eligible = "Whole population",
        months_since_pos = factor(months_since_pos, levels = c("Confirmed naive", "2+ years", "1-2 years", "6-12 months", "0-6 months")),
        vaccine = factor(vaccine, labels = c("Waned third dose", "0-2 months", "2-4 months", "4-6 months"))
    )

siren_df_interim4 <- siren_df_interim4 |>
    mutate(
        eligible = "Whole population",
        months_since_pos = factor(months_since_pos, levels = c("Confirmed naive", "2+ years", "1-2 years", "6-12 months", "0-6 months")),
        vaccine = factor(vaccine, labels = c("Waned third dose", "0-2 months", "2-4 months", "4-6 months"))
    )

pcr_positivity <- left_join(
    bind_rows(
        crude_inc(siren_split, eligible),
        crude_inc(siren_split, vaccine_short),
        crude_inc(siren_split, vaccine) |> filter(group != "Waned third dose"),
        crude_inc(siren_split |> filter(months_since_pos != "No evidence of infection"), months_since_pos)
    ),
    bind_rows(
        c(name = "Whole population", est = "N/A"),
        hazard_format(hazards_mllvrahgo),
        hazard_format(hazards_mmsvrahgo_noev)
    ) |> select(group = name, est),
    by = "group"
) |> left_join(
    bind_rows(
        c(name = "Whole population", symptomatic_ve = "N/A", asymptomatic_ve = "N/A"),
        hazard_format(hazards_msvrahgo_sym, sym = 1),
        hazard_format(hazards_mllvrahgo_sym, sym = 1) |> filter(name != "Waned third dose"),
        hazard_format(hazards_mmsvrahgo_sym, sym = 1)
    ) |> select(group = name, symptomatic_ve, asymptomatic_ve),
    by = "group"
) |>
    select(group, participants, events, exposure, incidence, est, symptomatic, symptomatic_ve, asymptomatic, asymptomatic_ve) |>
    kable(
        format = "latex", booktabs = T, linesep = "",
        caption = "Crude PCR positivity rates per 10,000 person-days and estimated vaccine effectiveness and protection from prior infection by vaccination status, time since previous infection and reported COVID symptoms.",
        label = "pcr_positivity",
        col.names = c(
            " ", "Number of participants", "Positive PCR tests",
            "Exposure (person-days at risk)",
            "Crude PCR positivity rate per 10,000 person-days (95% CI)",
            "Protection relative to baseline (95% CI)",
            "Positive PCR tests", "Protection relative to baseline (95% CI)",
            "Positive PCR tests", "Protection relative to baseline (95% CI)"
        )
    ) |>
    # column_spec(1, width = "3.3cm") |>
    column_spec(2, width = "1.5cm") |>
    column_spec(c(3, 7, 9), width = "1cm") |>
    column_spec(c(4), width = "1.7cm") |>
    column_spec(5, width = "2.3cm") |>
    column_spec(c(6, 8, 10), width = "1.9cm") |>
    row_spec(0, align = "l") |>
    add_header_above(c(" " = 6, "Symptomatic" = 2, "Asymptomatic" = 2)) |>
    pack_rows("Vaccination status", 2, 3, bold = FALSE) |>
    pack_rows("Time since fourth dose", 4, 6, bold = FALSE) |>
    pack_rows("Time since previous infection", 7, 11, bold = FALSE) |>
    kable_styling(position = "center") |>
    kable_styling(latex_options = c("scale_down", "hold_position")) |>
    footnote(
        "Participants joined the analysis at the date of their first PCR test after 12th September 2022. 171 participants had received their fourth dose booster on or before the date of their first PCR test and contributed no follow-up time in the waned third dose vaccine status.",
        threeparttable = T
    )

# additional formatting for linebreaks
pcr_positivity <- sub("Waned third dose", "\\\\makecell[l]\\{Waned third\\\\\\\\dose\\}", pcr_positivity)
pcr_positivity <- sub("Confirmed naive", "\\\\makecell[l]\\{Confirmed\\\\\\\\naive\\}", pcr_positivity)

save_kable(pcr_positivity, here("04_SIREN/Tables/pcr_positivity.tex"))

empirical_time <- function(variable) {
    siren_df_interim4 |>
        filter(episode_start >= as_date("2022-09-12") & !is.na(infection_date_1)) |>
        arrange(study_id, specimen_date) |>
        distinct(study_id, episode_start, episode_end, .keep_all = TRUE) |>
        mutate(
            eligible = "Whole population",
            time_pos = episode_end - episode_start,
            covid_symptoms = if_else(covid_symptoms %in% c("Asymptomatic", "Other symptoms"), "Non-COVID symptoms or asymptomatic", covid_symptoms),
            covid_symptoms = fct_relevel(covid_symptoms, "COVID symptoms"),
            months_since_pos = fct_relevel(months_since_pos, "Confirmed naive")
        ) |>
        group_by({{ variable }}) |>
        summarise(
            median = median(time_pos, na.rm = TRUE),
            lower = quantile(time_pos, 0.25, na.rm = TRUE),
            upper = quantile(time_pos, 0.75, na.rm = TRUE),
            est = paste0(median, " days [", lower, ", ", upper, "]")
        ) |>
        ungroup() |>
        select(group = {{ variable }}, est) |>
        filter(!is.na(group)) |>
        arrange(group)
}

sojourn_time <- bind_cols(
    bind_rows(
        empirical_time(eligible),
        empirical_time(vaccine_short),
        empirical_time(months_since_pos) |> filter(group != "No evidence of infection"),
        empirical_time(covid_symptoms)
    ),
    sojourn_times |>
        select(cat, sojourn, lower, upper) |>
        mutate(
            sojourn = paste0(round(sojourn, 2), " days (", round(lower, 2), ", ", round(upper, 2), ")")
        ) |>
        select(-lower, -upper) |> arrange(cat) |> select(sojourn)
) |>
    select(group, sojourn, est) |>
    kable(
        format = "latex", booktabs = T, linesep = "",
        caption = "Estimated mean sojourn time in PCR positive state, averaged across the study population, and empirical median duration of PCR positivity by vaccination status and time since previous infection.",
        label = "sojourn_time",
        col.names = c(
            " ",
            "Estimated mean sojourn time in PCR positive state (95% CI)",
            "Empirical median duration of PCR positivity (initial PCR positive to subsequent PCR negative) [interquartile range]"
        )
    ) |>
    column_spec(2:3, width = "3.8cm") |>
    pack_rows("Vaccination status", 2, 3, bold = FALSE) |>
    pack_rows("Time since previous infection", 4, 8, bold = FALSE) |>
    pack_rows("COVID symptoms", 9, 10, bold = FALSE) |>
    kable_styling(position = "center") |>
    kable_styling(latex_options = "hold_position")

# additional formatting for linebreaks
sojourn_time <- sub("Non-COVID symptoms or asymptomatic", "\\\\makecell[l]\\{Non-COVID symptoms\\\\\\\\or asymptomatic\\}", sojourn_time)

save_kable(sojourn_time, here("04_SIREN/Tables/sojourn_time.tex"))

msm_table <- tribble(
    ~msm, ~m1, ~m2, ~m3, ~m4, ~m5, ~m6, ~m7,
    "N (individuals in model)", "9560", "9560", "7549", "7549", "9560", "9560", "7549",
    "Symptom data (Y/N)", "N", "N", "N", "N", "Y", "Y", "Y",
    "Month", "p", "p", "p", "p", "p", "p", "p",
    "Binary vaccination status", "$\\checkmark$", "", "$\\checkmark$", "", "$\\checkmark$", "", "$\\checkmark$",
    "Vaccination status", "", "$\\checkmark$", "", "", "", "$\\checkmark$", "",
    "Time since infection", "", "", "$\\checkmark$", "$\\checkmark$", "", "", "$\\checkmark$",
    "Vaccination:time since\ninfection (interaction term)", "", "", "$\\checkmark$", "$\\checkmark$", "", "", "$\\checkmark$",
    "Region", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
    "Age group", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
    "Household status", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
    "Gender", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
    "Occupation/setting", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"
) |>
    mutate(msm = linebreak(msm)) |>
    kable(
        format = "latex", booktabs = T, linesep = "", escape = F,
        caption = "List of multi-state models used to generate estimates, with number of individuals, inclusion of symptom information, and list of covariates. $\\checkmark$: covariate included as main effect in regression, p: covariate included with piece-wise constant hazards.",
        label = "msm_table",
        col.names = c(" ", "M1", "M2", "M3", "M4", "M5", "M6", "M7")
    ) |>
    add_header_above(c(" " = 1, "Multi-state model" = 7)) |>
    pack_rows("Covariates", 3, 12) |>
    kable_styling(position = "center") |>
    kable_styling(latex_options = c("striped", "hold_position"))

save_kable(msm_table, here("04_SIREN/Tables/msm_table.tex"))

cox_table <- tribble(
    ~cox, ~c1, ~c2, ~c3, ~c4,
    "N (individuals in model)", "9560", "9560", "7549", "7549",
    "Symptom data (Y/N)", "N", "N", "N", "N",
    "Month", "", "", "", "",
    "Binary vaccination status", "$\\checkmark$", "", "$\\checkmark$", "",
    "Vaccination status", "", "$\\checkmark$", "", "",
    "Time since infection", "", "", "$\\checkmark$", "$\\checkmark$",
    "Vaccination:time since\ninfection (interaction term)", "", "", "$\\checkmark$", "$\\checkmark$",
    "Region", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
    "Age group", "s", "s", "s", "s",
    "Household status", "s", "s", "s", "s",
    "Gender", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
    "Occupation/setting", "s", "s", "s", "s"
) |>
    mutate(cox = linebreak(cox)) |>
    kable(
        format = "latex", booktabs = T, linesep = "", escape = F,
        caption = "List of Cox proportional hazards models used to generate estimates, with number of individuals, inclusion of symptom information, and list of covariates. $\\checkmark$: covariate included as main effect in regression, s: covariate included with stratification.",
        label = "cox_table",
        col.names = c(" ", "C1", "C2", "C3", "C4")
    ) |>
    add_header_above(c(" " = 1, "Cox proportional hazards model" = 4)) |>
    pack_rows("Covariates", 3, 12) |>
    kable_styling(position = "center") |>
    kable_styling(latex_options = c("striped", "hold_position"))

save_kable(cox_table, here("04_SIREN/Tables/cox_table.tex"))

fortnightly_schedule <- fortnightly_testing |>
    tbl_cross(
        row = variant_period,
        col = on_schedule,
        percent = "row",
        margin = "row",
        label = c(
            variant_period ~ "Variant circulating period",
            on_schedule ~ "Fortnightly testing schedule"
        ),
        statistic = "{p}%",
        digits = 1,
        missing = "no"
    ) |>
    as_kable_extra(
        format = "latex", booktabs = T, linesep = "",
        caption = "Proportion of enrolled SIREN participants undergoing PCR testing on a fortnightly schedule, by variant circulating period.",
        label = "fortnightly_schedule", align = c("l", "c", "c")
    ) |>
    kable_styling(position = "center") |>
    kable_styling(latex_options = "hold_position")

save_kable(fortnightly_schedule, here("04_SIREN/Tables/fortnightly_schedule.tex"))

# Table of AIC and Log Likelihood from SIREN models
load(here("Data/siren_cov_selection.RData"))

# $\checkmark$ can be used in pdflatex to get a tick
msm_cov_selection <- tribble(
    ~monthyear, ~vaccine_short, ~months_since_pos, ~region, ~agegr, ~household, ~gender, ~occupation_setting, ~medical_group, ~staff_type,
    ~ethnicity, ~patient_contact,
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "", "", "", "", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "", "", "", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "", "", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "$\\checkmark$", "", "", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "$\\checkmark$", "", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "$\\checkmark$", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "", "$\\checkmark$", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "", "", "$\\checkmark$",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "$\\checkmark$", "", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "$\\checkmark$", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "$\\checkmark$", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "", "$\\checkmark$",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "$\\checkmark$", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "$\\checkmark$", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "$\\checkmark$"
) |>
    bind_cols(
        model_selection |> select(-model)
    ) |>
    mutate(
        loglik = round(loglik, 2),
        aic = round(aic, 2)
    ) |>
    arrange(aic) |>
    kable(
        format = "latex", booktabs = T, linesep = "", escape = F,
        caption = "Covariates, log-likelihood and AIC values used for two-state model covariate selection, sorted by AIC.",
        label = "msm_cov_selection",
        col.names = c(
            "Month (piecewise-constant)", "Vaccination", "Time since infection", "Region", "Age group",
            "Household status", "Gender", "Occupation/setting", "Clinical risk group",
            "Staff type", "Ethnicity", "Patient contact", "Log likelihood", "AIC"
        )
    ) |>
    kable_styling(position = "center", latex_options = c("striped", "hold_position")) |>
    row_spec(0, angle = 90)

# lrtest.msm(mvtrahgo_int, mvtrahgop_int)

# additional formatting for linebreaks
msm_cov_selection <- sub("\\\\rotatebox\\{90\\}\\{Log likelihood\\}", "\\\\makecell[l]\\{Log-\\\\\\\\likelihood\\}", msm_cov_selection)
msm_cov_selection <- sub("\\\\rotatebox\\{90\\}\\{AIC\\}", "AIC", msm_cov_selection)

save_kable(msm_cov_selection, here("04_SIREN/Tables/msm_cov_selection.tex"))

msm_sym_selection <- tribble(
    ~monthyear, ~vaccine_short, ~months_since_pos, ~region, ~agegr, ~household, ~gender, ~occupation_setting, ~medical_group, ~staff_type,
    ~ethnicity, ~patient_contact,
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "", "", "", "", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "", "", "", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "", "", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "$\\checkmark$", "", "", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "$\\checkmark$", "", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "$\\checkmark$", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "", "$\\checkmark$", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "", "", "$\\checkmark$",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", "", "",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", ""
) |>
    bind_cols(
        sym_selection |> select(-model)
    ) |>
    mutate(
        loglik = round(loglik, 2),
        aic = round(aic, 2)
    ) |>
    arrange(aic) |>
    kable(
        format = "latex", booktabs = T, linesep = "", escape = F,
        caption = "Covariates, log-likelihood and AIC values used for symptom-status model covariate selection, sorted by AIC.",
        label = "msm_sym_selection",
        col.names = c(
            "Month (piecewise-constant)", "Vaccination", "Time since infection", "Region", "Age group",
            "Household status", "Gender", "Occupation/setting", "Clinical risk group",
            "Staff type", "Ethnicity", "Patient contact", "Log likelihood", "AIC"
        )
    ) |>
    kable_styling(position = "center", latex_options = c("striped", "hold_position")) |>
    row_spec(0, angle = 90)

# additional formatting for linebreaks
msm_sym_selection <- sub("\\\\rotatebox\\{90\\}\\{Log likelihood\\}", "\\\\makecell[l]\\{Log-\\\\\\\\likelihood\\}", msm_sym_selection)
msm_sym_selection <- sub("\\\\rotatebox\\{90\\}\\{AIC\\}", "AIC", msm_sym_selection)

save_kable(msm_sym_selection, here("04_SIREN/Tables/msm_sym_selection.tex"))

inf_averted_vec <- outer(waned_ppass, boosted_ppass, "-") |>
    quantile(c(0.025, 0.5, 0.975)) |>
    round(0)
inf_averted <- paste0(inf_averted_vec[2], " (", inf_averted_vec[1], ", ", inf_averted_vec[3], ")")

inf_averted_obs_vec <- outer(unlist(waned_sim), unlist(boosted_sim), "-") |>
    quantile(c(0.025, 0.5, 0.975)) |>
    round(0)
inf_averted_obs <- paste0(inf_averted_obs_vec[2], " (", inf_averted_obs_vec[1], ", ", inf_averted_obs_vec[3], ")")

inf_averted_biw_vec <- outer(unlist(waned_sim_ft), unlist(boosted_sim_ft), "-") |>
    quantile(c(0.025, 0.5, 0.975)) |>
    round(0)
inf_averted_biw <- paste0(inf_averted_biw_vec[2], " (", inf_averted_biw_vec[1], ", ", inf_averted_biw_vec[3], ")")

expected_infections <- bind_rows(
    bind_quantiles(waned_ppass, boosted_ppass, method = "Total infections"),
    bind_quantiles(waned_sim_ft, boosted_sim_ft, method = "Bi-weekly sampling scheme"),
    bind_quantiles(waned_sim, boosted_sim, method = "Observed sampling scheme")
) |>
    mutate(
        vaccine = factor(vaccine, levels = c("No vaccination", "Complete vaccination")),
        method = factor(method, levels = c("Observed sampling scheme", "Bi-weekly sampling scheme", "Total infections"))
    ) |>
    mutate(
        mean = round(mean, 0),
        lower = round(lower, 0),
        upper = round(upper, 0),
        estimate = paste0(mean, " (", lower, ", ", upper, ")"),
        method = factor(method, levels = c("Observed sampling scheme", "Bi-weekly sampling scheme", "Total infections"))
    ) |>
    select(method, estimate, vaccine) |>
    pivot_wider(names_from = vaccine, values_from = estimate) |>
    bind_cols(
        tibble(
            `Infections averted` = c(inf_averted, inf_averted_biw, inf_averted_obs)
        )
    ) |>
    kable(
        format = "latex", booktabs = TRUE, linesep = "",
        caption = "Estimated total number of infections and estimated number of detected infections (95\\% CI) under observed and bi-weekly sampling scheme, by counterfactual scenario.",
        label = "expected_infections",
        col.names = c(" ", "No vaccination", "Complete vaccination", "Infections averted")
    ) |>
    kable_styling(position = "center", latex_options = c("striped", "scale_down", "hold_position")) |>
    add_header_above(c(" " = 1, "Scenario" = 3)) |>
    pack_rows("Sampling scheme", 1, 3)

save_kable(expected_infections, here("04_SIREN/Tables/expected_infections.tex"))

load(here("Data/SIREN/siren_causal_model.RData"))

causal_table <- tribble(
    ~monthyear, ~vaccine_short, ~months_since_pos, ~region, ~agegr, ~gender, ~occupation_setting,
    ~medical_group, ~imd, ~household, ~patient_contact, ~loglik, ~aic,
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
    "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", msm:::logLik.msm(mlvtrahgomi_int),
    AIC(mlvtrahgomi_int)
) |>
    mutate(
        loglik = round(loglik, 2),
        aic = round(aic, 2)
    ) |>
    arrange(aic) |>
    kable(
        format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
        caption = "Covariates, log-likelihood and AIC values used for causal model.",
        label = "causal_table",
        col.names = c(
            "Month (piecewise-constant)", "Vaccination", "Time since infection", "Region", "Age group",
            "Gender", "Occupation/setting", "Clinical risk group", "IMD",
            "Household status", "Patient contact", "Log likelihood", "AIC"
        )
    ) |>
    kable_styling(position = "center", latex_options = c("hold_position")) |>
    row_spec(0, angle = 90)

causal_table <- sub("\\\\rotatebox\\{90\\}\\{Log likelihood\\}", "\\\\makecell[l]\\{Log-\\\\\\\\likelihood\\}", causal_table)
causal_table <- sub("\\\\rotatebox\\{90\\}\\{AIC\\}", "AIC", causal_table)

save_kable(causal_table, here("04_SIREN/Tables/causal_table.tex"))

load(here("Data/siren_diagnostics.RData"))

siren_deviance <- deviance$`Deviance*sign(O-E)` |>
    select(Time, Interval, Cov, `1-2`, `2-1`) |>
    mutate(`1-2` = round(`1-2`, 2), `2-1` = round(`2-1`, 2)) |>
    mutate(Cov = if_else(Cov == "[-1.12,-0.95)", "A", "B")) |>
    kable(
        format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
        caption = "Deviance statistics for fitted multi-state model M1, by time from study start, time interval, and covariate strata.",
        label = "siren_deviance",
        col.names = c(
            "Time from study start (weeks)", "Time interval", "Covariate category",
            "Susceptible to infected transition", "Infected to susceptible transition"
        )
    ) |>
    kable_styling(position = "center", latex_options = c("striped", "hold_position"))

siren_deviance <- sub("Time from study start \\(weeks\\)", "\\\\makecell[l]\\{Time from study\\\\\\\\start (weeks)\\}", siren_deviance)
siren_deviance <- sub("Time interval", "\\\\makecell[l]\\{Time\\\\\\\\interval\\}", siren_deviance)
siren_deviance <- sub("Covariate category", "\\\\makecell[l]\\{Covariate\\\\\\\\category\\}", siren_deviance)
siren_deviance <- sub("Susceptible to infected transition", "\\\\makecell[l]\\{Susceptible to\\\\\\\\infected transition\\}", siren_deviance)
siren_deviance <- sub("Infected to susceptible transition", "\\\\makecell[l]\\{Infected to\\\\\\\\susceptible transition\\}", siren_deviance)

save_kable(siren_deviance, here("04_SIREN/Tables/siren_deviance.tex"))
