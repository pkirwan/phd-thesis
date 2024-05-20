# Figures and tables for Chapter 3

# Load libraries
here::i_am("R/03d_hosp_tables.R")
library(here)
library(tidyverse)
library(kableExtra)
library(gtsummary)
library(flexsurv)

# Load data

load(file = here("Data/Hosp/chess_mixcure_dists.RData"))

# not sure much value in showing these figures

# p1 <- ajfit_month(hosp_sex_0, labeldays = 90, maxdays = 100, monthlist = c("Apr\n2020", "Oct\n2020")) + facet_grid(sex ~ monthofadmission)
# p2 <- ajfit_month(hosp_sex_p, labeldays = 90, maxdays = 100, monthlist = c("Apr\n2020", "Oct\n2020")) + facet_grid(sex ~ monthofadmission)
# p3 <- ajfit_month(hosp_sex_de, labeldays = 90, maxdays = 100, monthlist = c("Apr\n2020", "Oct\n2020")) + facet_grid(sex ~ monthofadmission)
# p4 <- ajfit_month(hosp_sex_di, labeldays = 90, maxdays = 100, monthlist = c("Apr\n2020", "Oct\n2020")) + facet_grid(sex ~ monthofadmission)
# p5 <- ajfit_month(hosp_sex_pde, labeldays = 90, maxdays = 100, monthlist = c("Apr\n2020", "Oct\n2020")) + facet_grid(sex ~ monthofadmission)
# p6 <- ajfit_month(hosp_sex_pdi, labeldays = 90, maxdays = 100, monthlist = c("Apr\n2020", "Oct\n2020")) + facet_grid(sex ~ monthofadmission)

# (p1 + p2) / (p3 + p4) / (p5 + p6) + plot_layout(guides = "collect", axis_titles = "collect") & theme(legend.position = "bottom")

# distribution table

gof_dist <- bind_rows(
    tribble(
        ~submodel, ~icu, ~death, ~discharge, ~loglik, ~aic,
        "Hospital", "Weibull", "gengamma", "gengamma", hospi_we$loglik, hospi_we$AIC,
        "Hospital", "gamma", "gengamma", "gengamma", hospi_ga$loglik, hospi_ga$AIC,
        "Hospital", "log-normal", "gengamma", "gengamma", hospi_ln$loglik, hospi_ln$AIC,
        "Hospital", "exponential", "gengamma", "gengamma", hospi_ex$loglik, hospi_ex$AIC,
        "Hospital", "log-normal", "Weibull", "gengamma", hospde_we$loglik, hospde_we$AIC,
        "Hospital", "log-normal", "gamma", "gengamma", hospde_ga$loglik, hospde_ga$AIC,
        "Hospital", "log-normal", "log-normal", "gengamma", hospde_ln$loglik, hospde_ln$AIC,
        "Hospital", "log-normal", "exponential", "gengamma", hospde_ex$loglik, hospde_ex$AIC,
        "Hospital", "log-normal", "gengamma", "Weibull", hospdi_we$loglik, hospdi_we$AIC,
        "Hospital", "log-normal", "gengamma", "gamma", hospdi_ga$loglik, hospdi_ga$AIC,
        "Hospital", "log-normal", "gengamma", "log-normal", hospdi_ln$loglik, hospdi_ln$AIC,
        "Hospital", "log-normal", "gengamma", "exponential", hospdi_ex$loglik, hospdi_ex$AIC
    ) |>
        arrange(aic),
    tribble(
        ~submodel, ~icu, ~death, ~discharge, ~loglik, ~aic,
        "ICU", "", "gengamma", "gengamma", icu_gg2$loglik, icu_gg2$AIC,
        "ICU", "", "gengamma", "Weibull", icudi_we$loglik, icudi_we$AIC,
        "ICU", "", "gengamma", "gamma", icudi_ga$loglik, icudi_ga$AIC,
        "ICU", "", "gengamma", "log-normal", icudi_ln$loglik, icudi_ln$AIC,
        "ICU", "", "gengamma", "exponential", icudi_ex$loglik, icudi_ex$AIC,
        "ICU", "", "Weibull", "gengamma", icude_we$loglik, icude_we$AIC,
        "ICU", "", "gamma", "gengamma", icude_ga$loglik, icude_ga$AIC,
        "ICU", "", "log-normal", "gengamma", icude_ln$loglik, icude_ln$AIC,
        "ICU", "", "exponential", "gengamma", icude_ex$loglik, icude_ex$AIC
    ) |>
        arrange(aic)
) |>
    mutate(
        loglik = round(loglik, 2),
        aic = round(aic, 2)
    ) |>
    kable(
        format = "latex", booktabs = T, linesep = "", escape = F,
        caption = "Log-likelihood and AIC values for parametric time to transition distributions, sorted by sub-model and AIC. Models with unidentifiable parameters not shown.",
        label = "gof_dist",
        col.names = c(
            "Sub-model", "ICU", "Death", "Discharge", "Log-likelihood", "AIC"
        )
    ) |>
    kable_styling(position = "center", latex_options = c("striped", "hold_position"))


save_kable(gof_dist, here("03_HospitalSeverity/Tables/gof_dist.tex"))

# Placement of covariates effects table
format_prob <- function(hosp_df) {
    # convert hosp_df to a string
    str <- hosp_df
    # split up the string
    mc <- strsplit(str, "_")[[1]]
    # get the model name
    model <- if_else(mc[1] == "hosp", "Hospital", "ICU")
    # get the covariates
    if (model == "Hospital") {
        icu <- if_else(mc[2] == "m", "$\\mu$", if_else(mc[2] == "s", "$\\sigma$", if_else(mc[2] == "ms", "$\\mu$, $\\sigma$", "")))
        death <- if_else(mc[3] == "m", "$\\mu$", if_else(mc[3] == "s", "$\\sigma$", if_else(mc[3] == "ms", "$\\mu$, $\\sigma$", "")))
        discharge <- if_else(mc[4] == "m", "$\\mu$", if_else(mc[4] == "s", "$\\sigma$", if_else(mc[4] == "ms", "$\\mu$, $\\sigma$", "")))
    } else {
        icu <- ""
        death <- if_else(mc[2] == "m", "$\\mu$", if_else(mc[2] == "s", "$\\sigma$", if_else(mc[2] == "ms", "$\\mu$, $\\sigma$", "")))
        discharge <- if_else(mc[3] == "m", "$\\mu$", if_else(mc[3] == "s", "$\\sigma$", if_else(mc[3] == "ms", "$\\mu$, $\\sigma$", "")))
    }
    # generate the table row
    return(tibble(
        submodel = model,
        p = "$\\checkmark$",
        icu, death, discharge,
        loglik = get(hosp_df)$loglik,
        aic = get(hosp_df)$AIC
    ))
}

load(file = here("Data/Hosp/chess_mixcure_anc.RData"))

anc_list <- c(
    grep("^hosp_1_", ls(), value = TRUE),
    grep("^hosp_m_", ls(), value = TRUE),
    grep("^hosp_s_", ls(), value = TRUE),
    grep("^icu_1", ls(), value = TRUE),
    grep("^icu_m", ls(), value = TRUE),
    grep("^icu_s_", ls(), value = TRUE)
)

gof_place <- anc_list |>
    map(format_prob) |>
    bind_rows() |>
    arrange(submodel, aic) |>
    mutate(
        loglik = round(loglik, 2),
        aic = round(aic, 2)
    ) |>
    kable(
        format = "latex", booktabs = T, linesep = "", escape = F,
        caption = "Log-likelihood and AIC values for log-normal and generalized gamma distributions, by location of covariate effects, sorted by sub-model and AIC. Models with unidentifiable parameters not shown.",
        label = "gof_place",
        col.names = c(
            "Sub-model", "$\\pi$", "ICU", "Death", "Discharge", "Log-likelihood", "AIC"
        )
    ) |>
    kable_styling(position = "center", latex_options = c("striped", "hold_position"))

save_kable(gof_place, here("03_HospitalSeverity/Tables/gof_place.tex"))

# interaction table

load(file = here("Data/Hosp/chess_mixcure_covs.RData"))

gof_int <- bind_rows(
    tribble(
        ~submodel, ~p, ~icu, ~death, ~discharge, ~loglik, ~aic,
        "Hospital", "+", "+", "+", "+", hosp_sex_0$loglik, hosp_sex_0$AIC,
        "Hospital", "$\\times$", "+", "+", "+", hosp_sex_p$loglik, hosp_sex_p$AIC,
        "Hospital", "+", "$\\times$", "+", "+", hosp_sex_i$loglik, hosp_sex_i$AIC,
        "Hospital", "+", "+", "$\\times$", "+", hosp_sex_de$loglik, hosp_sex_de$AIC,
        "Hospital", "+", "+", "+", "$\\times$", hosp_sex_di$loglik, hosp_sex_di$AIC,
        "Hospital", "$\\times$", "$\\times$", "+", "+", hosp_sex_pi$loglik, hosp_sex_pi$AIC,
        "Hospital", "$\\times$", "+", "$\\times$", "+", hosp_sex_pde$loglik, hosp_sex_pde$AIC,
        "Hospital", "$\\times$", "+", "+", "$\\times$", hosp_sex_pdi$loglik, hosp_sex_pdi$AIC
    ) |>
        arrange(aic),
    tribble(
        ~submodel, ~p, ~icu, ~death, ~discharge, ~loglik, ~aic,
        "ICU", "+", "+", "+", "+", icu_sex_0$loglik, icu_sex_0$AIC,
        "ICU", "$\\times$", "+", "+", "+", icu_sex_p$loglik, icu_sex_p$AIC,
        "ICU", "+", "+", "$\\times$", "+", icu_sex_de$loglik, icu_sex_de$AIC,
        "ICU", "+", "+", "+", "$\\times$", icu_sex_di$loglik, icu_sex_di$AIC,
        "ICU", "$\\times$", "+", "$\\times$", "+", icu_sex_pde$loglik, icu_sex_pde$AIC,
        "ICU", "$\\times$", "+", "+", "$\\times$", icu_sex_pdi$loglik, icu_sex_pdi$AIC
    ) |>
        arrange(aic)
) |>
    mutate(
        loglik = round(loglik, 2),
        aic = round(aic, 2)
    ) |>
    kable(
        format = "latex", booktabs = T, linesep = "", escape = F,
        caption = "Log-likelihood and AIC values for for log-normal and generalized gamma distributions, by inclusion of interaction of month and sex covariate, sorted by sub-model and AIC. Models with unidentifiable parameters not shown.",
        label = "gof_int",
        col.names = c(
            "Sub-model", "$\\pi$", "ICU", "Death", "Discharge", "Log-likelihood", "AIC"
        )
    ) |>
    kable_styling(position = "center", latex_options = c("striped", "hold_position"))

save_kable(gof_int, here("03_HospitalSeverity/Tables/gof_int.tex"))

load(file = here("Data/Hosp/chess_mixcure_covs_age.RData"))

gof_int_age <- bind_rows(
    tribble(
        ~submodel, ~p, ~icu, ~death, ~discharge, ~loglik, ~aic,
        "Hospital", "", "", "", "", hosp_agegroup_0$loglik, hosp_agegroup_0$AIC,
        "Hospital", "$\\checkmark$", "", "", "", hosp_agegroup_p$loglik, hosp_agegroup_p$AIC,
        "Hospital", "", "$\\mu$", "", "", hosp_agegroup_i$loglik, hosp_agegroup_i$AIC,
        "Hospital", "", "", "$\\sigma$", "", hosp_agegroup_de_sig$loglik, hosp_agegroup_de_sig$AIC,
        "Hospital", "", "", "$\\mu$", "", hosp_agegroup_de_mu$loglik, hosp_agegroup_de_mu$AIC,
        "Hospital", "", "", "$\\sigma, \\mu$", "", hosp_agegroup_de$loglik, hosp_agegroup_de$AIC,
        "Hospital", "", "", "", "$\\sigma$", hosp_agegroup_di_sig$loglik, hosp_agegroup_di_sig$AIC,
        "Hospital", "", "", "", "$\\mu$", hosp_agegroup_di_mu$loglik, hosp_agegroup_di_mu$AIC,
        "Hospital", "", "", "", "$\\sigma, \\mu$", hosp_agegroup_di$loglik, hosp_agegroup_di$AIC,
        "Hospital", "$\\checkmark$", "$\\mu$", "", "", hosp_agegroup_pi$loglik, hosp_agegroup_pi$AIC,
        "Hospital", "$\\checkmark$", "", "$\\sigma$", "", hosp_agegroup_pde_sig$loglik, hosp_agegroup_pde_sig$AIC,
        "Hospital", "$\\checkmark$", "", "$\\mu$", "", hosp_agegroup_pde_mu$loglik, hosp_agegroup_pde_mu$AIC,
        "Hospital", "$\\checkmark$", "", "$\\sigma, \\mu$", "", hosp_agegroup_pde$loglik, hosp_agegroup_pde$AIC,
        "Hospital", "$\\checkmark$", "", "", "$\\sigma$", hosp_agegroup_pdi_sig$loglik, hosp_agegroup_pdi_sig$AIC,
        "Hospital", "$\\checkmark$", "", "", "$\\mu$", hosp_agegroup_pdi_mu$loglik, hosp_agegroup_pdi_mu$AIC,
        "Hospital", "$\\checkmark$", "", "", "$\\sigma, \\mu$", hosp_agegroup_pdi$loglik, hosp_agegroup_pdi$AIC
    ) |>
        arrange(aic),
    tribble(
        ~submodel, ~p, ~icu, ~death, ~discharge, ~loglik, ~aic,
        "ICU", "", "", "", "", icu_agegroup_0$loglik, icu_agegroup_0$AIC,
        "ICU", "$\\checkmark$", "", "", "", icu_agegroup_p$loglik, icu_agegroup_p$AIC,
        "ICU", "", "", "$\\sigma$", "", icu_agegroup_de_sig$loglik, icu_agegroup_de_sig$AIC,
        "ICU", "", "", "$\\mu$", "", icu_agegroup_de_mu$loglik, icu_agegroup_de_mu$AIC,
        "ICU", "$\\checkmark$", "", "$\\sigma$", "", icu_agegroup_pde_sig$loglik, icu_agegroup_pde_sig$AIC,
        "ICU", "$\\checkmark$", "", "$\\mu$", "", icu_agegroup_pde_mu$loglik, icu_agegroup_pde_mu$AIC,
        "ICU", "$\\checkmark$", "", "$\\sigma, \\mu$", "", icu_agegroup_pde$loglik, icu_agegroup_pde$AIC,
        # "ICU", "", "", "$\\times$", "", icu_agegroup_de$loglik, icu_agegroup_de$AIC,
        # "ICU", "", "", "", "$\\times$", icu_agegroup_di$loglik, icu_agegroup_di$AIC,
        # "ICU", "$\\times$", "", "$\\times$", "", icu_agegroup_pde$loglik, icu_agegroup_pde$AIC,
        # "ICU", "$\\times$", "", "", "$\\times$", icu_agegroup_pdi$loglik, icu_agegroup_pdi$AIC
    ) |>
        arrange(aic)
) |>
    mutate(
        loglik = round(loglik, 2),
        aic = round(aic, 2)
    ) |>
    kable(
        format = "latex", booktabs = T, linesep = "", escape = F,
        caption = "Log-likelihood and AIC values for for log-normal and generalized gamma distributions, by inclusion of interaction of month and age group covariate, sorted by sub-model and AIC. Models with unidentifiable parameters not shown.",
        label = "gof_int_age",
        col.names = c(
            "Sub-model", "$\\pi$", "ICU", "Death", "Discharge", "Log-likelihood", "AIC"
        )
    ) |>
    kable_styling(position = "center", latex_options = c("striped", "hold_position"))

save_kable(gof_int_age, here("03_HospitalSeverity/Tables/gof_int_age.tex"))

# characteristics table
load(here("Data/hosp_sentinel_data.RData"))

sari_characteristics <- hospsen |>
    mutate(
        eventm = fct_na_value_to_level(eventm, "Unknown"),
        agegroup = case_when(
            agegroup == "[15,45)" ~ "15--45",
            agegroup == "[45,65)" ~ "45--65",
            agegroup == "[65,75)" ~ "65--75",
            agegroup == "[75,110)" ~ "75+"
        ),
        comorbid_multip = if_else(comorbid_multip == "3", "3+", as.character(comorbid_multip)),
        ethgp = fct_relevel(ethgp, "White", "Asian", "Black", "Mixed/Other", "Unreported")
    ) |>
    select(sex, monthyear, agegroup, ethgp, pheregion, comorbid_multip, eventm) |>
    tbl_summary(
        type = everything() ~ "categorical",
        percent = "row",
        by = eventm,
        label = c(
            sex ~ "Sex",
            monthyear ~ "Month of admission",
            agegroup ~ "Age group",
            ethgp ~ "Ethnicity",
            comorbid_multip ~ "Number of comorbidities",
            pheregion ~ "Region"
        ),
        digits = list(everything() ~ c(0, 1))
    ) |>
    add_overall(
        last = TRUE,
        statistic = ~"{n}"
    ) |>
    modify_footnote(
        update = everything() ~ NA
    ) |>
    modify_header(
        all_stat_cols() ~ "**{level}**  \n N = {format(n, big.mark = ',')}"
    ) |>
    as_kable_extra(
        format = "latex", booktabs = T, linesep = "", longtable = T,
        caption = "Baseline characteristics of patients with COVID-19 admitted to sentinel NHS trusts between 15 March 2020 to 28 February 2021, by outcome following hospital admission.",
        label = "sari_characteristics"
    ) |>
    kable_styling(position = "center") |>
    kable_styling(latex_options = c("hold_position"))

sari_characteristics <- sub("London/South of England", "\\\\makecell[l]\\{London/South\\\\\\\\of England\\}", sari_characteristics)
sari_characteristics <- sub("Midlands and East of England", "\\\\makecell[l]\\{Midlands and\\\\\\\\East of England\\}", sari_characteristics)
sari_characteristics <- sub("Number of comorbidities &", "\\\\multicolumn\\{2\\}\\{l\\}\\{Number of comorbidities\\}", sari_characteristics)

save_kable(sari_characteristics, here("03_HospitalSeverity/Tables/sari_characteristics.tex"))

# SUS characteristics

load(here("Data/hosp_pdata.RData"))
load(here("Data/hosp_sus_noso.RData"))
load(here("Data/hosp_all_comparison.RData"))

sus_characteristics <- bind_rows(
    hosp |> mutate(
        group = "Study population (hospitalised for COVID-19 in England)",
        ageGrp7 = factor(ageGrp7, labels = c("0--14", "15--24", "25--44", "45--64", "65--74", "75--84", "85+")),
        eventm = fct_na_value_to_level(eventm, "Right-censored in hospital"),
    ) |> filter(!is.na(charlson_index), sex != "Unknown"
    ),
    noso |> mutate(
        sex = sexHC,
        group = "All people with hospital-onset COVID-19 in England",
        ageGrp7 = factor(ageGrp7, labels = c("0--14", "15--24", "25--44", "45--64", "65--74", "75--84", "85+")),
        eventm = fct_na_value_to_level(eventm, "Right-censored in hospital"),
        monthyear = NA,
        vaccine = NA,
        comorbid_multip = NA
        ),
    all_comparison |> mutate(
        group = "All people with PCR-confirmed, community-acquired COVID-19 in England",
        ageGrp7 = factor(ageGrp7, labels = c("0--14", "15--24", "25--44", "45--64", "65--74", "75--84", "85+")),
        sex = fct_drop(sex),
        monthyear = NA,
        eventm = NA,
        vaccine = NA,
        comorbid_multip = NA
    )
) |>
    mutate(
        group = factor(group, levels = c("Study population (hospitalised for COVID-19 in England)", "All people with hospital-onset COVID-19 in England", "All people with PCR-confirmed, community-acquired COVID-19 in England")),
        imd_quintile = factor(imd_quintile, labels = c("Most deprived", "2nd quintile", "3rd quintile", "4th quintile", "Least deprived"))
    ) |>
    select(group, ageGrp7, sex, ethGrp4, pheRegion, imd_quintile, eventm, monthyear, vaccine, charlson_index, bed_occupancy_cat) |>
    tbl_summary(
        type = everything() ~ "categorical",
        by = group,
        percent = "col",
        missing = "no",
        label = c(
            ageGrp7 ~ "Age group",
            sex ~ "Sex",
            ethGrp4 ~ "Ethnicity",
            pheRegion ~ "Region of residence",
            imd_quintile ~ "Index of Multiple Deprivation",
            eventm ~ "Hospital outcome",
            monthyear ~ "Month of hospital admission",
            vaccine ~ "Vaccination status at date of admission (January 2021 onwards)",
            charlson_index ~ "Charlson comorbidity index",
            bed_occupancy_cat ~ "Hospital load at time of admission (as proportion of busiest week)"
        )
    ) |>
    modify_footnote(
        update = everything() ~ NA
    ) |>
    modify_header(
        all_stat_cols() ~ "**{level}**  \n N = {format(n, big.mark = ',')}"
    ) |>
    as_kable_extra(
        format = "latex", booktabs = T, linesep = "", longtable = T,
        caption = "Characteristics of the study population compared with all people hospital-onset COVID-19, and all people with PCR-confirmed community-acquired COVID-19 in England.",
        label = "sus_characteristics"
    ) |>
    kable_styling(position = "center") |>
    kable_styling(latex_options = c("hold_position"))

sus_characteristics <- sub("Study population \\(hospitalised for COVID-19 in England\\)", 
                           "Study population\\}\\\\\\ \\\\\\ \\\\\\\\\\\ \\\\\\textbf\\{(hospitalised for\\}\\\\\\ \\\\\\ \\\\\\\\\\\ \\\\\\textbf\\{COVID-19\\}\\\\\\ \\\\\\ \\\\\\\\\\\ \\\\\\textbf\\{in England)", sus_characteristics)
sus_characteristics <- sub("All people with hospital-onset COVID-19 in England", 
                           "All people with\\}\\\\\\ \\\\\\ \\\\\\\\\\\ \\\\\\textbf\\{hospital-onset\\}\\\\\\ \\\\\\ \\\\\\\\\\\ \\\\\\textbf\\{COVID-19\\}\\\\\\ \\\\\\ \\\\\\\\\\\ \\\\\\textbf\\{in England", sus_characteristics)
sus_characteristics <- sub("All people with PCR-confirmed, community-acquired COVID-19 in England", 
                           "All people with\\}\\\\\\ \\\\\\ \\\\\\\\\\\ \\\\\\textbf\\{PCR-confirmed,\\}\\\\\\ \\\\\\ \\\\\\\\\\\ \\\\\\textbf\\{community-acquired\\}\\\\\\ \\\\\\ \\\\\\\\\\\ \\\\\\textbf\\{COVID-19 in England", sus_characteristics)
sus_characteristics <- gsub("0 \\(NA\\\\%\\)", "-", sus_characteristics)
                           
sus_characteristics <- sub("=21 days", "$\\\\\\leq$21 days", sus_characteristics)
sus_characteristics <- gsub("=14 days", "$\\\\\\geq$14 days", sus_characteristics)
sus_characteristics <- sub("Vaccination status at date of admission \\(January 2021 onwards\\) &  &", "\\\\multicolumn\\{3\\}\\{l\\}\\{Vaccination status at date of admission (January 2021 onwards)\\}", sus_characteristics)
sus_characteristics <- sub("Hospital load at time of admission \\(as proportion of busiest week\\) &  &", "\\\\multicolumn\\{3\\}\\{l\\}\\{Hospital load at time of admission (as proportion of busiest week)\\}", sus_characteristics)
sus_characteristics <- sub("Index of Multiple Deprivation &  &", "\\\\multicolumn\\{3\\}\\{l\\}\\{Index of Multiple Deprivation\\}", sus_characteristics)

save_kable(sus_characteristics, here("03_HospitalSeverity/Tables/sus_characteristics.tex"))
