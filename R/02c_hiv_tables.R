# Figures and tables for Chapter 2

# Load libraries
here::i_am("R/02c_hiv_tables.R")

library(here)
library(tidyverse)
library(kableExtra)
library(gtsummary)

# RITA test availability by CD4 strata
load(here("Data/hiv_msm_2023.RData"))

# CD4 count distribution for those who are reclassified, by year and overall

cd4_dist_age <- msm |>
    filter(earliesteventyear > 2010, earliesteventyear < 2020, recent == "Y", cd4cat != "No CD4") |>
    mutate(
        agegpdiag = fct_drop(agegpdiag),
        cd4cat = fct_drop(cd4cat)
    ) |>
    tbl_cross(
        row = cd4cat,
        col = agegpdiag,
        label = c(
            cd4cat ~ "CD4 strata",
            agegpdiag ~ "Age group at diagnosis"
        ),
        percent = "col",
        margin = "col",
        digits = c(0, 1)
    ) |>
    as_kable_extra(
        format = "latex", booktabs = T, linesep = "",
        caption = "Distribution of CD4 cell counts among those diagnosed with recently acquired HIV between 2011--2019, by age group at diagnosis.",
        label = "cd4_dist_age"
    ) |>
    kable_styling(position = "center") |>
    kable_styling(latex_options = c("scale_down", "hold_position"))

save_kable(cd4_dist_age, here("02_HIVBackCalc/Tables/cd4_dist_age.tex"))

cd4_dist_eey <- msm |>
    filter(earliesteventyear > 2010, earliesteventyear < 2020, recent == "Y", cd4cat != "No CD4") |>
    mutate(
        cd4cat = fct_drop(cd4cat)
    ) |>
    tbl_cross(
        row = earliesteventyear,
        col = cd4cat,
        label = c(
            cd4cat ~ "CD4 strata",
            earliesteventyear ~ "Year of diagnosis"
        ),
        percent = "row",
        margin = "row",
        digits = c(0, 1)
    ) |>
    as_kable_extra(
        format = "latex", booktabs = T, linesep = "",
        caption = "Distribution of CD4 cell counts among those diagnosed with recently acquired HIV between 2011--2019, by year of diagnosis.",
        label = "cd4_dist_eey"
    ) |>
    kable_styling(position = "center") |>
    kable_styling(latex_options = c("hold_position"))

save_kable(cd4_dist_eey, here("02_HIVBackCalc/Tables/cd4_dist_eey.tex"))

# Summary tables with demographics of RITA and non-RITA tested GBM
rita_demog <- msm |>
    filter(earliesteventyear > 2010, earliesteventyear < 2020) |>
    select(agegpdiag, ethngp, pheregionres, cd4cat, assay_test) |>
    mutate(
        agegpdiag = fct_drop(agegpdiag)
    ) |>
    tbl_summary(
        type = everything() ~ "categorical",
        percent = "col",
        by = assay_test,
        label = c(
            agegpdiag ~ "Age group at diagnosis",
            ethngp ~ "Ethnicity",
            pheregionres ~ "Region of residence",
            cd4cat ~ "CD4 count at diagnosis"
        ),
        digits = list(everything() ~ c(0, 1))
    ) |>
    modify_footnote(
        update = everything() ~ NA
    ) |>
    modify_header(
        all_stat_cols() ~ "**{level}**  \n N = {format(n, big.mark = ',')}"
    ) |>
    as_kable_extra(
        format = "latex", booktabs = T, linesep = "",
        caption = "Demographic characteristics of GBM newly diagnosed in EW\\&NI between 2011--2019 by availability of incidence assay.",
        label = "rita_demog"
    ) |>
    kable_styling(position = "center") |>
    kable_styling(latex_options = c("hold_position"))

save_kable(rita_demog, here("02_HIVBackCalc/Tables/rita_demog.tex"))

# Summary tables with demographics of LN and non-LN GBM
lastneg_demog <- msm |>
    filter(earliesteventyear > 2010, earliesteventyear < 2020) |>
    select(agegpdiag, ethngp, pheregionres, cd4cat, lastneg) |>
    mutate(
        agegpdiag = fct_drop(agegpdiag)
    ) |>
    tbl_summary(
        type = everything() ~ "categorical",
        percent = "col",
        by = lastneg,
        label = c(
            agegpdiag ~ "Age group at diagnosis",
            ethngp ~ "Ethnicity",
            pheregionres ~ "Region of residence",
            cd4cat ~ "CD4 count at diagnosis"
        ),
        digits = list(everything() ~ c(0, 1))
    ) |>
    modify_footnote(
        update = everything() ~ NA
    ) |>
    modify_header(
        all_stat_cols() ~ "**{level}**  \n N = {format(n, big.mark = ',')}"
    ) |>
    as_kable_extra(
        format = "latex", booktabs = TRUE, linesep = "",
        caption = "Demographic characteristics of GBM newly diagnosed in EW\\&NI between 2011--2019 by availability of previous negative test.",
        label = "lastneg_demog"
    ) |>
    kable_styling(position = "center") |>
    kable_styling(latex_options = c("hold_position"))

save_kable(lastneg_demog, here("02_HIVBackCalc/Tables/lastneg_demog.tex"))

vl_demog <- msm |>
    filter(earliesteventyear > 2010, earliesteventyear < 2020) |>
    select(agegpdiag, ethngp, pheregionres, cd4cat, vl_avail) |>
    mutate(
        agegpdiag = fct_drop(agegpdiag)
    ) |>
    tbl_summary(
        type = everything() ~ "categorical",
        percent = "col",
        by = vl_avail,
        label = c(
            agegpdiag ~ "Age group at diagnosis",
            ethngp ~ "Ethnicity",
            pheregionres ~ "Region of residence",
            cd4cat ~ "CD4 count at diagnosis"
        ),
        digits = list(everything() ~ c(0, 1))
    ) |>
    modify_footnote(
        update = everything() ~ NA
    ) |>
    modify_header(
        all_stat_cols() ~ "**{level}**  \n N = {format(n, big.mark = ',')}"
    ) |>
    as_kable_extra(
        format = "latex", booktabs = T, linesep = "",
        caption = "Demographic characteristics of GBM newly diagnosed in EW\\&NI between 2011--2019 by availability of baseline VL.",
        label = "vl_demog"
    ) |>
    kable_styling(position = "center") |>
    kable_styling(latex_options = c("hold_position"))

save_kable(vl_demog, here("02_HIVBackCalc/Tables/vl_demog.tex"))

# Table of annual incidence from CD4-only and dual biomarker back-calculation

load(here("Data/hiv_rita_estimates.RData"))

inc_wider <- function(df, start_year) {
    df |>
        filter(year >= start_year) |>
        pivot_wider(names_from = quartile, values_from = value) |>
        mutate(
            model = paste0(
                round(`50%`, -1),
                " (",
                round(`2.5%`, -1),
                ", ",
                round(`97.5%`, -1),
                ")"
            )
        ) |>
        select(year, model)
}

incidence_annual <- left_join(
    inc_wider(hiv_2019_spl$incidence_annual, start_year = 2012) |>
        rename(`CD4-only` = model),
    inc_wider(hiv_2019_rita$incidence_annual, start_year = 2012) |>
        rename(`Dual biomarker` = model),
    by = "year"
) |>
    rename(" " = year) |>
    kable(
        format = "latex", booktabs = T, linesep = "",
        caption = "Estimated annual incidence (posterior median and 95\\% CrI) of HIV among GBM in EW\\&NI between 2012--2019, by model.",
        label = "incidence_annual"
    ) |>
    add_header_above(c(" " = 1, "Model" = 2)) |>
    pack_rows("Year", 1, 8, bold = FALSE) |>
    kable_styling(position = "center") |>
    kable_styling(latex_options = c("hold_position"))

save_kable(incidence_annual, here("02_HIVBackCalc/Tables/incidence_annual.tex"))

load(here("Data/HIV/ProcOutput_ai_1995_2021_data6_cf.RData"))
load(here("Data/HIV/ProcOutput_ai_1995_2022_data6.RData"))

counterfactual_inc_annual <- left_join(
    inc_wider(hiv_2022_spl$incidence_annual, start_year = 2016) |>
        rename(`Unmodified model` = model),
    inc_wider(hiv_2021_spl_cf1$incidence_annual, start_year = 2016) |>
        rename(`(a) Incidence unaffected` = model),
    by = "year"
) |>
    left_join(
        inc_wider(hiv_2021_spl_cf2$incidence_annual, start_year = 2016) |>
            rename(`(b) Diagnosis probabilities unaffected` = model),
        by = "year"
    ) |>
    rename(" " = year) |>
    kable(
        format = "latex", booktabs = T, linesep = "",
        caption = "Estimated annual incidence (posterior median and 95\\% CrI) of HIV among GBM in EW\\&NI between 2012--2021, by model.",
        label = "counterfactual_inc_annual"
    ) |>
    add_header_above(c(" " = 1, "Scenario" = 3)) |>
    pack_rows("Year", 1, 6, bold = FALSE) |>
    kable_styling(position = "center") |>
    kable_styling(latex_options = c("hold_position"))

counterfactual_inc_annual <- sub("\\(b\\) Diagnosis probabilities unaffected", "\\\\makecell[l]\\{(b) Diagnosis probabilities\\\\\\\\unaffected\\}", counterfactual_inc_annual)

save_kable(counterfactual_inc_annual, here("02_HIVBackCalc/Tables/counterfactual_inc_annual.tex"))
