# Hospitalised severity data outputs using make

make(
    recipe = "R/03a_hosp_data_preparation.R",
    prereq = c(
        "Data/Hosp/data_2023-Dec-14.csv",
        "Data/Hosp/chess_week_feb.RData",
        "Data/Hosp/chess_mixcure_apr.RData",
        "Data/Hosp/sus_ecds_processed.RData"
    ),
    target = c(
        "Data/hosp_cases.RData",
        "Data/hosp_sentinel_data.RData"
    )
)

make(
    recipe = "R/03b_hosp_cases.R",
    prereq = c(
        "Data/hosp_cases.RData",
        "Data/hosp_sentinel_data.RData"
    ),
    target = c(
        "03_HospitalSeverity/Figs/pandemic_waves.pdf",
        "03_HospitalSeverity/Figs/sari_icu_admission.pdf",
        "03_HospitalSeverity/Figs/sari_time_distribution.pdf",
        "03_HospitalSeverity/Figs/sus_vaccine_month.pdf"
    )
)

make(
    recipe = "R/03c_hosp_sari.R",
    prereq = c(
        "Data/hosp_mixcure.RData",
        "R/00_ajfit_plot.R"
    ),
    target = c(
        "03_HospitalSeverity/Figs/sari_gof_hosp.pdf",
        "03_HospitalSeverity/Figs/sari_gof_icu.pdf",
        "03_HospitalSeverity/Figs/sari_event_prob.pdf",
        "03_HospitalSeverity/Figs/sari_icu_prob.pdf",
        "03_HospitalSeverity/Figs/sari_hfr.pdf",
        "03_HospitalSeverity/Figs/sari_med_time.pdf",
        "03_HospitalSeverity/Figs/sari_med_time_icu.pdf",
        "03_HospitalSeverity/Figs/sari_weekly_prob.pdf"
    )
)

make(
    recipe = "R/03d_hosp_tables.R",
    prereq = c(
        "Data/Hosp/chess_mixcure_dists.RData",
        "Data/Hosp/chess_mixcure_anc.RData",
        "Data/Hosp/chess_mixcure_covs.RData",
        "Data/Hosp/chess_mixcure_covs_age.RData",
        "Data/hosp_sentinel_data.RData",
        "Data/hosp_sus.RData",
        "Data/hosp_sus_noso.RData",
        "Data/hosp_all_comparison.RData"
    ),
    target = c(
        "03_HospitalSeverity/Tables/gof_dist.tex",
        "03_HospitalSeverity/Tables/gof_place.tex",
        "03_HospitalSeverity/Tables/gof_int.tex",
        "03_HospitalSeverity/Tables/sari_characteristics.tex",
        "03_HospitalSeverity/Tables/sus_characteristics.tex"
    )
)

make(
    recipe = "R/03e_hosp_sus.R",
    prereq = c(
        "R/00_fg_functions.R",
        "R/00_aj_functions.R",
        "Data/hosp_sus.RData",
        "Data/Hosp/coxfit_multi_vaccine.RData",
        "Data/Hosp/coxfit_multi_month.RData",
        "Data/hosp_fg_shift.RData"
    ),
    target = c(
        "03_HospitalSeverity/Figs/fg_aj_month_comparison.pdf",
        "03_HospitalSeverity/Figs/hfr_month.pdf",
        "03_HospitalSeverity/Figs/hfr_month_age.pdf",
        "03_HospitalSeverity/Figs/hfr_month_load.pdf",
        "03_HospitalSeverity/Figs/hfr_month_sex.pdf",
        "03_HospitalSeverity/Figs/hfr_month_ethn.pdf",
        "03_HospitalSeverity/Figs/hfr_month_cci.pdf",
        "03_HospitalSeverity/Figs/fg_vaccine.pdf",
        "03_HospitalSeverity/Figs/fg_hazards.pdf",
        "03_HospitalSeverity/Figs/fg_month.pdf",
        "03_HospitalSeverity/Figs/fg_shift.pdf"
    )
)
