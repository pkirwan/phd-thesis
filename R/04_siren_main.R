# SIREN data outputs using make

make(
    recipe = "R/04a_siren_data_preparation.R",
    prereq = c(
        "Data/SIREN/msm_ppass_msvrahgo.RData",
        "R/00_simvax_msm.R",
        "Data/SIREN/Interim6_positives_seq.xlsx"
    ),
    target = c(
        "Data/siren_cohort.RData",
        "Data/siren_seq_df.RData",
        "Data/siren_hazards.RData",
        "Data/siren_models.RData"
    )
)

make(
    recipe = "R/04b_siren_characteristics.R",
    prereq = c(
        "Data/siren_cohort.RData"
    ),
    target = c(
        "04_SIREN/Figs/proportion_vaccinated.pdf",
        "04_SIREN/Figs/participants.pdf",
        "04_SIREN/Figs/infections.pdf",
        "04_SIREN/Figs/siren_timeline.pdf",
        "04_SIREN/Figs/symptoms_age_sex.pdf",
        "04_SIREN/Figs/occupation_household.pdf",
        "04_SIREN/Figs/fortnightly_schedule.pdf"
    )
)

make(
    recipe = "R/04c_study_characteristics.R",
    prereq = c(
        "Data/SIREN/siren_processed_interim4_vax_final.RData"
    ),
    target = c(
        "04_SIREN/Figs/vaccination_history_region.pdf",
        "04_SIREN/Figs/vaccination_demog.pdf",
        "04_SIREN/Figs/symptoms_vaccine_tsp.pdf",
        "04_SIREN/Figs/start_end_followup.pdf"
    )
)

make(
    recipe = "R/04d_siren_msm.R",
    prereq = c(
        "Data/siren_diagnostics.RData",
        "Data/siren_sojourn_times.RData",
        "R/00_forest_plot.R"
    ),
    target = c(
        "04_SIREN/Figs/vaccine_long.pdf",
        "04_SIREN/Figs/vaccine_short.pdf",
        "04_SIREN/Figs/vaccine_short_noev.pdf",
        "04_SIREN/Figs/vaccine_interaction_noev.pdf",
        "04_SIREN/Figs/vaccine_short_sym.pdf",
        "04_SIREN/Figs/vaccine_long_sym.pdf",
        "04_SIREN/Figs/vaccine_msp_short_sym.pdf",
        "04_SIREN/Figs/vaccine_msp_long_sym.pdf",
        "04_SIREN/Figs/ve_long.pdf",
        "04_SIREN/Figs/ve_rel.pdf",
        "04_SIREN/Figs/sojourn_time.pdf",
        "04_SIREN/Figs/model_diagnostics.pdf"
    )
)

make(
    recipe = "R/04e_siren_msm_causal.R",
    prereq = c(
        "Data/siren_causal_diagnostics.RData",
        "Data/siren_expected_pos.RData",
        "Data/siren_expected_pos_ft.RData",
        # "Data/siren_ppass.RData",
        "R/00_forest_plot.R"
    ),
    target = c(
        "04_SIREN/Figs/ppass_causal_msm.pdf",
        "04_SIREN/Figs/causal_model_diagnostics.pdf",
        "04_SIREN/Figs/expected_causal_msm.pdf"
    )
)

make(
    recipe = "R/04f_siren_tables.R",
    prereq = c(
    "Data/SIREN/siren_processed_interim4_vax_final.RData",
    "Data/siren_cohort.RData",
    "Data/siren_hazards.RData",
    "Data/siren_sojourn_times.RData",
    "R/00_forest_plot.R",
    "Data/siren_expected_pos.RData",
    "Data/siren_expected_pos_ft.RData",
    "Data/siren_diagnostics.RData"
),
    target = c(
        "04_SIREN/Tables/siren_demographics.tex",
        "04_SIREN/Tables/siren_vaccine_uptake.tex",
        "04_SIREN/Tables/pcr_positivity.tex",
        "04_SIREN/Tables/sojourn_time.tex",
        "04_SIREN/Tables/fortnightly_schedule.tex",
        "04_SIREN/Tables/expected_infections.tex",
        "04_SIREN/Tables/causal_table.tex",
        "04_SIREN/Tables/siren_deviance.tex"
    )
)

make(
    recipe = "R/04x_siren_appendix.R",
    prereq = c(
        "Data/siren_seq_df.RData",
        "Data/siren_sojourn_times.RData",
        "Data/siren_hazards.RData"
    ),
    target = c(
        "04_SIREN/Figs/ve_convalescent.pdf",
        "04_SIREN/Figs/ve_baseline.pdf",
        "04_SIREN/Figs/interim4_sequences.pdf",
        "04_SIREN/Figs/sojourn_semi_markov.pdf",
        "04_SIREN/Figs/ve_misclass.pdf"
    )
)