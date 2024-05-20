# HIV data outputs using make

make(
    recipe = "R/02a_hiv_data_preparation.R",
    prereq = c(
        "Data/HIV/ProcOutput_rita_2011_2019_data6.RData",
        "Data/HIV/ProcOutput_ai_2011_2019_data6.RData",
        "Data/HIV/ProcOutput_sim.RData",
        "R/00_hiv_model_sim.R"
    ),
    target = c(
        "Data/hiv_msm_2023.RData",
        "Data/hiv_estimates.RData",
        "Data/hiv_rita_estimates.RData",
        "Data/hiv_sim_estimates.RData",
        "Data/hiv_sim_estimates_missing.RData",
        "Data/hiv_ld_paper.RData",
        "Data/hiv_forecast.RData"
    )
)

make(
    recipe = "R/02b_hiv_data_availability.R",
    prereq = "Data/hiv_msm_2023.RData",
    target = c(
        "02_HIVBackCalc/Figs/cd4_trajectory.pdf",
        "02_HIVBackCalc/Figs/rita_availability_by_cd4.pdf",
        "02_HIVBackCalc/Figs/lntest_by_cd4.pdf",
        "02_HIVBackCalc/Figs/vl_availability_by_cd4.pdf",
        "02_HIVBackCalc/Figs/vl_cd4_recent.pdf",
        "02_HIVBackCalc/Figs/latediag.pdf"
    )
)

make(
    recipe = "R/02c_hiv_tables.R",
    prereq = c(
        "Data/hiv_msm_2023.RData",
        "Data/hiv_rita_estimates.RData",
        "Data/HIV/ProcOutput_ai_1995_2021_data6_cf.RData"
    ),
    target = c(
        "02_HIVBackCalc/Tables/cd4_dist_age.tex",
        "02_HIVBackCalc/Tables/cd4_dist_eey.tex",
        "02_HIVBackCalc/Tables/rita_demog.tex",
        "02_HIVBackCalc/Tables/lastneg_demog.tex",
        "02_HIVBackCalc/Tables/vl_demog.tex",
        "02_HIVBackCalc/Tables/incidence_annual.tex",
        "02_HIVBackCalc/Tables/counterfactual_inc_annual.tex"
    )
)

make(
    recipe = "R/02d_hiv_backcalc.R",
    prereq = c(
        "Data/hiv_estimates.RData",
        "Data/hiv_sim_estimates.RData",
        "Data/hiv_rita_estimates.RData",
        "R/00_ribbon_plot.R",
        "Data/hiv_forecast.RData"
    ),
    target = c(
        "02_HIVBackCalc/Figs/incidence_overlay_2014_2018.pdf",
        "02_HIVBackCalc/Figs/incidence_overlay_2017_2022.pdf",
        "02_HIVBackCalc/Figs/lambda_sim.pdf",
        "02_HIVBackCalc/Figs/incidence_boxplot_sim_year.pdf",
        "02_HIVBackCalc/Figs/incidence_mse_sim.pdf",
        "02_HIVBackCalc/Figs/sim_estimates.pdf",
        "02_HIVBackCalc/Figs/diag_fit_sim.pdf",
        "02_HIVBackCalc/Figs/rita_estimates.pdf",
        "02_HIVBackCalc/Figs/diag_fit_rita.pdf",
        "02_HIVBackCalc/Figs/undiag_prev_mpes.pdf",
        "02_HIVBackCalc/Figs/projected_incidence.pdf"
    )
)

make(
    recipe = "R/02f_hiv_counterfactual.R",
    prereq = c(
        "Data/HIV/ProcOutput_ai_1995_2021_data6_cf.RData",
        "Data/HIV/model_ai_1995_2021_data6.RData",
        "R/00_ribbon_plot.R"
    ),
    target = c(
        "02_HIVBackCalc/Figs/lockdown_diagnoses.pdf",
        "02_HIVBackCalc/Figs/counterfactual_estimates.pdf",
        "02_HIVBackCalc/Figs/counterfactual_fit.pdf"
    )
)

make(
    recipe = "R/02x_hiv_appendix.R",
    prereq = c(
        "Data/hiv_sim_estimates.RData",
        "Data/hiv_sim_estimates_missing.RData",
        "R/00_ribbon_plot.R"
    ),
    target = c(
        "02_HIVBackCalc/Figs/incidence_boxplot_year_miss.pdf",
        "02_HIVBackCalc/Figs/incidence_mse_miss.pdf",
        "02_HIVBackCalc/Figs/sim_estimates_miss.pdf",
        "02_HIVBackCalc/Figs/diag_fit_sim_miss.pdf",
        "02_HIVBackCalc/Figs/incidence_boxplot_year_reclass.pdf",
        "02_HIVBackCalc/Figs/incidence_mse_reclass.pdf",
        "02_HIVBackCalc/Figs/sim_estimates_reclass.pdf",
        "02_HIVBackCalc/Figs/diag_fit_sim_reclass.pdf"
    )
)