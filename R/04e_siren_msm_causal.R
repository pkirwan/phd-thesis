# Figures and tables for Chapter 4

# Load libraries
here::i_am("R/04e_siren_msm_causal.R")
library(here)
library(tidyverse)

load(here("Data/siren_expected_pos.RData"))
load(here("Data/siren_expected_pos_ft.RData"))
load(here("Data/siren_causal_diagnostics.RData"))

# figures extracted from ppass estimates run on morricone
ppass <- tribble(
    ~vaccine, ~mean, ~lower, ~upper,
    "No vaccination", 0.4884 * 6349, 0.3663 * 6349, 0.627 * 6349,
    "Complete vaccination", 0.419 * 6349, 0.3137 * 6349, 0.5476 * 6349
)

ppass |>
    mutate(
        vaccine = factor(vaccine, levels = c("No vaccination", "Complete vaccination"))
    ) |>
    ggplot() +
    aes(x = vaccine, y = mean, ymin = lower, ymax = upper, colour = vaccine) +
    geom_point(position = position_dodge(width = 0.6)) +
    geom_errorbar(position = position_dodge(width = 0.6), width = 0.6) +
    labs(
        x = "Counterfactual scenario",
        y = "Estimated number of infections",
        color = "Counterfactual scenario"
    ) +
    scale_y_continuous(limits = c(0, 4000)) +
    theme_minimal(15) +
    theme(
        legend.position = "none"
    ) +
    scale_colour_manual(values = thesis_col[seq(1, length(thesis_col), 3)])

ggsave(here("04_SIREN/Figs/ppass_causal_msm.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/ppass_causal_msm.png"), width = 10, height = 6)

# 157 people were infected in the cohort at the start of the study
# remove these from the expected verification set

rbind(
    "Bi-weekly sampling scheme" = make_quantiles(expected_sim_ft) - 157,
    "Observed sampling scheme" = make_quantiles(expected_sim) - 157
) |>
    as_tibble(rownames = "method") |>
    mutate(
        method = factor(method, levels = c("Observed sampling scheme", "Bi-weekly sampling scheme"))
    ) |>
    ggplot() +
    aes(x = method, y = mean, ymin = lower, ymax = upper) +
    geom_hline(yintercept = 1023, linetype = 2, color = thesis_col[3]) +
    annotate("text", y = 1023, x = "Bi-weekly sampling scheme", label = "Number of infections detected\n(n = 1023)", vjust = 0.5, color = thesis_col[3], size = 5) +
    geom_point(colour = thesis_col[7]) +
    geom_errorbar(width = 0.4, colour = thesis_col[7]) +
    labs(
        x = "Sampling scheme",
        y = "Expected number of detected infections"
    ) +
    scale_y_continuous(limits = c(900, NA)) +
    theme_minimal(15) +
    theme(
        legend.position = "top"
    )

ggsave(here("04_SIREN/Figs/expected_causal_msm.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/expected_causal_msm.png"), width = 10, height = 6)

# model diagnostics
diagnostics_plot(diagnostics_causal) + theme(plot.subtitle = element_blank())

ggsave(here("04_SIREN/Figs/causal_model_diagnostics.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/causal_model_diagnostics.png"), width = 10, height = 6)