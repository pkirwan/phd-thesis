# Figures and tables for Chapter 2

# Load libraries
here::i_am("R/02x_hiv_appendix.R")

library(here)
library(tidyverse)
library(patchwork)
library(ggforce)
library(scales)

###################
# Simulated data with missing information
###################
load(here("Data/hiv_sim_estimates.RData"))
load(here("data/hiv_sim_estimates_missing.RData"))

# plot data for boxplots
plot_data <- bind_rows(
    misclass_incidence_miss |> mutate(group = "CD4-only model"),
    rita_incidence_miss |> mutate(group = "Dual biomarker\nmodel"),
    # reclass_incidence |> mutate(group = "Naive\nmodel"),
) |>
    left_join(incidence_par, by = "year") |> # add the observed incidence
    mutate(diff = value.x - value.y) |>
    filter(quartile == "50%", year > 0)

# MSE
mse <- plot_data |>
    group_by(model, group.x) |>
    summarise(mse = mean(diff^2))

box_plot_year(plot_data)

ggsave(here("02_HIVBackCalc/Figs/incidence_boxplot_year_miss.pdf"), width = 10, height = 13)
ggsave(here("02_HIVBackCalc/Figs/incidence_boxplot_year_miss.png"), width = 10, height = 13)

p1 <- box_plot(plot_data)

p2 <- mse_plot(mse)

p1 + p2

ggsave(here("02_HIVBackCalc/Figs/incidence_mse_miss.pdf"), width = 10, height = 6)
ggsave(here("02_HIVBackCalc/Figs/incidence_mse_miss.png"), width = 10, height = 6)

# figures
p1 <- bind_rows(
    misclass_undiag_prev_miss |> group_by(year, quartile) |> summarise(value = median(value)) |> mutate(group = "CD4-only model"),
    rita_undiag_prev_miss |> group_by(year, quartile) |> summarise(value = median(value)) |> mutate(group = "Dual biomarker model")
) |> bc_ribbon_plot(point_data = undiag_prev_par, xlab = "Quarter")

p2 <- bind_rows(
    misclass_incidence_miss |> filter(year > 0) |> group_by(year, quartile) |> summarise(value = median(value)) |> mutate(group = "CD4-only model"),
    rita_incidence_miss |> filter(year > 0) |> group_by(year, quartile) |> summarise(value = median(value)) |> mutate(group = "Dual biomarker model")
) |> bc_ribbon_plot(point_data = incidence_par, xlab = "Quarter", ylab = "Estimated HIV incidence")

p3 <- bind_rows(
    misclass_diag_prob_miss |> group_by(year, quartile, strata) |> summarise(value = median(value)) |> mutate(group = "CD4-only model"),
    rita_diag_prob_miss |> group_by(year, quartile, strata) |> summarise(value = median(value)) |> mutate(group = "Dual biomarker model")
) |> bc_diag_prob_plot(point_data = diag_prob_par)

(p2 + p1) / p3 + plot_annotation(tag_levels = "A")

ggsave(here("02_HIVBackCalc/Figs/sim_estimates_miss.pdf"), width = 10, height = 13)
ggsave(here("02_HIVBackCalc/Figs/sim_estimates_miss.png"), width = 10, height = 13)

bind_rows(
    misclass_diagnoses_miss |> group_by(year, quartile) |> filter(year > 0) |> summarise(value = median(value)) |> mutate(group = "CD4-only model"),
    rita_diagnoses_miss |> group_by(year, quartile) |> filter(year > 0) |> summarise(value = median(value)) |> mutate(group = "Dual biomarker model")
) |> bc_ribbon_plot(point_data = diagnoses, xlab = "Quarter", legend_pos = "bottom", ylab = "Estimated HIV diagnoses")

ggsave(here("02_HIVBackCalc/Figs/diag_fit_sim_miss.pdf"), width = 10, height = 6)
ggsave(here("02_HIVBackCalc/Figs/diag_fit_sim_miss.png"), width = 10, height = 6)


###################
# Naive data comparison
###################

# plot data for boxplots
plot_data <- bind_rows(
    rita_incidence |> mutate(group = "Dual biomarker\nmodel"),
    reclass_incidence |> mutate(group = "CD4-only model\nwith reclassification")
) |>
    left_join(incidence_par, by = "year") |> # add the observed incidence
    mutate(diff = value.x - value.y) |>
    filter(quartile == "50%", year > 0) |>
    mutate(group.x = factor(group.x, levels = c("Dual biomarker\nmodel", "CD4-only model\nwith reclassification")))

# MSE
mse <- plot_data |>
    group_by(model, group.x) |>
    summarise(mse = mean(diff^2))

box_plot_year(plot_data,
    col = thesis_col[seq(4, length(thesis_col), 7)]
)

ggsave(here("02_HIVBackCalc/Figs/incidence_boxplot_year_reclass.pdf"), width = 10, height = 13)
ggsave(here("02_HIVBackCalc/Figs/incidence_boxplot_year_reclass.png"), width = 10, height = 13)

p1 <- box_plot(plot_data, col = thesis_col[seq(4, length(thesis_col), 7)])

p2 <- mse_plot(mse, col = thesis_col[seq(4, length(thesis_col), 7)], offset = 15)

p1 + p2

ggsave(here("02_HIVBackCalc/Figs/incidence_mse_reclass.pdf"), width = 10, height = 6)
ggsave(here("02_HIVBackCalc/Figs/incidence_mse_reclass.png"), width = 10, height = 6)

# figures
p1 <- bind_rows(
    rita_undiag_prev |> group_by(year, quartile) |> summarise(value = median(value)) |> mutate(group = "Dual biomarker model"),
    reclass_undiag_prev |> group_by(year, quartile) |> summarise(value = median(value)) |> mutate(group = "CD4-only model with reclassification")
) |>
    mutate(group = factor(group, levels = c("Dual biomarker model", "CD4-only model with reclassification"))) |>
    bc_ribbon_plot(point_data = undiag_prev_par, xlab = "Quarter", line_col = thesis_col[seq(4, length(thesis_col), 7)])

p2 <- bind_rows(
    rita_incidence |> group_by(year, quartile) |> filter(year > 0) |> summarise(value = median(value)) |> mutate(group = "Dual biomarker model"),
    reclass_incidence |> group_by(year, quartile) |> filter(year > 0) |> summarise(value = median(value)) |> mutate(group = "CD4-only model with reclassification")
) |>
    mutate(group = factor(group, levels = c("Dual biomarker model", "CD4-only model with reclassification"))) |>
    bc_ribbon_plot(point_data = incidence_par, xlab = "Quarter", ylab = "Estimated HIV incidence", line_col = thesis_col[seq(4, length(thesis_col), 7)])

p3 <- bind_rows(
    rita_diag_prob |> group_by(year, quartile, strata) |> summarise(value = median(value)) |> mutate(group = "Dual biomarker model"),
    reclass_diag_prob |> group_by(year, quartile, strata) |> summarise(value = median(value)) |> mutate(group = "CD4-only model with reclassification")
) |>
    mutate(group = factor(group, levels = c("Dual biomarker model", "CD4-only model with reclassification"))) |>
    bc_diag_prob_plot(point_data = diag_prob_par, line_col = thesis_col[seq(4, length(thesis_col), 7)])

(p2 + p1) / p3 + plot_annotation(tag_levels = "A")

ggsave(here("02_HIVBackCalc/Figs/sim_estimates_reclass.pdf"), width = 10, height = 13)
ggsave(here("02_HIVBackCalc/Figs/sim_estimates_reclass.png"), width = 10, height = 13)

bind_rows(
    rita_diagnoses |> group_by(year, quartile) |> filter(year > 0) |> summarise(value = median(value)) |> mutate(group = "Dual biomarker model"),
    reclass_diagnoses |> group_by(year, quartile) |> filter(year > 0) |> summarise(value = median(value)) |> mutate(group = "CD4-only model with reclassification")
) |>
    mutate(group = factor(group, levels = c("Dual biomarker model", "CD4-only model with reclassification"))) |>
    bc_ribbon_plot(point_data = diagnoses, xlab = "Quarter", ylab = "Estimated HIV diagnoses", legend_pos = "bottom", line_col = thesis_col[seq(4, length(thesis_col), 7)])

ggsave(here("02_HIVBackCalc/Figs/diag_fit_sim_reclass.pdf"), width = 10, height = 6)
ggsave(here("02_HIVBackCalc/Figs/diag_fit_sim_reclass.png"), width = 10, height = 6)
