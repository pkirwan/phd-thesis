# Figures and tables for Chapter 2

# Load libraries
here::i_am("R/02d_hiv_backcalc.R")

library(here)
library(tidyverse)
library(patchwork)
library(ggforce)
library(scales)
library(ggridges)

#####################
# Overlaid estimates
#####################
load(here("Data/hiv_estimates.RData"))

hiv_2014 <- hiv_2014_rw$incidence_annual |>
    pivot_wider(names_from = quartile, values_from = value) |>
    filter(year > 2005)
hiv_2015 <- hiv_2015_rw$incidence_annual |>
    pivot_wider(names_from = quartile, values_from = value) |>
    filter(year > 2005)
hiv_2016 <- hiv_2016_rw$incidence_annual |>
    pivot_wider(names_from = quartile, values_from = value) |>
    filter(year > 2005)
hiv_2017 <- hiv_2017_rw$incidence_annual |>
    pivot_wider(names_from = quartile, values_from = value) |>
    filter(year > 2005)
hiv_2018 <- hiv_2018_rw$incidence_annual |>
    pivot_wider(names_from = quartile, values_from = value) |>
    filter(year > 2005)
hiv_2019 <- hiv_2019_rw$incidence_annual |>
    pivot_wider(names_from = quartile, values_from = value) |>
    filter(year > 2005)
# hiv_2020 <- hiv_2020_rw$incidence_annual |> pivot_wider(names_from = quartile, values_from = value)

## incidence plot
hiv_2014 |>
    ggplot() +
    aes(year, y = `50%`, ymin = `2.5%`, ymax = `97.5%`) +
    scale_y_continuous(limits = c(0, 5000), expand = expansion(mult = c(0, 0.05))) +
    geom_ribbon(data = hiv_2019, fill = "grey90", color = "grey40", alpha = 0.4, lty = "dashed") +
    geom_line(data = hiv_2019, colour = thesis_col[10]) +
    geom_ribbon(data = hiv_2018, fill = "grey90", color = "grey40", alpha = 0.4, lty = "dashed") +
    geom_line(data = hiv_2018, colour = thesis_col[10]) +
    geom_ribbon(data = hiv_2017, fill = "grey90", color = "grey40", alpha = 0.4, lty = "dashed") +
    geom_line(data = hiv_2017, colour = thesis_col[10]) +
    geom_ribbon(data = hiv_2016, fill = "grey90", color = "grey40", alpha = 0.4, lty = "dashed") +
    geom_line(data = hiv_2016, colour = thesis_col[10]) +
    geom_ribbon(data = hiv_2015, fill = "grey90", color = "grey40", alpha = 0.4, lty = "dashed") +
    geom_line(data = hiv_2015, colour = thesis_col[10]) +
    geom_ribbon(fill = "grey90", color = "grey40", alpha = 0.4, lty = "dashed") +
    geom_line(colour = thesis_col[10]) +
    theme_minimal(15) +
    geom_segment(data = hiv_2014 |> filter(year > 2012), aes(y = `2.5%`, yend = `97.5%`, x = 2013, xend = 2013), colour = thesis_col[4], linewidth = 0.7) +
    geom_segment(data = hiv_2015 |> filter(year > 2013), aes(y = `2.5%`, yend = `97.5%`, x = 2014, xend = 2014), colour = thesis_col[4], linewidth = 0.7) +
    geom_segment(data = hiv_2016 |> filter(year > 2014), aes(y = `2.5%`, yend = `97.5%`, x = 2015, xend = 2015), colour = thesis_col[4], linewidth = 0.7) +
    geom_segment(data = hiv_2017 |> filter(year > 2015), aes(y = `2.5%`, yend = `97.5%`, x = 2016, xend = 2016), colour = thesis_col[4], linewidth = 0.7) +
    geom_segment(data = hiv_2018 |> filter(year > 2016), aes(y = `2.5%`, yend = `97.5%`, x = 2017, xend = 2017), colour = thesis_col[4], linewidth = 0.7) +
    geom_segment(data = hiv_2019 |> filter(year > 2017), aes(y = `2.5%`, yend = `97.5%`, x = 2018, xend = 2018), colour = thesis_col[4], linewidth = 0.7) +
    scale_x_continuous(
        breaks = seq(2006, 2019, 2)
    ) +
    ylab("Estimated incidence of HIV") +
    xlab("Year")

ggsave(here("02_HIVBackCalc/Figs/incidence_overlay_2014_2018.pdf"), width = 10, height = 6)
ggsave(here("02_HIVBackCalc/Figs/incidence_overlay_2014_2018.png"), width = 10, height = 6)


hiv_2017 <- hiv_2017_rw_npd$incidence_annual |>
    pivot_wider(names_from = quartile, values_from = value) |>
    filter(year > 2009)
hiv_2018 <- hiv_2018_rw_npd$incidence_annual |>
    pivot_wider(names_from = quartile, values_from = value) |>
    filter(year > 2009)
hiv_2019 <- hiv_2019_rw_npd$incidence_annual |>
    pivot_wider(names_from = quartile, values_from = value) |>
    filter(year > 2009)
hiv_2020 <- hiv_2020_rw$incidence_annual |>
    pivot_wider(names_from = quartile, values_from = value) |>
    filter(year > 2009)
hiv_2021 <- hiv_2021_rw$incidence_annual |>
    pivot_wider(names_from = quartile, values_from = value) |>
    filter(year > 2009)
hiv_2022 <- hiv_2022_rw$incidence_annual |>
    pivot_wider(names_from = quartile, values_from = value) |>
    filter(year > 2009)
hiv_2023 <- hiv_2023_rw$incidence_annual |>
    pivot_wider(names_from = quartile, values_from = value) |>
    filter(year > 2009)

## incidence plot
hiv_2018 |>
    ggplot() +
    aes(year, y = `50%`, ymin = `2.5%`, ymax = `97.5%`) +
    scale_y_continuous(limits = c(0, 4000), expand = expansion(mult = c(0, 0.05))) +
    geom_ribbon(fill = "grey", alpha = 0.2) +
    geom_ribbon(data = hiv_2019, fill = "grey", alpha = 0.2) +
    geom_ribbon(data = hiv_2020, fill = "grey", alpha = 0.2) +
    geom_ribbon(data = hiv_2021, fill = "grey", alpha = 0.2) +
    geom_ribbon(data = hiv_2022, fill = "grey", alpha = 0.2) +
    geom_ribbon(data = hiv_2023, fill = "grey", alpha = 0.2) +
    geom_line(colour = thesis_col[11]) +
    geom_line(data = hiv_2019, colour = thesis_col[11]) +
    geom_line(data = hiv_2020, colour = thesis_col[11]) +
    geom_line(data = hiv_2021, colour = thesis_col[11]) +
    geom_line(data = hiv_2022, colour = thesis_col[11]) +
    geom_line(data = hiv_2023, colour = thesis_col[11]) +
    theme_minimal(15) +
    geom_segment(data = hiv_2018 |> filter(year > 2016), aes(y = `2.5%`, yend = `97.5%`, x = 2017, xend = 2017), colour = thesis_col[4], linewidth = 1) +
    geom_segment(data = hiv_2019 |> filter(year > 2017), aes(y = `2.5%`, yend = `97.5%`, x = 2018, xend = 2018), colour = thesis_col[4], linewidth = 1) +
    geom_segment(data = hiv_2020 |> filter(year > 2018), aes(y = `2.5%`, yend = `97.5%`, x = 2019, xend = 2019), colour = thesis_col[4], linewidth = 1) +
    geom_segment(data = hiv_2021 |> filter(year > 2019), aes(y = `2.5%`, yend = `97.5%`, x = 2020, xend = 2020), colour = thesis_col[4], linewidth = 1) +
    geom_segment(data = hiv_2022 |> filter(year > 2020), aes(y = `2.5%`, yend = `97.5%`, x = 2021, xend = 2021), colour = thesis_col[4], linewidth = 1) +
    geom_segment(data = hiv_2023 |> filter(year > 2021), aes(y = `2.5%`, yend = `97.5%`, x = 2022, xend = 2022), colour = thesis_col[4], linewidth = 1) +
    scale_x_continuous(
        breaks = seq(2010, 2022, 2)
    ) +
    ylab("Estimated incidence of HIV") +
    xlab("Year")

ggsave(here("02_HIVBackCalc/Figs/incidence_overlay_2017_2022.pdf"), width = 10, height = 6)
ggsave(here("02_HIVBackCalc/Figs/incidence_overlay_2017_2022.png"), width = 10, height = 6)

###################
# Simulated data
###################
load(here("data/hiv_sim_estimates.RData"))

lambda_sim <- inf_lambda |>
    ggplot() +
    aes(x, y, colour = group) +
    geom_line(linewidth = 1) +
    geom_point(data = inf_poisson) +
    theme_minimal(15) +
    labs(y = expression(paste("Values of ", lambda)), x = "Quarter", colour = "") +
    scale_y_continuous(limits = c(0, 700), expand = expansion(mult = c(0, 0.05))) +
    scale_x_continuous(breaks = seq(0, 134, 20)) +
    theme(panel.grid.minor = element_blank()) +
    scale_colour_manual(values = c(thesis_col[1], thesis_col[9]), labels = label_parse()) +
    guides(color = guide_legend(override.aes = list(linetype = c(1, 0), shape = c(32, 16)))) +
    theme(legend.position = "bottom")

diag_prob_sim <- diag_mu |>
    pivot_longer(names_to = "strata", values_to = "value", -c(x, group)) |>
    mutate(strata = factor(strata, labels = c("d[1]", "d[2]", "d[3]", "d[4]", "d[5]"))) |>
    ggplot() +
    aes(x, value, colour = group) +
    geom_line(linewidth = 1) +
    geom_point(data = diag_normal) +
    theme_minimal(15) +
    labs(y = expression(paste("Values of ", delta)), x = "Quarter", colour = "") +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    scale_x_continuous(breaks = seq(0, 134, 40)) +
    theme(panel.grid.minor = element_blank()) +
    scale_colour_manual(values = c(thesis_col[1], thesis_col[9]), labels = label_parse()) +
    guides(color = guide_legend(override.aes = list(linetype = c(1, 0), shape = c(32, 16)))) +
    theme(legend.position = "bottom") +
    facet_row(~strata, labeller = label_parsed)

(lambda_sim / diag_prob_sim) + plot_annotation(tag_levels = "A")

ggsave(here("02_HIVBackCalc/Figs/lambda_sim.pdf"), width = 10, height = 13)
ggsave(here("02_HIVBackCalc/Figs/lambda_sim.png"), width = 10, height = 13)

# plot data for boxplots
plot_data <- bind_rows(
    misclass_incidence |> mutate(group = "CD4-only model"),
    rita_incidence |> mutate(group = "Dual biomarker\nmodel"),
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

ggsave(here("02_HIVBackCalc/Figs/incidence_boxplot_sim_year.pdf"), width = 10, height = 13)
ggsave(here("02_HIVBackCalc/Figs/incidence_boxplot_sim_year.png"), width = 10, height = 13)

p1 <- box_plot(plot_data)

p2 <- mse_plot(mse)

p1 + p2

ggsave(here("02_HIVBackCalc/Figs/incidence_mse_sim.pdf"), width = 10, height = 6)
ggsave(here("02_HIVBackCalc/Figs/incidence_mse_sim.png"), width = 10, height = 6)

# figures
p1 <- bind_rows(
    misclass_undiag_prev |> group_by(year, quartile) |> summarise(value = median(value)) |> mutate(group = "CD4-only model"),
    rita_undiag_prev |> group_by(year, quartile) |> summarise(value = median(value)) |> mutate(group = "Dual biomarker model")
) |> bc_ribbon_plot(point_data = undiag_prev_par, xlab = "Quarter")

p2 <- bind_rows(
    misclass_incidence |> group_by(year, quartile) |> filter(year > 0) |> summarise(value = median(value)) |> mutate(group = "CD4-only model"),
    rita_incidence |> group_by(year, quartile) |> filter(year > 0) |> summarise(value = median(value)) |> mutate(group = "Dual biomarker model")
) |> bc_ribbon_plot(point_data = incidence_par, xlab = "Quarter", ylab = "Estimated HIV incidence")

p3 <- bind_rows(
    misclass_diag_prob |> group_by(year, quartile, strata) |> summarise(value = median(value)) |> mutate(group = "CD4-only model"),
    rita_diag_prob |> group_by(year, quartile, strata) |> summarise(value = median(value)) |> mutate(group = "Dual biomarker model")
) |> bc_diag_prob_plot(point_data = diag_prob_par)

(p2 + p1) / p3 + plot_annotation(tag_levels = "A")

ggsave(here("02_HIVBackCalc/Figs/sim_estimates.pdf"), width = 10, height = 13)
ggsave(here("02_HIVBackCalc/Figs/sim_estimates.png"), width = 10, height = 13)

bind_rows(
    misclass_diagnoses |> group_by(year, quartile) |> filter(year > 0) |> summarise(value = median(value)) |> mutate(group = "CD4-only model"),
    rita_diagnoses |> group_by(year, quartile) |> filter(year > 0) |> summarise(value = median(value)) |> mutate(group = "Dual biomarker model")
) |> bc_ribbon_plot(point_data = diagnoses_par, xlab = "Quarter", legend_pos = "bottom", ylab = "Estimated HIV diagnoses")

ggsave(here("02_HIVBackCalc/Figs/diag_fit_sim.pdf"), width = 10, height = 6)
ggsave(here("02_HIVBackCalc/Figs/diag_fit_sim.png"), width = 10, height = 6)


###################
# RITA data
###################
# RITA + CD4 back-calculation compared to CD4-only back-calculation

load(here("data/hiv_rita_estimates.RData"))

# HIV diagnoses
hiv_dx <- tibble(
    value = stan_data$HIV + stan_data$AIDS,
    year = seq(2011, 2019.75, by = 0.25),
    group = "Observed HIV diagnoses"
)

# figures
p4 <- bind_rows(
    hiv_2019_rita$undiag_prev |> filter(year >= 2011) |> mutate(group = "Dual biomarker model"),
    hiv_2019_spl$undiag_prev |> filter(year >= 2011) |> mutate(group = "CD4-only model")
) |> bc_ribbon_plot() +
    scale_x_continuous(limits = c(2011.25, NA), breaks = seq(2011, 2019, 2))


p5 <- bind_rows(
    hiv_2019_rita$incidence |> filter(year >= 2011.25) |> mutate(group = "Dual biomarker model"),
    hiv_2019_spl$incidence |> filter(year >= 2011.25) |> mutate(group = "CD4-only model"),
) |> bc_ribbon_plot(, ylab = "Estimated HIV incidence") +
    scale_x_continuous(limits = c(2011.25, NA), breaks = seq(2011, 2019, 2))

p6 <- bind_rows(
    hiv_2019_rita$diag_prob |> filter(year >= 2011) |> mutate(group = "Dual biomarker model"),
    hiv_2019_spl$diag_prob |> filter(year >= 2011) |> mutate(group = "CD4-only model")
) |> bc_diag_prob_plot() +
    scale_x_continuous(limits = c(2011.25, NA), breaks = seq(2011, 2019, 2))

(p5 + p4) / p6 + plot_annotation(tag_levels = "A")

ggsave(here("02_HIVBackCalc/Figs/rita_estimates.pdf"), width = 10, height = 13)
ggsave(here("02_HIVBackCalc/Figs/rita_estimates.png"), width = 10, height = 13)

bind_rows(
    hiv_2019_rita$diagnoses |> filter(year >= 2011.25) |> mutate(group = "Dual biomarker model"),
    hiv_2019_spl$diagnoses |> filter(year >= 2011.25) |> mutate(group = "CD4-only model")
) |> bc_ribbon_plot(point_data = hiv_dx, ylab = "Estimated HIV diagnoses", legend_pos = "bottom") +
    scale_x_continuous(limits = c(2011.25, NA), breaks = seq(2011, 2019, 2))

ggsave(here("02_HIVBackCalc/Figs/diag_fit_rita.pdf"), width = 10, height = 6)
ggsave(here("02_HIVBackCalc/Figs/diag_fit_rita.png"), width = 10, height = 6)


# MPES

mpes_estimates <- tribble(
    ~year, ~value, ~ymin, ~ymax,
    2013.5, 7093, 4021, 13188,
    2014.5, 6406, 3656, 12022,
    2015.5, 5384, 3011, 10453,
    2016.5, 4409, 2530, 8448,
    2017.5, 3715, 2059, 7200,
    2018.5, 3606, 2059, 6761, # <- check this with Anne
    2019.5, 2885, 1649, 5302
) |> mutate(group = "MPES estimates")

# figures
bind_rows(
    hiv_2019_rita$undiag_prev |> filter(year >= 2011) |> mutate(group = "Dual biomarker model"),
    hiv_2019_spl$undiag_prev |> filter(year >= 2011) |> mutate(group = "CD4-only model"),
) |> bc_ribbon_plot(legend_pos = "bottom", point_col = "black", line_col = c(thesis_col[c(1, 4)], "black")) +
    scale_x_continuous(limits = c(2011.25, NA), breaks = seq(2011, 2022, 2)) +
    # add mpes estimates
    geom_errorbar(data = mpes_estimates, aes(y = value, ymin = ymin, ymax = ymax), width = 0.5) +
    geom_point(data = mpes_estimates, aes(y = value), colour = "black", fill = "white")

ggsave(here("02_HIVBackCalc/Figs/undiag_prev_mpes.pdf"), width = 10, height = 6)
ggsave(here("02_HIVBackCalc/Figs/undiag_prev_mpes.png"), width = 10, height = 6)

# forecast incidence

load(here("data/hiv_forecast.RData"))

# probably compare 2018ad to 2022ai

p1 <- projection_plot(projection_2018)
p2 <- projection_plot(projection_2022)

p1 / p2 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect", axis_titles = "collect_x") & theme(legend.position = "bottom")

ggsave(here("02_HIVBackCalc/Figs/projected_incidence.pdf"), width = 10, height = 13)
ggsave(here("02_HIVBackCalc/Figs/projected_incidence.png"), width = 10, height = 13)
