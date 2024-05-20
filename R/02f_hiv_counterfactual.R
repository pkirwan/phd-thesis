# HIV counterfactuals

here::i_am("R/02f_hiv_counterfactual.R")

# Load libraries
library(here)
library(tidyverse)

# load data
load(here("Data/HIV/ProcOutput_ai_1995_2021_data6_cf.RData"))
load(here("Data/HIV/ProcOutput_ai_1995_2022_data6.RData"))
load(here("Data/HIV/model_ai_1995_2021_data6.RData"))

# generate a tibble containing the HIV diagnosis data
hiv_dx <- tibble(
    value = stan_data$HIV + stan_data$AIDS,
    year = seq(1995, 2021.75, by = 0.25),
    group = "Observed HIV diagnoses"
) |> filter(year > 2015.75)

hiv_dx |>
    filter(year > 2017.75) |>
    mutate(
        lockdown = case_when(
            year %in% c(2020.25, 2020.5, 2020.75, 2021, 2021.25, 2021.5, 2021.75) ~ "Lockdown restrictions and other measures in place",
            TRUE ~ "Period before lockdown restrictions"
        ),
        lockdown = factor(lockdown, levels = c(
            "Period before lockdown restrictions",
            "Lockdown restrictions and other measures in place"
        ))
    ) |>
    ggplot() +
    aes(x = year, y = value, fill = lockdown) +
    geom_col() +
    labs(x = "Year", y = "Observed HIV diagnoses", fill = "") +
    theme_minimal(15) +
    scale_x_continuous(breaks = seq(2018, 2021, by = 1)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_fill_manual(values = thesis_col[seq(1, length(thesis_col), 4)])

ggsave(here("02_HIVBackCalc/Figs/lockdown_diagnoses.pdf"), width = 10, height = 6)
ggsave(here("02_HIVBackCalc/Figs/lockdown_diagnoses.png"), width = 10, height = 6)


# figures
p1 <- incidence_plt_2022_spl <- bind_rows(
    hiv_2022_spl$incidence |> rename(year = quarter) |> filter(year > 2015.75) |> mutate(group = " Unmodified model") |> filter(quartile == "50%"),
    hiv_2021_spl_cf1$incidence |>  filter(year > 2015.75) |> mutate(group = "(a) Incidence unaffected"),
    hiv_2021_spl_cf2$incidence |>  filter(year > 2015.75) |> mutate(group = "(b) Diagnosis probabilities unaffected")
) |> bc_ribbon_plot(ylab = "Estimated HIV incidence")

p2 <- incidence_plt_2022_spl <- bind_rows(
    hiv_2022_spl$incidence |> rename(year = quarter) |> filter(year > 2015.75) |> mutate(group = " Unmodified model")
) |> bc_ribbon_plot(ylab = "Estimated HIV incidence")

p3 <- diag_prob_plt <- bind_rows(
    hiv_2022_spl$diag_prob |> rename(year = quarter) |> filter(year > 2015.75) |> mutate(group = " Unmodified model") |> filter(quartile == "50%"),
    hiv_2021_spl_cf1$diag_prob |>  filter(year > 2015.75) |> mutate(group = "(a) Incidence unaffected"),
    hiv_2021_spl_cf2$diag_prob |>  filter(year > 2015.75) |> mutate(group = "(b) Diagnosis probabilities unaffected")
) |> bc_diag_prob_plot(facet_rows = 1)

p4 <- diag_prob_plt <- bind_rows(
    hiv_2022_spl$diag_prob |> rename(year = quarter) |> filter(year > 2015.75) |> mutate(group = " Unmodified model")
) |> bc_diag_prob_plot(legend_pos = "none", facet_rows = 1)

(p2 + p1 + plot_layout(axis_titles = "collect_y")) / p4 / p3 / guide_area() + plot_annotation(tag_levels = "A") + plot_layout(heights = c(1, 1, 1, 0.1), guides = 'collect') & theme(legend.box = 'horizontal')

ggsave(here("02_HIVBackCalc/Figs/counterfactual_estimates.pdf"), width = 10, height = 13)
ggsave(here("02_HIVBackCalc/Figs/counterfactual_estimates.png"), width = 10, height = 13)

p5 <- bind_rows(
    hiv_2022_spl$diagnoses |> filter(year > 2015.75) |> mutate(group = " Unmodified model") |> filter(quartile == "50%"),
    hiv_2021_spl_cf1$diagnoses |> filter(year > 2015.75) |> mutate(group = "(a) Incidence unaffected"),
    hiv_2021_spl_cf2$diagnoses |> filter(year > 2015.75) |> mutate(group = "(b) Diagnosis probabilities unaffected")
) |> bc_ribbon_plot(point_data = hiv_dx, ylab = "Estimated HIV diagnoses", legend_pos = "bottom", point_col = thesis_col[3], lims = c(100, 640))

p6 <- bind_rows(
    hiv_2022_spl$diagnoses |> filter(year > 2015.75) |> mutate(group = " Unmodified model")
) |> bc_ribbon_plot(point_data = hiv_dx, ylab = "Estimated HIV diagnoses", legend_pos = "none", point_col = thesis_col[3], lims = c(100, 640))

(p6 + p5) / guide_area() + plot_annotation(tag_levels = "A") + plot_layout(heights = c(1, 0.05), guides = 'collect') & theme(legend.box = 'horizontal') & guides(fill = guide_legend(nrow = 2), colour = guide_legend(nrow = 2))

ggsave(here("02_HIVBackCalc/Figs/counterfactual_fit.pdf"), width = 10, height = 6)
ggsave(here("02_HIVBackCalc/Figs/counterfactual_fit.png"), width = 10, height = 6)
