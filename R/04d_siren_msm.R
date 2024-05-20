# Figures and tables for Chapter 4

# Load libraries
here::i_am("R/04d_siren_msm.R")

library(here)
library(msm)
library(tidyverse)
library(survival)
library(finalfit)
library(scales)

# load the various datasets
load(here("Data/SIREN/siren_processed_interim4_vax_final.RData"))
load(here("Data/SIREN/siren_cox_interim4_vax_final.RData"))
load(here("Data/siren_models.RData"))
load(here("Data/siren_hazards.RData"))
load(here("Data/siren_diagnostics.RData"))
load(here("Data/siren_sojourn_times.RData"))

# Cox models
dependent <- "Surv(tstart, tstop, event)"

# vaccine short effect
explanatory_multi <- c("months_since_pos*vaccine_short", "gender", "strata(occupation_setting)", "strata(agegr)", "household", "strata(region)", "cluster(trust_code)")
cox_short <- siren_split |>
    filter(months_since_pos != "No evidence of infection") |>
    coxphmulti(dependent, explanatory_multi)

explanatory_multi <- c("vaccine_short", "gender", "strata(occupation_setting)", "strata(agegr)", "household", "strata(region)", "cluster(trust_code)")
cox_short_nomo <- siren_split |>
    coxphmulti(dependent, explanatory_multi)

# vaccine long effect
explanatory_multi <- c("vaccine", "gender", "strata(occupation_setting)", "strata(agegr)", "household", "strata(region)", "cluster(trust_code)")
cox_long_nomo <- siren_split |>
    coxphmulti(dependent, explanatory_multi)

# interaction effect
explanatory_multi <- c("months_since_pos", "months_since_pos:vaccine_short", "gender", "strata(occupation_setting)", "strata(agegr)", "household", "strata(region)", "cluster(trust_code)")
cox_interaction <- siren_split |>
    filter(months_since_pos != "No evidence of infection") |>
    coxphmulti(dependent, explanatory_multi)

# forest plots
forest_plot(mllvrahgo, cox_fit = cox_long_nomo, covars = c("vaccine", "gender", "household"))

ggsave(here("04_SIREN/Figs/vaccine_long.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/vaccine_long.png"), width = 10, height = 6)

forest_plot(msvrahgo, cox_fit = cox_short_nomo, covars = c("vaccine_short", "household", "gender"))

ggsave(here("04_SIREN/Figs/vaccine_short.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/vaccine_short.png"), width = 10, height = 6)

forest_plot(mmsvrahgo_noev, cox_fit = cox_short, covars = c("vaccine_short", "months_since_pos", "gender", "household", "months_since_pos:vaccine_short"))

ggsave(here("04_SIREN/Figs/vaccine_short_noev.pdf"), width = 10, height = 13)
ggsave(here("04_SIREN/Figs/vaccine_short_noev.png"), width = 10, height = 13)

forest_plot(mmsvrahgo_interaction_noev, cox_fit = cox_interaction, covars = c("months_since_pos", "months_since_pos:vaccine_short", "gender", "household"))

ggsave(here("04_SIREN/Figs/vaccine_interaction_noev.pdf"), width = 10, height = 13)
ggsave(here("04_SIREN/Figs/vaccine_interaction_noev.png"), width = 10, height = 13)

# symptomatic forest plots

forest_plot(msvrahgo_sym, covars = "vaccine_short", transition2 = "State 1 - State 3")

ggsave(here("04_SIREN/Figs/vaccine_short_sym.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/vaccine_short_sym.png"), width = 10, height = 6)

forest_plot(mllvrahgo_sym, covars = "vaccine", transition2 = "State 1 - State 3")

ggsave(here("04_SIREN/Figs/vaccine_long_sym.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/vaccine_long_sym.png"), width = 10, height = 6)

forest_plot(mmsvrahgo_sym, covars = c("vaccine_short", "months_since_pos"), transition2 = "State 1 - State 3")

ggsave(here("04_SIREN/Figs/vaccine_msp_short_sym.pdf"), width = 10, height = 13)
ggsave(here("04_SIREN/Figs/vaccine_msp_short_sym.png"), width = 10, height = 13)

forest_plot(mmllvrahgo_sym, covars = c("vaccine", "months_since_pos"), transition2 = "State 1 - State 3")

ggsave(here("04_SIREN/Figs/vaccine_msp_long_sym.pdf"), width = 10, height = 13)
ggsave(here("04_SIREN/Figs/vaccine_msp_long_sym.png"), width = 10, height = 13)

# forest plots from paper
p1 <- ve_plot(
    hazards_mllvrahgo |>
        filter(name %in% c("Waned\nthird\ndose", "Fourth\ndose")),
    colour = thesis_col[2]
)

p2 <- ve_plot(
    hazards_mllvrahgo |>
        filter(name %in% c("Fourth dose\n0-2 months", "Fourth dose\n2-4 months", "Fourth dose\n4-6 months")),
    colour = thesis_col[2],
    xlab = "Time since booster vaccination"
)

p3 <- ve_plot(
    hazards_mmsvrahgo_noev,
    colour = thesis_col[10],
    xlab = "Time since previous infection",
    ylab = "Estimated protection"
)

#(p1 + p2 + plot_layout(widths = c(1, 2))) / p3 + plot_annotation(tag_levels = "A")

# Symptomatic plots
p4 <- ve_plot_sym(
    hazards_msvrahgo_sym,
    colour = value_id
)

p5 <- ve_plot_sym(
    hazards_mllvrahgo_sym |> filter(name %in% c("Fourth dose\n0-2 months", "Fourth dose\n2-4 months", "Fourth dose\n4-6 months")),
    xlab = "Time since booster vaccination",
    colour = value_id
)

p6 <- ve_plot_sym(
    hazards_mmsvrahgo_sym,
    colour = value_id,
    xlab = "Time since previous infection",
    ylab = "Estimated protection"
)

ve1 <- p1 + p2 + p3 + plot_layout(widths = c(2, 5, 5), axes = "collect") & theme(axis.title.x = element_blank())
ve2 <- p4 + p5 + p6 + plot_layout(widths = c(2, 5, 5), axes = "collect", guides = "collect") & theme(legend.position = "bottom")

ve1 / ve2 + plot_annotation(tag_levels = "A")

ggsave(here("04_SIREN/Figs/ve_long.pdf"), width = 10, height = 13)
ggsave(here("04_SIREN/Figs/ve_long.png"), width = 10, height = 13)


ve_plot(
    hazards_mmsvrahgo_interaction_noev,
    colour = thesis_col[12],
    xlab = "Time since previous infection",
    breaks = c(-1, -0.5, 0, 0.5, 1),
    ylim = c(-1, 1)
)

ggsave(here("04_SIREN/Figs/ve_rel.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/ve_rel.png"), width = 10, height = 6)

# sojourn times
p0 <- sojourn_plot(
    sojourn_times |> filter(cat == "Whole population"),
    xlab = "",
    colour = thesis_col[4]
)

p1 <- sojourn_plot(
    sojourn_times |> filter(cat %in% c("Waned third dose", "Fourth dose")),
    colour = thesis_col[2]
)

p2 <- sojourn_plot(
    sojourn_times |> filter(cat %in% c("Confirmed naive", "2+ years", "1-2 years", "6-12 months", "0-6 months", "Naive")),
    xlab = "Time since previous infection",
    colour = thesis_col[10]
)

p3 <- sojourn_plot_sym(sojourn_times, colour = cat)

p0 / p1 / p2 / p3 + plot_layout(heights = c(0.5, 1, 2, 1), axis_titles = "collect") + plot_annotation(tag_levels = "A")

ggsave(here("04_SIREN/Figs/sojourn_time.pdf"), width = 10, height = 13)
ggsave(here("04_SIREN/Figs/sojourn_time.png"), width = 10, height = 13)

# model diagnostics
diagnostics_plot(diagnostics) + theme(plot.subtitle = element_blank())

ggsave(here("04_SIREN/Figs/model_diagnostics.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/model_diagnostics.png"), width = 10, height = 6)

# cox models
cox_1 <- tribble(
    ~days, ~est, ~lower, ~upper, ~group,
    "14-73 days", 0.85, 0.72, 0.92, "BNT162b2, long interval",
    "74-133 days", 0.66, 0.53, 0.75, "BNT162b2, long interval",
    "134-193 days", 0.68, 0.54, 0.77, "BNT162b2, long interval",
    "194-239 days", 0.51, 0.22, 0.69, "BNT162b2, long interval",
    "14-73 days", 0.89, 0.78, 0.94, "BNT162b2, short interval",
    "74-133 days", 0.58, 0.18, 0.79, "BNT162b2, short interval",
    "134-193 days", 0.50, 0.26, 0.67, "BNT162b2, short interval",
    "194-265 days", 0.53, 0.28, 0.69, "BNT162b2, short interval",
    "14-73 days", 0.58, 0.23, 0.77, "ChAdOx1",
    "74-133 days", 0.50, 0.29, 0.65, "ChAdOx1",
    "134-220 days", 0.72, 0.39, 0.87, "ChAdOx1"
) |>
    mutate(
        days = factor(days, levels = c("14-73 days", "74-133 days", "134-193 days", "194-239 days", "194-265 days", "134-220 days")),
        group = factor(group, levels = c("BNT162b2, long interval", "BNT162b2, short interval", "ChAdOx1"))
    )

cox_1 |>
    ggplot() +
    aes(x = days, y = est, ymin = lower, ymax = upper, colour = group) +
    geom_errorbar(width = 0.4) +
    geom_point() +
    scale_colour_manual(values = thesis_col[seq(2, length(thesis_col), 5)]) +
    scale_y_continuous(
        labels = percent,
        limits = c(0, 1),
        expand = c(0, 0)
    ) +
    theme_minimal(15) +
    labs(
        x = "Time since second dose (days)",
        y = "Adjusted vaccine effectiveness",
        colour = "Vaccine"
    ) +
    theme(
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10)),
        # rotate x labels
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    ) +
    facet_grid(~group, scales = "free_x", space = "free_x")

ggsave(here("04_SIREN/Figs/cox_1.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/cox_1.png"), width = 10, height = 6)

cox_2 <- tribble(
    ~days, ~est, ~lower, ~upper, ~group, ~period,
    "0-2 months", 0.63, 0.40, 0.77, "BNT162b2 primary course", "Delta",
    "0-2 months", 0.77, 0.37, 0.91, "ChAdOx1 primary course", "Delta",
    "0-2 months", 0.35, 0.21, 0.47, "BNT162b2 primary course", "Omicron",
    "2-4 months", 0.34, 0.22, 0.43, "BNT162b2 primary course", "Omicron",
    ">4 months", 0.21, 0.00, 0.38, "BNT162b2 primary course", "Omicron",
    "0-2 months", 0.68, 0.46, 0.81, "ChAdOx1 primary course", "Omicron",
    "2-4 months", 0.56, 0.32, 0.72, "ChAdOx1 primary course", "Omicron",
    ">4 months", 0.56, 0.32, 0.72, "ChAdOx1 primary course", "Omicron"
) |>
    mutate(
        days = factor(days, levels = c("0-2 months", "2-4 months", ">4 months")),
        group = factor(group, levels = c("BNT162b2 primary course", "ChAdOx1 primary course")),
        period = factor(period, levels = c("Delta", "Omicron"))
    )

cox_3 <- tribble(
    ~days, ~est, ~lower, ~upper, ~period,
    "3-6 months", 0.90, 0.42, 0.98, "Delta",
    "6-9 months", 0.75, 0.59, 0.85, "Delta",
    "9-12 months", 0.63, 0.48, 0.74, "Delta",
    "12-15 months", 0.93, 0.72, 0.98, "Delta",
    ">15 months", 0.85, 0.81, 0.89, "Delta",
    "3-6 months", 0.67, 0.56, 0.75, "Omicron",
    "6-9 months", 0.53, 0.20, 0.73, "Omicron",
    "9-12 months", 0.27, 0.04, 0.44, "Omicron",
    "12-15 months", 0.27, 0.16, 0.37, "Omicron",
    ">15 months", 0.26, 0.17, 0.34, "Omicron"
) |>
    mutate(
        days = factor(days, levels = c("3-6 months", "6-9 months", "9-12 months", "12-15 months", ">15 months")),
        period = factor(period, levels = c("Delta", "Omicron"))
    )

p1 <- cox_2 |>
    ggplot() +
    aes(x = days, y = est, ymin = lower, ymax = upper, colour = group) +
    geom_errorbar(width = 0.4, position = position_dodge(width = 0.5)) +
    geom_point(position = position_dodge(width = 0.5)) +
    scale_colour_manual(values = thesis_col[seq(4, length(thesis_col), 6)]) +
    scale_y_continuous(
        labels = percent,
        limits = c(0, 1),
        expand = c(0.01, 0)
    ) +
    theme_minimal(15) +
    labs(
        x = "Time since third dose (months)",
        y = "Adjusted vaccine effectiveness",
        colour = "Vaccine"
    ) +
    theme(
        legend.position = "top",
        panel.grid.major.x = element_blank(),
        # rotate x labels
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10))
    ) +
    facet_grid(~period, scales = "free_x", space = "free_x") +
    guides(colour = guide_legend(nrow = 2))

p2 <- cox_3 |>
    ggplot() +
    aes(x = days, y = est, ymin = lower, ymax = upper, colour = period) +
    geom_errorbar(width = 0.4, position = position_dodge(width = 0.5)) +
    geom_point(position = position_dodge(width = 0.5)) +
    scale_colour_manual(values = thesis_col[seq(6, length(thesis_col), 6)]) +
    scale_y_continuous(
        labels = percent,
        limits = c(0, 1),
        expand = c(0.01, 0)
    ) +
    theme_minimal(15) +
    labs(
        x = "Time since previous infection (months)",
        y = "Estimated protection",
        colour = "Variant circulating period"
    ) +
    theme(
        legend.position = "top",
        panel.grid.major.x = element_blank(),
        # rotate x labels
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10))
    ) +
    guides(colour = guide_legend(nrow = 2))

p1 + p2 + plot_annotation(tag_levels = "A") + plot_layout(widths = c(4, 5)) #, guides = "collect") & theme(legend.position = "bottom", legend.direction = "vertical")

ggsave(here("04_SIREN/Figs/cox_2.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/cox_2.png"), width = 10, height = 6)
