# Load libraries
here::i_am("R/04x_siren_appendix.R")

library(here)
library(msm)
library(tidyverse)
library(survival)
library(finalfit)
library(scales)

load(here("Data/SIREN/msm_models_interim4_convalesce.RData"))
load(here("Data/SIREN/msm_models_interim4_convalesce_2.RData"))
load(here("Data/SIREN/msm_models_interim4_misclass.RData"))
load(here("Data/siren_hazards.RData"))

hazards_conv_msp <- forest_plot(mmsvrahgo_noev, table = TRUE, covars = "months_since_pos") |>
    mutate(
        est = 1 - est,
        lower = 1 - lower,
        upper = 1 - upper,
        name = factor(name,
            levels = c("Confirmed naive", "2+ years", "1-2 years", "6-12 months", "0-6 months"),
            labels = c("Confirmed\nnaive", "2+\nyears", "1-2\nyears", "6-12\nmonths", "0-6\nmonths")
        )
    )

hazards_conv_vacc <- forest_plot(msvrahgo, table = TRUE, covars = "vaccine_short") |>
    mutate(
        est = 1 - est,
        lower = 1 - lower,
        upper = 1 - upper,
        name = factor(name,
            levels = c("Waned third dose", "Fourth dose"),
            labels = c("Waned\nthird\ndose", "Fourth\ndose")
        )
    )

p1 <- bind_rows(
    hazards_mllvrahgo |> filter(name %in% c("Waned\nthird\ndose", "Fourth\ndose")) |> mutate(model = "SI"),
    hazards_conv_vacc |> mutate(model = "SIR")
) |>
    ve_plot_sym(
        colour = model,
        ylim = c(-0.5, 1),
        colour_lab = "",
        colour_levels = c("Two-state model", "Three-state model"),
        colour_vals = rev(thesis_col[seq(4, length(thesis_col), 5)])
    )

p2 <- bind_rows(
    hazards_mmsvrahgo_noev |> mutate(model = "SI"),
    hazards_conv_msp |> mutate(model = "SIR")
) |>
    ve_plot_sym(
        colour = model,
        xlab = "Time since previous infection",
        ylab = "Estimated protection",
        ylim = c(-0.5, 1),
        colour_lab = "",
        colour_levels = c("Two-state model", "Three-state model"),
        colour_vals = rev(thesis_col[seq(4, length(thesis_col), 5)])
    )

p1 + p2 + plot_annotation(tag_levels = "A") + plot_layout(widths = c(2, 5), axes = "collect", guides = "collect") & theme(legend.position = "bottom")

ggsave(here("04_SIREN/Figs/ve_convalescent.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/ve_convalescent.png"), width = 10, height = 6)


hazards_misclass_msp <- forest_plot(mmsvrahgo_misclass, table = TRUE, covars = "months_since_pos") |>
    mutate(
        est = 1 - est,
        lower = 1 - lower,
        upper = 1 - upper,
        name = factor(name,
            levels = c("Confirmed naive", "2+ years", "1-2 years", "6-12 months", "0-6 months"),
            labels = c("Confirmed\nnaive", "2+\nyears", "1-2\nyears", "6-12\nmonths", "0-6\nmonths")
        )
    )

hazards_misclass_vacc <- forest_plot(msvrahgo_misclass, table = TRUE, covars = "vaccine_short") |>
    mutate(
        est = 1 - est,
        lower = 1 - lower,
        upper = 1 - upper,
        name = factor(name,
            levels = c("Waned third dose", "Fourth dose"),
            labels = c("Waned\nthird\ndose", "Fourth\ndose")
        )
    )

p1 <- bind_rows(
    hazards_mllvrahgo |> filter(name %in% c("Waned\nthird\ndose", "Fourth\ndose")) |> mutate(model = "Two-state model"),
    hazards_misclass_vacc |> mutate(model = "Misclassification model")
) |>
    mutate(model = factor(model, levels = c("Two-state model", "Misclassification model"))) |>
    ve_plot_sym(
        colour = model,
        ylim = c(-0.5, 1),
        colour_lab = "",
        colour_levels = c("Two-state model", "Misclassification model"),
        colour_vals = rev(thesis_col[seq(3, length(thesis_col), 5)])
    )

p2 <- bind_rows(
    hazards_mmsvrahgo_noev |> mutate(model = "Two-state model"),
    hazards_misclass_msp |> mutate(model = "Misclassification model")
) |>
    mutate(model = factor(model, levels = c("Two-state model", "Misclassification model"))) |>
    ve_plot_sym(
        colour = model,
        xlab = "Time since previous infection",
        ylab = "Estimated protection",
        ylim = c(-0.5, 1),
        colour_lab = "",
        colour_levels = c("Two-state model", "Misclassification model"),
        colour_vals = rev(thesis_col[seq(3, length(thesis_col), 5)])
    )

p1 + p2 + plot_annotation(tag_levels = "A") + plot_layout(widths = c(2, 5), axes = "collect", guides = "collect") & theme(legend.position = "bottom")

ggsave(here("04_SIREN/Figs/ve_misclass.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/ve_misclass.png"), width = 10, height = 6)


load(here("Data/SIREN/msm_models_interim4_noev_susan.RData"))
load(here("Data/SIREN/msm_models_interim4_sym_susan.RData"))
load(here("Data/siren_seq_df.RData"))

hazards <- forest_plot(mmsvrahgo_noev, table = TRUE, covars = "months_since_pos") |>
    mutate(
        est = 1 - est,
        lower = 1 - lower,
        upper = 1 - upper,
        name = factor(name,
            levels = c("Confirmed naive", "2+ years", "1-2 years", "6-12 months", "0-6 months"),
            labels = c("Confirmed\nnaive", "2+\nyears", "1-2\nyears", "6-12\nmonths", "0-6\nmonths")
        )
    )

p3 <- ve_plot(
    hazards,
    colour = thesis_col[10],
    xlab = "Time since previous infection",
    ylab = "Estimated protection",
    ylim = c(-0.55, 1)
)

# Symptomatic plots
hazards <- forest_plot(mmsvrahgo_sym, table = TRUE, transition2 = "State 1 - State 3", covars = "months_since_pos") |>
    mutate(
        est = 1 - est,
        lower = 1 - lower,
        upper = 1 - upper,
        name = factor(name,
            levels = c("Confirmed naive", "2+ years", "1-2 years", "6-12 months", "0-6 months"),
            labels = c("Confirmed\nnaive", "2+\nyears", "1-2\nyears", "6-12\nmonths", "0-6\nmonths")
        )
    )

p4 <- ve_plot_sym(
    hazards,
    colour = value_id,
    xlab = "Time since previous infection",
    ylab = "Estimated protection",
    ylim = c(-1, 1)
)

p3 + p4 + plot_annotation(tag_levels = "A") + plot_layout(widths = c(5, 5), axes = "collect", guides = "collect") & theme(legend.position = "bottom")

ggsave(here("04_SIREN/Figs/ve_baseline.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/ve_baseline.png"), width = 10, height = 6)


seq_df |>
    ggplot() +
    aes(
        y = fct_rev(variant_name),
        x = n,
        fill = variant_name,
        label = pct
    ) +
    geom_col() +
    geom_text(
        data = seq_df |> filter(n > 100),
        aes(label = paste0(round(pct * 100, 1), "%")),
        size = 5,
        hjust = 1.2,
        color = "white",
        fontface = "bold"
    ) +
    geom_text(
        data = seq_df |> filter(n < 100),
        aes(label = paste0(round(pct * 100, 1), "%")),
        size = 5,
        hjust = -0.2,
        color = "black"
    ) +
    labs(
        y = "",
        x = "Number of sequenced PCR tests"
    ) +
    theme_minimal(14) +
    scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150)) +
    scale_fill_manual(values = thesis_col[seq(1, length(thesis_col), 1)]) +
    theme(
        legend.position = "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank()
    )

ggsave(here("04_SIREN/Figs/interim4_sequences.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/interim4_sequences.png"), width = 10, height = 6)

load(here("Data/SIREN/msm_models_semi_markov.RData"))
load(here("Data/siren_sojourn_times.RData"))

semi_markov <- sojourn.msm(msvga_semi_markov)

sojourn_times <- sojourn_times |>
    bind_rows(
        tibble(
            cat = c("Short stay mean", "Long stay mean"),
            sojourn = semi_markov$estimates[2:3] * 7,
            lower = semi_markov$L[2:3] * 7,
            upper = semi_markov$U[2:3] * 7
        )
    )

p0 <- sojourn_plot(
    sojourn_times |> filter(cat == "Whole population"),
    xlab = "",
    colour = thesis_col[4],
    ylims = c(0.7, 13.5),
    breaks = c(1, 3, 5, 7, 9, 11),
    texty = 12.7
) + theme(axis.title.x = element_text(margin = margin(t = 10), hjust = 0.4))

p1 <- sojourn_plot(
    sojourn_times |> filter(cat %in% c("Short stay mean", "Long stay mean")),
    xlab = "",
    colour = thesis_col[2],
    ylims = c(0.7, 13.5),
    breaks = c(1, 3, 5, 7, 9, 11),
    texty = 12.7
) + theme(axis.title.x = element_text(margin = margin(t = 10), hjust = 0.4))

p0 / p1 + plot_layout(heights = c(1, 2), axis_titles = "collect") + plot_annotation(tag_levels = "A")

ggsave(here("04_SIREN/Figs/sojourn_semi_markov.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/sojourn_semi_markov.png"), width = 10, height = 6)
