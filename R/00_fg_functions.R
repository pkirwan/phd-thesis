# Functions for generating Fine-Gray estimate plots

library(tidyverse)
library(lubridate)
library(survival)
library(scales)

fg_plot <- function(tidy_fg_fit, labels, levels, var_group, plot_title = "") {
    labels |>
        full_join(
            tidy_fg_fit |>
                bind_cols(levels |> select(label)),
            by = "label"
        ) |>
        mutate(
            estimate = if_else(is.na(estimate), 1, estimate),
            label = factor(label, levels = labels$label),
            ref = if_else(estimate == 1, "ref", "not ref")
        ) |>
        filter(group %in% var_group) |>
        ggplot() +
        aes(y = label, x = estimate, xmin = conf.low, xmax = conf.high, color = group) +
        geom_point(size = 2) +
        geom_errorbarh(height = .3, linewidth = 0.9) +
        scale_x_continuous(
            name = "Hazard ratio", trans = "log10",
            breaks = scales::pretty_breaks()
        ) +
        scale_y_discrete(limits = rev, name = "") +
        geom_vline(xintercept = 1, color = "black", linetype = "dashed", alpha = .5) +
        labs(
            subtitle = plot_title
        ) +
        scale_colour_manual(values = thesis_col[seq(4, length(thesis_col), 2)]) +
        theme_minimal(15) +
        theme(legend.position = "none") +
        facet_grid(group ~ ., space = "free_y", scales = "free_y", switch = "y") +
        theme(panel.spacing = unit(1, "lines")) +
        theme(
            strip.placement = "outside",
            strip.background = element_rect(fill = NA, colour = NA),
            panel.spacing = unit(0.3, "cm"), axis.title.y = element_blank()
        )
}

fg_plot_vaccine <- function(tidy_fg_fit, labels, levels, var_group, plot_title = "", colour = thesis_col[2]) {
    labels |>
        full_join(
            tidy_fg_fit |>
                bind_cols(levels |> select(label)),
            by = "label"
        ) |>
        mutate(
            estimate = if_else(is.na(estimate), 1, estimate),
            label = factor(label, levels = labels$label),
            ref = if_else(estimate == 1, "ref", "not ref")
        ) |>
        filter(group %in% var_group) |>
        ggplot() +
        aes(y = label, x = estimate, xmin = conf.low, xmax = conf.high) +
        geom_point(size = 2, color = colour) +
        geom_errorbarh(height = .3, linewidth = 0.9, color = colour) +
        scale_x_continuous(
            name = "Hazard ratio", trans = "log10",
            breaks = scales::pretty_breaks()
        ) +
        scale_y_discrete(limits = rev, name = "") +
        geom_vline(xintercept = 1, color = "black", linetype = "dashed", alpha = .5) +
        labs(
            subtitle = plot_title
        ) +
        theme_minimal(15) +
        theme(legend.position = "none") +
        facet_grid(group ~ ., space = "free_y", scales = "free_y", switch = "y") +
        theme(panel.spacing = unit(1, "lines")) +
        theme(
            strip.placement = "outside",
            strip.background = element_rect(fill = NA, colour = NA),
            panel.spacing = unit(0.3, "cm"), axis.title.y = element_blank()
        )
}

fg_plot_shift <- function(tidy_fg_fit_shift, plot_title = "") {
    labels |>
        full_join(
            tidy_fg_fit_shift |>
                bind_cols(levels |> select(label)),
            by = c("label", "shift")
        ) |>
        distinct() |>
        mutate(
            estimate = if_else(is.na(estimate), 1, estimate),
            ref = if_else(estimate == 1, "ref", "not ref")
        ) |>
        ggplot() +
        aes(y = label, x = estimate, xmin = conf.low, xmax = conf.high, color = shift) +
        geom_point(size = 1.5, position = position_dodge2(width = 0.9, reverse = TRUE)) +
        geom_errorbarh(height = .3, position = position_dodge2(width = 1, reverse = TRUE)) +
        scale_x_continuous(name = "Hazard ratio", trans = "log10") +
        scale_y_discrete(limits = rev(level_order), name = "") +
        scale_colour_manual(values = thesis_col_blue[seq(2, length(thesis_col_blue))]) +
        geom_vline(xintercept = 1, color = "black", linetype = "dashed", alpha = .5) +
        labs(
            subtitle = plot_title,
            color = "Shift in symptom onset (days)"
            # caption = "Shown on log scale, reference group: Jun 2020."
        ) +
        theme_minimal(15) +
        theme(panel.spacing = unit(1, "lines")) +
        theme(
            legend.position = "bottom",
            strip.placement = "outside",
            strip.background = element_rect(fill = NA, colour = NA),
            panel.spacing = unit(0.3, "cm"), axis.title.y = element_blank()
        ) +
        facet_wrap(~shift, nrow = 1)
}

fg_shift <- function(hosp, shift = 1) {
    hosp_shift <- hosp |>
        mutate(
            monthyear = case_when(
                eventm == "Death" ~ paste0(lubridate::month(hospital_in + shift, label = TRUE), " ", year(hospital_in + shift)),
                TRUE ~ monthyear
            ),
            monthyear = factor(monthyear, levels = level_order)
        )

    fg_shift <- hosp_shift |> mutate(
        eventm = as.numeric(eventm),
        eventm = if_else(is.na(eventm), 0, eventm),
        eventm = factor(eventm, 0:2, labels = c("Censor", "Death", "Discharge"))
    )

    pdata_shift <- finegray(Surv(fg_shift$time1m, fg_shift$eventm) ~ ., data = fg_shift)

    pdata_shift$ageGrp7 <- relevel(pdata_shift$ageGrp7, ref = "15-24")
    pdata_shift$monthyear <- relevel(pdata_shift$monthyear, ref = "Jun 2020")
    pdata_shift$charlson_index <- relevel(pdata_shift$charlson_index, ref = "0")

    fg_fit_shift <- coxph(Surv(fgstart, fgstop, fgstatus) ~ monthyear + sex + strata(ageGrp7) + strata(phec_name) + strata(vaccine) + ethGrp4 + imd_quintile + charlson_index,
        weight = fgwt,
        data = pdata_shift
    )

    return(fg_fit_shift)
}
