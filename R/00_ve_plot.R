library(tidyverse)
library(patchwork)
library(scales)

ve_plot <- function(
    hazards,
    colour,
    xlab = "Vaccination status",
    ylab = "Estimated booster VE",
    breaks = c(-0.5, -0.25, 0, 0.25, 0.5, 0.75, 1),
    ylim = c(-0.3, 1)) {
    hazards |>
        ggplot() +
        aes(x = name, y = est, ymin = lower, ymax = upper) +
        geom_hline(yintercept = 0) +
        geom_point(size = 3, colour = colour) +
        geom_errorbar(width = 0.2, colour = colour) +
        labs(
            x = xlab,
            y = ylab
        ) +
        scale_y_continuous(
            expand = expansion(mult = c(0.05, 0.05)),
            breaks = breaks,
            labels = label_percent()
        ) +
        coord_cartesian(ylim = ylim) +
        theme_minimal(15) +
        theme(
            # no vertical grid lines
            panel.grid.major.x = element_blank(),
            # no horizontal grid lines
            panel.grid.major = element_line(colour = "grey", linetype = "dashed"),
            panel.grid.minor = element_blank(),
            # no legend title
            legend.position = "none",
            # space between x-axis and label
            axis.title.x = element_text(margin = margin(t = 10))
        )
}

ve_plot_sym <- function(
    hazards,
    colour,
    xlab = "Vaccination status",
    ylab = "Estimated booster VE",
    breaks = c(-1.5, -1, -0.5, 0, 0.5, 1),
    ylim = c(-1.5, 1),
    colour_lab = "Symptom status",
    colour_levels = c("COVID symptoms", "Non-COVID symptoms or asymptomatic"),
    colour_vals = rev(thesis_col[seq(1, length(thesis_col), 6)])
    ) {
    hazards |>
        ggplot() +
        aes(x = name, y = est, ymin = lower, ymax = upper, colour = {{ colour }}) +
        geom_hline(yintercept = 0) +
        geom_point(size = 3, position = position_dodge(width = 0.5)) +
        geom_errorbar(width = 0.2, position = position_dodge(width = 0.5)) +
        labs(
            x = xlab,
            y = ylab,
            colour = colour_lab
        ) +
        scale_y_continuous(
            expand = expansion(mult = c(0.05, 0.05)),
            breaks = breaks,
            labels = label_percent()
        ) +
        coord_cartesian(ylim = ylim) +
        theme_minimal(15) +
        theme(
            # no vertical grid lines
            panel.grid.major.x = element_blank(),
            # no horizontal grid lines
            panel.grid.major = element_line(colour = "grey", linetype = "dashed"),
            panel.grid.minor = element_blank(),
            # no legend title
            #legend.position = "none",
            # space between x-axis and label
            axis.title.x = element_text(margin = margin(t = 10))
        ) +
        scale_color_manual(
            values = colour_vals,
            label = colour_levels
        )
}

sojourn_plot <- function(
    sojourn_times,
    colour,
    xlab = "Vaccination status",
    ylims = c(3.2, 16),
    breaks = c(1, 3, 5, 7, 9, 11, 13),
    texty = 14.9) {
    sojourn_times |>
        ggplot() +
        aes(
            x = cat, y = sojourn, ymin = lower, ymax = upper,
            label = paste0(
                format(round(sojourn, 2), nsmall = 2),
                " days (",
                format(round(lower, 2), nsmall = 2),
                ", ",
                format(round(upper, 2), nsmall = 2),
                ")"
            )
        ) +
        geom_hline(yintercept = 0) +
        geom_point(size = 3, colour = colour) +
        geom_errorbar(width = 0.2, colour = colour) +
        geom_text(aes(y = texty), colour = "grey20") +
        labs(
            x = xlab,
            y = "Estimated mean duration of PCR positivity (days)",
            fill = ""
        ) +
        scale_x_discrete(
            limits = rev
        ) +
        scale_y_continuous(
            expand = expansion(mult = c(0.05, 0.05)),
            breaks = breaks
        ) +
        coord_flip(ylim = ylims) +
        theme_minimal(15) +
        theme(
            # no vertical grid lines
            panel.grid.major.y = element_blank(),
            # no horizontal grid lines
            panel.grid.major.x = element_line(colour = "grey", linetype = "dashed"),
            panel.grid.minor = element_blank(),
            # no legend title
            legend.position = "none",
            # space between x-axis and label
            axis.title.y = element_text(margin = margin(r = 10, l = 10)),
            axis.title.x = element_text(margin = margin(t = 10), hjust = 0.3)
        )
}


sojourn_plot_sym <- function(
    sojourn_times,
    colour) {
    sojourn_times |>
        filter(cat %in% c("COVID symptoms", "Non-COVID symptoms\nor asymptomatic")) |>
        ggplot() +
        aes(
            x = cat, y = sojourn, ymin = lower, ymax = upper, color = {{ colour }},
            label = paste0(
                format(round(sojourn, 2), nsmall = 2),
                " days (",
                format(round(lower, 2), nsmall = 2),
                ", ",
                format(round(upper, 2), nsmall = 2),
                ")"
            )
        ) +
        geom_hline(yintercept = 0) +
        geom_point(size = 3) +
        geom_errorbar(width = 0.2) +
        geom_text(aes(y = 14.9), color = "grey20") +
        labs(
            x = "Symptom status",
            y = "Estimated mean duration of PCR positivity (days)",
            fill = ""
        ) +
        scale_x_discrete(
            limits = rev
        ) +
        scale_y_continuous(
            expand = expansion(mult = c(0.05, 0.05)),
            breaks = c(3, 5, 7, 9, 11, 13)
        ) +
        coord_flip(ylim = c(3.2, 16)) +
        theme_minimal(15) +
        theme(
            # no vertical grid lines
            panel.grid.major.y = element_blank(),
            # no horizontal grid lines
            panel.grid.major.x = element_line(colour = "grey", linetype = "dashed"),
            panel.grid.minor = element_blank(),
            # no legend title
            legend.position = "none",
            # space between x-axis and label
            axis.title.y = element_text(margin = margin(r = 10, l = 10)),
            axis.title.x = element_text(margin = margin(t = 10), hjust = 0.3)
        ) +
        scale_color_manual(
            values = rev(thesis_col[seq(1, length(thesis_col), 6)]),
            label = c("COVID symptoms", "Non-COVID symptoms or asymptomatic")
        )
}
