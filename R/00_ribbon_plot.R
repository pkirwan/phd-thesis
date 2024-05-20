library(tidyverse)
library(ggridges)
library(ggh4x)

# figures
bc_ribbon_plot <- function(
    data,
    point_data,
    xlab = "Year",
    ylab = "Estimated undiagnosed HIV prevalence",
    lims = c(NA, NA),
    legend_pos = "none",
    line_col = thesis_col[seq(1, length(thesis_col), 3)],
    point_col = thesis_col[9]) {
    plt <- data |>
        pivot_wider(names_from = "quartile") |>
        ggplot() +
        aes(year, colour = group, fill = group) +
        scale_y_continuous(
            limits = lims,
            expand = expansion(mult = c(0.05, 0.05))
        ) +
        geom_line(aes(y = `50%`), linewidth = 1) +
        geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.1) +
        labs(
            x = xlab, y = ylab,
            colour = "", shape = "", fill = ""
        ) +
        theme_minimal(15) +
        scale_colour_manual(values = line_col) +
        scale_fill_manual(values = line_col) +
        theme(legend.position = legend_pos, panel.grid.minor = element_blank())

    if (!missing(point_data)) {
        plt <- plt +
            geom_point(data = point_data, aes(y = value, shape = group), colour = point_col, fill = point_col)
    }
    return(plt)
}

design <- c(
    "
AABBCC
#DDEE#
"
)

bc_diag_prob_plot <- function(
    data,
    point_data,
    xlab = "Year",
    ylab = "Estimated HIV diagnosis probability",
    legend_pos = "bottom",
    line_col = thesis_col[seq(1, length(thesis_col), 3)],
    point_col = thesis_col[9],
    facet_rows = NULL) {
    plt <- data |>
        mutate(strata = factor(strata, levels = c("Recently acquired", "CD4>500", "CD4 350-500", "CD4 200-350", "CD4<200"))) |>
        bc_ribbon_plot(point_data = point_data, xlab = xlab, ylab = ylab, legend_pos = legend_pos, line_col = line_col, point_col = point_col) +
        scale_y_continuous(
            # limits = c(0, NA),
            labels = scales::percent,
            expand = expansion(mult = c(0.05, 0.05))
        ) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    if (is.null(facet_rows)) {
        plt <- plt + ggh4x::facet_manual(~strata, scales = "free", design = design)
    } else {
        plt <- plt + facet_wrap(~strata, scales = "free", nrow = facet_rows)
    }
    return(plt)
}

box_plot_year <- function(
    data,
    col = thesis_col[seq(1, length(thesis_col), 3)]) {
    data |>
        filter(quarter %% 10 == 0) |>
        ggplot() +
        aes(group.x, diff, colour = group.x) +
        geom_hline(yintercept = 0, colour = "grey") +
        geom_point(position = position_jitter(width = 0.3), alpha = 0.2) +
        geom_boxplot(coef = Inf, width = 0.4, alpha = 0.2) +
        labs(
            x = "",
            y = "Difference",
            colour = "Model"
        ) +
        theme_minimal(15) +
        scale_colour_manual(values = col) +
        theme(legend.position = "none", panel.grid.minor = element_blank()) +
        facet_wrap(~quarter, ncol = 3, labeller = label_both)
}

box_plot <- function(
    data,
    col = thesis_col[seq(1, length(thesis_col), 3)]) {
    data |>
        ggplot() +
        aes(group.x, diff, colour = group.x) +
        geom_hline(yintercept = 0, colour = "grey") +
        # geom_point(data = slice_sample(plot_data, prop = 0.1), position = position_jitter(width = 0.3), alpha = 0.08) +
        geom_boxplot(coef = Inf, alpha = 0.2, width = 0.4) +
        geom_text(
            data = data |> group_by(group.x) |> summarise(bias = mean(diff), pos = quantile(diff, 0.75) + 7),
            aes(
                y = pos,
                label = paste0("Bias: ", format(round(bias, 2), nsmall = 2))
            ),
            size = 4,
            nudge_x = 0.25,
        ) +
        labs(
            x = "",
            y = "Difference",
            colour = "Model"
        ) +
        theme_minimal(15) +
        scale_colour_manual(values = col) +
        theme(legend.position = "none", panel.grid.minor = element_blank())
}

mse_plot <- function(
    data,
    col = thesis_col[seq(1, length(thesis_col), 3)],
    offset = 30) {
    data |>
        ggplot() +
        aes(group.x, mse, colour = group.x) +
        geom_boxplot(coef = Inf, alpha = 0.2, width = 0.4) +
        geom_text(
            data = data |> group_by(group.x) |> summarise(bias = mean(mse), pos = quantile(mse, 0.75) + offset),
            aes(
                y = pos,
                label = paste0("PMSE: ", format(round(bias, 1), nsmall = 1))
            ),
            nudge_x = 0.3,
            size = 4
        ) +
        labs(
            x = "",
            y = "Squared difference",
            colour = "Model"
        ) +
        theme_minimal(15) +
        scale_colour_manual(values = col) +
        theme(legend.position = "none", panel.grid.minor = element_blank())
}

projection_plot <- function(data) {
    data |>
        ggplot() +
        aes(x = freq, y = year, fill = inc_group) +
        geom_density_ridges(scale = 1.2) +
        coord_flip(
            xlim = c(0, 2500)
        ) +
        scale_x_continuous(
            breaks = c(50, 250, 800, 1600, 2400),
            expand = expansion(mult = c(0, 0.05))
        ) +
        labs(
            x = "Annual HIV incidence",
            y = "Year",
            fill = ""
        ) +
        geom_vline(xintercept = 250, linetype = "dashed") +
        geom_vline(xintercept = 50, linetype = "dashed") +
        theme_minimal(15) +
        theme(
            legend.position = "top",
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.line.x = element_line(colour = "black"),
            axis.ticks.x = element_line(colour = "black"),
            axis.line.y = element_line(colour = "black"),
            axis.ticks.y = element_line(colour = "black")
        ) +
        scale_fill_manual(values = c(thesis_col[5], thesis_col[9]))
}
