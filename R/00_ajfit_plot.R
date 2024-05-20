library(tidyverse)
library(flexsurv)
library(scales)

ajfit_month_plot <- function(
    x, title = NULL, subtitle = NULL, xlab = "Days after hospital admission", labeldays = 250, maxdays = 300,
    monthlist = c("Mar\n2020", "Apr\n2020", "May\n2020", "Jun-Aug\n2020", "Sep\n2020", "Oct\n2020", "Nov\n2020", "Dec\n2020", "Jan\n2021", "Feb\n2021")) {
    x |>
        filter(state != "Hospital") |>
        mutate(monthofadmission = factor(monthofadmission,
            levels = c("Mar", "Apr", "May", "Jun/Jul/Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb"),
            labels = c("Mar\n2020", "Apr\n2020", "May\n2020", "Jun-Aug\n2020", "Sep\n2020", "Oct\n2020", "Nov\n2020", "Dec\n2020", "Jan\n2021", "Feb\n2021")
        )) |>
        filter(monthofadmission %in% monthlist) |>
        ggplot() +
        aes(x = time, y = val, ymin = lower, ymax = upper, lty = model, col = state, fill = state) +
        # geom_hline(yintercept = 0) +
        geom_line(linewidth = 1) +
        geom_ribbon(alpha = 0.3, linewidth = NA) +
        scale_y_continuous(
            limits = c(0, 1),
            breaks = seq(0, 1, by = 0.25),
            labels = scales::percent_format(accuracy = 1)
        ) +
        scale_x_continuous(
            limits = c(0, maxdays),
            breaks = c(0, labeldays)
        ) +
        labs(title = title, subtitle = subtitle, x = xlab, y = "Probability of having moved to state", lty = "Model", color = "State", fill = "State") +
        theme_minimal(15) +
        scale_color_manual(values = thesis_col[seq(1, length(thesis_col), 3)]) +
        scale_fill_manual(values = thesis_col[seq(1, length(thesis_col), 3)]) +
        theme(
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank()
        )
}

probs_month_plot <- function(x, title = NULL, subtitle = NULL, ylim = 1) {
    x |>
        ggplot() +
        aes(monthofadmission, val, colour = event, ymin = lower, ymax = upper) +
        # geom_hline(yintercept = 0) +
        geom_errorbar(linewidth = 1, position = position_dodge(width = 0.5), width = 1.5) +
        labs(y = "", x = "", title = title, subtitle = subtitle, color = "Outcome") +
        scale_y_continuous(
            limits = c(0, ylim),
            breaks = seq(0, ylim, by = 0.25),
            labels = scales::percent_format(accuracy = 1)
        ) +
        theme_minimal(15) +
        scale_color_manual(values = thesis_col[seq(1, length(thesis_col), 3)]) +
        theme(
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
        )
}

probs_month_plot_death <- function(x, title = NULL, subtitle = NULL, ylim = 1, colour = thesis_col[1]) {
    x |>
        ggplot() +
        aes(monthofadmission, val, colour = event, ymin = lower, ymax = upper) +
        # geom_hline(yintercept = 0) +
        geom_errorbar(linewidth = 1.05) +
        labs(y = "", x = "", title = title, subtitle = subtitle, color = "Outcome") +
        scale_y_continuous(
            limits = c(0, ylim),
            breaks = seq(0, ylim, by = 0.25),
            labels = scales::percent_format(accuracy = 1)
        ) +
        theme_minimal(15) +
        scale_color_manual(values = colour) +
        theme(
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
        )
}


quantile_month_plot <- function(x, title = NULL, subtitle = NULL, ylim = 20) {
    x |>
        ggplot() +
        aes(monthofadmission, ymin = lower_0.5, ymax = upper_0.5, y = median, colour = event) +
        # geom_hline(yintercept = 0) +
        # geom_errorbar(position = position_dodge(width = 0.8), width = 1.5, linewidth = 1.05) +
        geom_point(position = position_dodge(width = 0.5), size = 2) +
        geom_linerange(position = position_dodge(width = 0.5), linewidth = 1) +
        labs(title = title, subtitle = subtitle, y = "Days", x = "", colour = "Outcome") +
        coord_cartesian(ylim = c(0, ylim)) +
        theme_minimal(15) +
        scale_color_manual(values = thesis_col[seq(1, length(thesis_col), 3)]) +
        theme(
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
        )
}

probs_week_plot <- function(month_df, week_df, xlab = "", annotate = FALSE, guide = TRUE) {
    p1 <- week_df |>
        ggplot() +
        aes(week, val_d, color = event, fill = event, ymin = lower_d, ymax = upper_d) +
        geom_errorbar(
            width = 4,
            data = month_df |> filter(monthofadmission != "Jun/Jul/Aug"),
            aes(week, val, ymin = lower, ymax = upper)
        ) +
        geom_errorbar(
            width = 11,
            data = month_df |> filter(monthofadmission == "Jun/Jul/Aug"),
            aes(week, val, ymin = lower, ymax = upper)
        ) +
        geom_line() +
        geom_ribbon(alpha = 0.3, lty = 0) +
        labs(
            y = "",
            x = xlab,
            color = "Outcome", fill = "Outcome"
        ) +
        scale_y_continuous(
            limits = c(0, 1),
            labels = scales::percent_format(),
            expand = expansion(mult = c(0, 0.05))
        ) +
        scale_x_continuous(
            limits = c(10, 53),
            breaks = c(10, 30, 50)
        ) +
        theme_minimal(15) +
        theme(
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            legend.position = "top"
        )

    if (guide) {
        p1 <- p1 +
            scale_color_manual(values = thesis_col[seq(1, length(thesis_col), 3)]) +
            scale_fill_manual(values = thesis_col[seq(1, length(thesis_col), 3)])
    } else {
        p1 <- p1 +
            scale_color_manual(values = thesis_col[seq(1, length(thesis_col), 3)], guide = "none") +
            scale_fill_manual(values = thesis_col[seq(1, length(thesis_col), 3)], guide = "none")
    }

    if (annotate) {
        p1 <- p1 +
            annotate("text", x = 12 - 0.2, y = 0.45, label = "Mar 2020", size = 4.5, angle = 90) +
            annotate("text", x = 16 - 0.2, y = 0.45, label = "Apr 2020", size = 4.5, angle = 90) +
            annotate("text", x = 20 - 0.2, y = 0.45, label = "May 2020", size = 4.5, angle = 90) +
            annotate("text", x = 29 - 0.2, y = 0.45, label = "Jun-Aug 2020", size = 4.5, angle = 90) +
            annotate("text", x = 38 - 0.2, y = 0.45, label = "Sep 2020", size = 4.5, angle = 90) +
            annotate("text", x = 42 - 0.2, y = 0.45, label = "Oct 2020", size = 4.5, angle = 90) +
            annotate("text", x = 47 - 0.2, y = 0.45, label = "Nov 2020", size = 4.5, angle = 90) +
            annotate("text", x = 51 - 0.2, y = 0.45, label = "Dec 2020", size = 4.5, angle = 90)
    }
    return(p1)
}
