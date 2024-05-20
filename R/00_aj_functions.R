# Functions for computing Aalen-Johansen estimates and generating plots

library(tidyverse)
library(lubridate)
library(survival)
library(matrixStats)

# Compute Aalen-Johansen estimates
aj_fit <- function(x, nd) {
    evnames <- c("Death", "Discharge")
    statenames <- c("Hospital", evnames)

    x <- x |> mutate(eventm = if_else(is.na(eventm), "0", as.character(eventm)))
    ncovvals <- dplyr::count(nd) |> as.numeric()
    sftidy <- vector(ncovvals, mode = "list")
    varnames <- names(nd)

    # calculate the AJ survival estimates
    for (i in 1:ncovvals) {
        tbl <- x |> inner_join(nd |> filter(row_number() == i), by = varnames)
        if (dplyr::count(tbl) == 0) {
            next
        }
        event <- tbl$eventm
        event <- factor(event, levels = c(0, evnames))
        time <- tbl$timem |> as.numeric()
        sf <- survival::survfit(Surv(time, event) ~ 1)
        sftidy[[i]] <- as.data.frame(unclass(sf)[c("time", "pstate", "lower", "upper")])
        sftidy[[i]] <- sftidy[[i]] |> bind_cols(nd |> filter(row_number() == i))
    }
    sftidy <- do.call("rbind", sftidy)
    ajlong <- sftidy |>
        pivot_longer(
            cols = c(num_range("pstate.", 1:length(statenames)), num_range("lower.", 1:length(statenames)), num_range("upper.", 1:length(statenames))),
            names_to = c("summary", "state"),
            names_sep = "\\.", values_to = "prob"
        ) |>
        pivot_wider(names_from = "summary", values_from = "prob")
    ajlong$state <- as.character(factor(ajlong$state, labels = statenames))
    names(ajlong)[names(ajlong) == "pstate"] <- "val"
    return(ajlong)
}


# Plot HFR estimates
ajhfr <- function(x, mat = FALSE, flip = FALSE, thin = TRUE) {
    covars <- x |>
        select(-c(time, state, val, lower, upper)) |>
        names()
    vars <- rlang::syms(covars)
    if (covars[1] == "monthyear") {
        x <- x |>
            mutate(monthyear = factor(monthyear, levels = level_order, labels = level_order))
    }
    if (covars[1] == "vaccine") {
        x <- x |>
            mutate(vaccine = factor(vaccine, levels = vaccine_order))
    }
    x <- x |>
        filter(state == "Death") |>
        group_by(!!!vars, state) |>
        arrange(time) |>
        slice(n()) |>
        ungroup()
    if (mat == TRUE) {
        return(x)
    } else {
        if (thin == TRUE) {
            x <- x |>
                filter(monthyear %in% level_order_thinned)
        }

        p1 <- x |>
            ggplot() +
            aes(x = eval(as.name(covars[1])), val, ymin = lower, ymax = upper, color = state) +
            geom_errorbar(linewidth = 0.6) +
            labs(y = "Probability", x = "Month of hospital admission", title = "", caption = "", colour = "") +
            coord_cartesian(ylim = c(0, NA)) +
            theme_minimal(15) +
            scale_y_continuous(expand = expansion(mult = c(0, 0.05)), labels = scales::percent_format(accuracy = 1)) +
            scale_colour_manual(values = thesis_col[seq(1, length(thesis_col), 3)]) +
            scale_x_discrete(breaks = level_order_thinned[c(FALSE, TRUE, FALSE, FALSE)], expand = expansion(mult = c(0, 0))) +
            theme(
                legend.position = "none",
                panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_blank(),
                # panel.grid.major.x = element_blank()
            )
        if (flip == TRUE) {
            p1 <- p1 + scale_x_discrete(breaks = level_order_thinned[c(FALSE, TRUE, FALSE, TRUE)], expand = expansion(mult = c(0.01, 0.05)), limits = rev) + coord_flip(ylim = c(0, NA))
        }
        # else {
        #     p1 <- p1 + aes(color = eval(as.name(covars[1])))
        # }
        return(p1)
    }
}

# Plot LOS estimates
ajlos <- function(x, mat = FALSE, stat = "median", flip = FALSE, maxdays = 10, thin = TRUE) {
    covars <- x |>
        select(-c(time, state, val, lower, upper)) |>
        names()
    vars <- rlang::syms(covars)
    if (stat == "median") {
        x <- x |>
            filter(state %in% c("Death", "Discharge")) |>
            group_by(!!!vars, state) |>
            arrange(state, !!!vars) |>
            mutate(
                lower = if_else(val == 0, 0, lower),
                upper = if_else(val == 0, 0, upper),
                weight = lead(val, default = 0) - val,
                uppweight = lead(upper, default = 0) - upper,
                lowweight = lead(lower, default = 0) - lower
            ) |>
            summarise(
                med = matrixStats::weightedMedian(time, weight),
                upper = matrixStats::weightedMedian(time, uppweight),
                lower = matrixStats::weightedMedian(time, lowweight),
                .groups = "drop"
            )
    }
    if (stat == "mean") {
        x <- x |>
            filter(state %in% c("Death", "Discharge")) |>
            group_by(!!!vars, state) |>
            arrange(state, !!!vars) |>
            mutate(
                lower = if_else(val == 0, 0, lower),
                upper = if_else(val == 0, 0, upper),
                weight = lead(val, default = 0) - val,
                uppweight = lead(upper, default = 0) - upper,
                lowweight = lead(lower, default = 0) - lower,
                weight = if_else(weight < 0, 0, weight),
                uppweight = if_else(uppweight < 0, 0, uppweight),
                lowweight = if_else(lowweight < 0, 0, lowweight)
            ) |>
            summarise(
                med = weighted.mean(time, weight),
                upper = weighted.mean(time, uppweight),
                lower = weighted.mean(time, lowweight),
                .groups = "drop"
            )
    }
    if (covars[1] == "monthyear") {
        x <- x |>
            mutate(monthyear = factor(monthyear, levels = level_order, labels = level_order))
    }
    if (covars[1] == "vaccine") {
        x <- x |>
            mutate(vaccine = factor(vaccine, levels = vaccine_order))
    }
    if (mat == TRUE) {
        return(x)
    } else {
        if (thin == TRUE) {
            x <- x |>
                filter(monthyear %in% level_order_thinned)
        }

        p1 <- x |>
            ggplot() +
            aes(x = eval(as.name(covars[1])), med, color = state, min = lower, max = upper) +
            geom_point(size = 2, position = position_dodge(width = 0.3)) +
            geom_linerange(position = position_dodge(width = 0.3)) +
            labs(y = "Days", x = "Month of hospital admission", title = "", caption = "", colour = "") +
            coord_cartesian(ylim = c(0, maxdays)) +
            theme_minimal(15) +
            scale_colour_manual(values = thesis_col[seq(1, length(thesis_col), 3)]) +
            scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = scales::pretty_breaks()) +
            scale_x_discrete(breaks = level_order_thinned[c(FALSE, TRUE, FALSE, FALSE)], expand = expansion(mult = c(0.04, 0.04))) +
            # theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
            theme(
                panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_blank()
            )
        if (flip == TRUE) {
            p1 <- p1 + scale_x_discrete(breaks = level_order_thinned[c(FALSE, TRUE, FALSE, TRUE)], expand = expansion(mult = c(0.05, 0.05)), limits = rev) + coord_flip(ylim = c(0, maxdays))
        }
        return(p1)
    }
}

ajcurve <- function(x) {
    x |>
        filter(state %in% c("Death"), monthyear %in% level_order_thinned) |>
        mutate(monthyear = factor(monthyear, levels = level_order)) |>
        ggplot() +
        aes(x = time, y = val, ymin = lower, ymax = upper, fill = state) +
        geom_line() +
        geom_ribbon(alpha = 0.3) +
        labs(title = "", x = "Days after hospital admission", y = "Probability of fatality", fill = "Month of admission", caption = "") +
        scale_x_continuous(breaks = c(0, 25, 50), limits = c(0, 60)) +
        theme_minimal(15) +
        scale_fill_manual(values = thesis_col[seq(1, length(thesis_col), 3)]) +
        theme(legend.position = "top")
}

# Join HFR and LOS estimate plots
aj_full_fit <- function(ajfit_df, nd, mat = FALSE, save_name = NA, flip = FALSE, fig_height = 13, fig_width = 12, shade = 0, maxdays = 10, thin = TRUE) {

    if (mat == TRUE) {
        hfr <- ajhfr(ajfit_df, mat = TRUE)
        los <- ajlos(ajfit_df, mat = TRUE, maxdays = maxdays)
        out <- list("hfr" = hfr, "los" = los)
        return(out)
    } else {
        covars <- nd |> names()
        cats <- nd[1] |>
            distinct() |>
            dplyr::count()
        p1 <- ajhfr(ajfit_df, flip = flip, thin = thin) # +
        # annotate("text", label = "estimates for most recent months  \nlikely to be revised upwards  \n", x = Inf, y = Inf, vjust = 0, hjust = 1, angle = 90, alpha = shade, size = 2) +
        # annotate("rect", xmin = as.numeric(cats - shade + 0.5), xmax = as.numeric(cats + 0.5), ymin = -Inf, ymax = Inf, alpha = .2)
        p2 <- ajlos(ajfit_df, maxdays = maxdays, flip = flip, thin = thin) # +
        # annotate("text", label = "estimates for most recent months  \nlikely to be revised upwards  \n", x = Inf, y = Inf, vjust = 0, hjust = 1, angle = 90, alpha = shade, size = 2) +
        # annotate("rect", xmin = as.numeric(cats - shade + 0.5), xmax = as.numeric(cats + 0.5), ymin = -Inf, ymax = Inf, alpha = .2)
        if (length(nd) == 2 && flip == TRUE) {
            p1 <- p1 + facet_grid(rows = vars(eval(as.name(covars[2]))))
            p2 <- p2 + facet_grid(rows = vars(eval(as.name(covars[2]))))
            plot <- p1 + p2
        } else if (length(nd) == 2 && flip == FALSE) {
            p1 <- p1 + facet_grid(~ eval(as.name(covars[2])))
            p2 <- p2 + facet_grid(~ eval(as.name(covars[2])))
            plot <- p1 / p2
        } else {
            plot <- p1 + p2
        }
        plot + plot_annotation(tag_levels = "A") + plot_layout(axis_titles = "collect", axes = "collect")
    }
}


fix_factors <- function(fs) {
    if ("monthofadmission" %in% names(fs)) {
        fs <- fs |>
            mutate(
                monthofadmission = factor(monthofadmission,
                    levels = c("Mar", "Apr", "May", "Jun/Jul/Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb"),
                    labels = c("Mar 2020", "Apr 2020", "May 2020", "Jun-Aug 2020", "Sep 2020", "Oct 2020", "Nov 2020", "Dec 2020", "Jan 2021", "Feb 2021")
                ),
                week = case_when(
                    monthofadmission == "Mar 2020" ~ 12,
                    monthofadmission == "Apr 2020" ~ 16,
                    monthofadmission == "May 2020" ~ 20,
                    monthofadmission == "Jun-Aug 2020" ~ 29,
                    monthofadmission == "Sep 2020" ~ 38,
                    monthofadmission == "Oct 2020" ~ 42,
                    monthofadmission == "Nov 2020" ~ 47,
                    monthofadmission == "Dec 2020" ~ 51
                )
            )
    }
    if ("agegroup" %in% names(fs)) {
        fs <- fs |>
            mutate(
                agegroup = case_when(
                    agegroup == "[15,45)" ~ "15-45",
                    agegroup == "[45,65)" ~ "45-65",
                    agegroup == "[65,75)" ~ "65-75",
                    agegroup == "[75,110)" ~ "75+"
                )
            )
    }
    if ("comorbid_multip" %in% names(fs)) {
        fs <- fs |>
            mutate(
                comorbid_multip = if_else(comorbid_multip == "3", "3+", comorbid_multip)
            )
    }
    return(fs)
}

probs_month <- function(x, nd) {
    fs <- probs_flexsurvmix(x, newdata = nd, B = 500)
    fs <- fix_factors(fs)
    return(fs)
}

probs_mix <- function(x, y, nd) {
    fs <- fmixmsm("Hospital" = x, "ICU" = y) |>
        ppath_fmixmsm(newdata = nd, B = 500, final = TRUE)
    fs <- fix_factors(fs) |>
        arrange(final, monthofadmission) |>
        filter(final == "Death") |>
        rename(event = final)
    return(fs)
}

quantile_month <- function(x, nd) {
    fs <- quantile_flexsurvmix(x, newdata = nd, B = 500, probs = c(0.25, 0.5, 0.75))
    fs <- fix_factors(fs) |>
        pivot_wider(names_from = p, values_from = c(val, lower, upper)) |>
        rename(median = val_0.5, lower_quartile = val_0.25, upper_quartile = val_0.75) |>
        arrange(event, monthofadmission)
    return(fs)
}

probs_week <- function(x, nd) {
    week_df <- probs_flexsurvmix(x, newdata = nd, B = 500)
    week_df <- fix_factors(week_df) |>
        rename(val_d = val, lower_d = lower, upper_d = upper)
    return(week_df)
}
