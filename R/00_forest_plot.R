library(tidyverse)
library(ggpp)
library(broom)
library(patchwork)
library(ggnewscale)
library(scales)

# forest_plot function
forest_plot <- function(fit,
                        cox_fit = NULL,
                        covars = NULL,
                        transition1 = "State 1 - State 2",
                        transition2 = NULL,
                        state_labels = c("COVID symptoms", "Non-COVID symptoms or asymptomatic"),
                        table = FALSE) {
    # extract the covs and covnames
    covnames <- fit$data$mf |> colnames()
    covs <- tibble(
        name = fit$data$mf[, 1] |> levels(),
        group = covnames[1],
        value_id = transition1,
        n = as.numeric(fit$data$mf[, 1] |> table())
    )
    i <- 2
    while (covnames[i] != "(subject)") {
        covs <- covs |> bind_rows(tibble(
            name = fit$data$mf[, i] |> levels(),
            group = covnames[i],
            value_id = transition1,
            n = as.numeric(fit$data$mf[, i] |> table())
        ))
        i <- i + 1
    }

    if (!is.null(transition2)) {
        covs <- covs |>
            bind_rows(
                covs |> mutate(value_id = transition2)
            )
    }

    covtable <- covs |>
        full_join(
            hazard.msm(fit) |> enframe() |> unnest_longer(col = value) |>
                mutate(
                    group = gsub(
                        paste0(
                            ".*(",
                            paste(covs$group |> unique(), collapse = "|"),
                            ").*"
                        ),
                        "\\1", name
                    ),
                    int_group = gsub(
                        paste0(
                            ".*(",
                            paste(covs$group |> unique(), collapse = "|"),
                            ").*(",
                            paste(covs$group |> unique(), collapse = "|"),
                            ").*"
                        ),
                        "\\1:\\2", name
                    ),
                    group = if_else(int_group != name, int_group, group),
                    name = gsub(paste(covs$group |> unique(), collapse = "|"), "", name),
                    # swap labels on incorrectly labelled covariates
                    name = if_else(name == "Fourth dose 0-3 months", "placeholder", name),
                    name = if_else(name == "Fourth dose 3+ months", "Fourth dose 0-3 months", name),
                    name = if_else(name == "placeholder", "Fourth dose 3+ months", name),
                    # swap labels on incorrectly labelled covariates
                    name = if_else(name == "Fourth dose 2-4 months", "placeholder", name),
                    name = if_else(name == "Fourth dose 4+ months", "Fourth dose 2-4 months", name),
                    name = if_else(name == "placeholder", "Fourth dose 4+ months", name)
                ),
            by = c("name", "group", "value_id")
        ) |>
        mutate(
            name = factor(name, levels = unique(name), labels = gsub("_", " ", unique(name))),
            group = str_to_sentence(gsub("_", " ", group)),
            est = value[, 1],
            lower = value[, 2],
            upper = value[, 3],
            est = if_else(is.na(est), 1, est)
        ) |>
        select(-value)

    if (!is.null(cox_fit)) {
        covtable <- covtable |>
            left_join(
                cox_fit |>
                    tidy(exponentiate = TRUE, conf.int = TRUE) |>
                    mutate(
                        name = term,
                        group = gsub(
                            paste0(
                                ".*(",
                                paste(covs$group |> unique(), collapse = "|"),
                                ").*"
                            ),
                            "\\1", name
                        ),
                        int_group = gsub(
                            paste0(
                                ".*(",
                                paste(covs$group |> unique(), collapse = "|"),
                                ").*(",
                                paste(covs$group |> unique(), collapse = "|"),
                                ").*"
                            ),
                            "\\1:\\2", name
                        ),
                        group = if_else(int_group != name, int_group, group),
                        name = gsub(paste(covs$group |> unique(), collapse = "|"), "", name)
                    ) |>
                    mutate(
                        name = factor(name, levels = unique(name), labels = gsub("_", " ", unique(name))),
                        group = str_to_sentence(gsub("_", " ", group)),
                        c_est = estimate,
                        c_lower = conf.low,
                        c_upper = conf.high
                    ) |>
                    select(name, c_est, c_lower, c_upper),
                by = c("name")
            )
    } else {
        covtable <- covtable |>
            cbind(tibble(c_est = NA, c_lower = NA, c_upper = NA))
    }

    if (!is.null(covars)) {
        covtable <- covtable |> filter(group %in% str_to_sentence(gsub("_", " ", covars)))
    }

    if (!is.null(cox_fit)) {
        p1 <- covtable |>
            filter(value_id == transition1) |>
            mutate(
                n = if_else(is.na(n), 1, n),
                group = case_when(
                    group == "Vaccine short" ~ "Vaccine",
                    group == "Months since pos:vaccine short" ~ "Months since pos:Vaccine",
                    TRUE ~ group
                )
            ) |>
            ggplot() +
            aes(y = name, x = est, xmin = lower, xmax = upper) +
            labs(x = "Hazard ratio (95% CI, log scale)", y = "") +
            scale_x_continuous(trans = "log10") +
            scale_y_discrete(limits = rev) +
            geom_tile(aes(fill = factor(group, levels = group |> unique()), width = Inf), alpha = 0.3) +
            geom_point(shape = 15, aes(color = "Multi-state model", size = n, alpha = 0.2), position = position_nudge(y = 0.1)) +
            geom_point(aes(x = c_est, color = "Cox model"), shape = 17, position = position_nudge(y = -0.1)) +
            geom_errorbarh(height = 0.2, aes(color = "Multi-state model"), position = position_nudge(y = 0.1)) +
            geom_errorbarh(aes(xmin = c_lower, xmax = c_upper, color = "Cox model"), height = 0.2, position = position_nudge(y = -0.1)) +
            geom_vline(xintercept = 1, linetype = "longdash", colour = "black") +
            scale_fill_manual(values = rep(c("white", "grey"), 9)) +
            scale_color_manual(
                name = "",
                values = c(`Cox model` = "#d24c4e", `Multi-state model` = "#008ecd")
            ) +
            theme_classic(15) +
            scale_size_area() +
            theme(
                strip.background = element_blank(),
                strip.placement = "outside",
                panel.spacing = unit(0, "lines"),
                axis.title.x = element_text(),
                axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                axis.line.y = element_blank(),
                axis.ticks.y = element_blank(),
                legend.position = "top",
                panel.grid.major.x = element_line(colour = "lightgrey", linewidth = 0.5)
            ) +
            guides(size = "none", alpha = "none", fill = "none") +
            facet_grid(rows = vars(factor(group, levels = group |> unique())), scales = "free_y", space = "free_y", switch = "y")
    } else {
        p1 <- covtable |>
            filter(!(est == 1 & !is.na(lower))) |>
            mutate(
                n = if_else(is.na(n), 1, n),
                group = case_when(
                    group == "Vaccine short" ~ "Vaccine",
                    group == "Months since pos:vaccine short" ~ "Months since pos:Vaccine",
                    TRUE ~ group
                )
            ) |>
            # filter(value_id == transition1) |>
            ggplot() +
            aes(
                y = name, x = est, xmin = lower, xmax = upper,
                label = if_else(!is.na(lower),
                    paste0(sprintf("%.2f", round(est, 2)), " (", sprintf("%.2f", round(lower, 2)), "-", sprintf("%.2f", round(upper, 2)), ")"),
                    "1 (baseline)"
                )
            ) +
            labs(x = "Hazard ratio (95% CI, log scale)", y = "") +
            scale_x_continuous(trans = "log10") +
            scale_y_discrete(limits = rev) +
            geom_tile(aes(fill = factor(group, levels = group |> unique()), width = Inf), alpha = 0.3) +
            scale_fill_manual(values = rep(c("white", "grey"), 9)) +
            new_scale_fill() +
            geom_point(shape = 22, position = position_dodge(width = 0.6), aes(size = n, alpha = 0.2, color = value_id, fill = value_id)) +
            geom_errorbarh(height = 0.2, position = position_dodge(width = 0.6), aes(color = value_id)) +
            geom_vline(xintercept = 1, linetype = "longdash", colour = "black") +
            coord_cartesian(clip = "off") +
            theme_classic(15) +
            scale_size_area() +
            theme(
                strip.background = element_blank(),
                strip.placement = "outside",
                panel.spacing = unit(0, "lines"),
                axis.title.x = element_text(),
                axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                axis.line.y = element_blank(),
                axis.ticks.y = element_blank(),
                legend.position = "none",
                panel.grid.major.x = element_line(colour = "lightgrey", linewidth = 0.5)
            ) +
            facet_grid(rows = vars(factor(group, levels = group |> unique())), scales = "free_y", space = "free_y", switch = "y")
    }
    if (!is.null(transition2)) {
        p1 <- p1 +
            scale_color_discrete(label = state_labels) +
            theme(legend.position = "bottom", legend.title = element_blank()) +
            guides(shape = "none", fill = "none", size = "none", alpha = "none", fill_new = "none")
    }
    if (table == TRUE) {
        if (!is.null(transition2)) {
            p1 <- covtable |>
                filter(!(est == 1 & !is.na(lower))) |>
                select(value_id, group, name, est, lower, upper)
        } else {
            p1 <- covtable |>
                filter(!(est == 1 & !is.na(lower))) |>
                select(group, name, est, lower, upper)
        }
    }
    p1
}


# model results
model_results <- function(model, dataset = NA, savename = NA, mintime = 0, timezero = 0, maxtime, ...) {
    out <- list()

    # print model results
    out$model <- model

    # print intensity matrix
    out$qmatrix <- qmatrix.msm(model)

    # print sojourn times
    out$sojorn <- sojourn.msm(model)

    # model assessment
    # cannot be run with the full dataset, but can make this quicker by down-sampling the number of subjects to use
    # to do this need to specify BOTH the subset and the initstates options (perhaps something to raise with Chris)
    # subset is a vector of study_ids, initstates is the number of individuals occupying each state at time zero

    # to get figures
    # prevalence.msm(siren_msm, times=seq(0,20,2), timezero = 0) # include timezero (possibly because time was in non-numeric week format)

    # to plot
    if (is.data.frame(dataset)) {
        sample <- dataset |>
            slice_sample(prop = 0.5) |>
            distinct(study_id) # 50% sample to compare against(~100,000 obs)
        states <- dataset |>
            arrange(time) |>
            distinct(study_id, .keep_all = TRUE) |>
            count(state) |>
            select(n)

        prev <- prevalence.msm(model,
            timezero,
            times = seq(timezero, maxtime, by = 1),
            mintime, subset = sample$study_id, initstates = states$n,
            covariates = "mean",
            ci = "normal",
            ...
        )
        out$prev <- prev
    }

    # pearson.msm provides a p-value of the Pearson test statistic, time must be numeric
    # very time consuming therefore moved to processing file
    # pearson.msm(model, timegroups = 2)

    return(out)
}

diagnostics_plot <- function(diagnostics, n_states = 2, ylim = c(0, 8)) {
    prev <- diagnostics$prev

    if (n_states == 2) {
        gg_tibble <- tibble(
            x = prev$`Observed percentages`[, 1] |> labels() |> as.numeric(),
            State1 = prev$`Observed percentages`[, 1],
            State2 = prev$`Observed percentages`[, 2],
            group = "Estimate from observed data"
        ) |>
            bind_rows(
                tibble(
                    x = prev$`Expected percentages`$estimates[, 1] |> labels() |> as.numeric(),
                    State1 = prev$`Expected percentages`$estimates[, 1],
                    State2 = prev$`Expected percentages`$estimates[, 2],
                    group = "Forecast from fitted model"
                )
            ) |>
            pivot_longer(-c(x, group))


        gg_tibble_ci <- tibble(
            x = prev$`Expected percentages`$estimates[, 1] |> labels() |> as.numeric(),
            value = prev$`Expected percentages`$estimates[, 1],
            lcl = prev$`Expected percentages`$ci[, 1, "2.5%"],
            ucl = prev$`Expected percentages`$ci[, 1, "97.5%"],
            name = "State1",
            group = "Forecast from fitted model"
        ) |>
            bind_rows(
                tibble(
                    x = prev$`Expected percentages`$estimates[, 1] |> labels() |> as.numeric(),
                    value = prev$`Expected percentages`$estimates[, 2],
                    lcl = prev$`Expected percentages`$ci[, 2, "2.5%"],
                    ucl = prev$`Expected percentages`$ci[, 2, "97.5%"],
                    name = "State2",
                    group = "Forecast from fitted model"
                )
            )
    } else if (n_states == 3) {
        gg_tibble <- tibble(
            x = prev$`Observed percentages`[, 1] |> labels() |> as.numeric(),
            State1 = prev$`Observed percentages`[, 1],
            State2 = prev$`Observed percentages`[, 2],
            State3 = prev$`Observed percentages`[, 3],
            group = "Estimate from observed data"
        ) |>
            bind_rows(
                tibble(
                    x = prev$`Expected percentages`$estimates[, 1] |> labels() |> as.numeric(),
                    State1 = prev$`Expected percentages`$estimates[, 1],
                    State2 = prev$`Expected percentages`$estimates[, 2],
                    State3 = prev$`Expected percentages`$estimates[, 3],
                    group = "Forecast from fitted model"
                )
            ) |>
            pivot_longer(-c(x, group))


        gg_tibble_ci <- tibble(
            x = prev$`Expected percentages`$estimates[, 1] |> labels() |> as.numeric(),
            value = prev$`Expected percentages`$estimates[, 1],
            lcl = prev$`Expected percentages`$ci[, 1, "2.5%"],
            ucl = prev$`Expected percentages`$ci[, 1, "97.5%"],
            name = "State1",
            group = "Forecast from fitted model"
        ) |>
            bind_rows(
                tibble(
                    x = prev$`Expected percentages`$estimates[, 1] |> labels() |> as.numeric(),
                    value = prev$`Expected percentages`$estimates[, 2],
                    lcl = prev$`Expected percentages`$ci[, 2, "2.5%"],
                    ucl = prev$`Expected percentages`$ci[, 2, "97.5%"],
                    name = "State2",
                    group = "Forecast from fitted model"
                )
            ) |>
            bind_rows(
                tibble(
                    x = prev$`Expected percentages`$estimates[, 1] |> labels() |> as.numeric(),
                    value = prev$`Expected percentages`$estimates[, 3],
                    lcl = prev$`Expected percentages`$ci[, 3, "2.5%"],
                    ucl = prev$`Expected percentages`$ci[, 3, "97.5%"],
                    name = "State3",
                    group = "Forecast from fitted model"
                )
            )
    } else if (n_states == 4) {
        gg_tibble <- tibble(
            x = prev$`Observed percentages`[, 1] |> labels() |> as.numeric(),
            State1 = prev$`Observed percentages`[, 1],
            State2 = prev$`Observed percentages`[, 2],
            State3 = prev$`Observed percentages`[, 3],
            State4 = prev$`Observed percentages`[, 4],
            group = "Estimate from observed data"
        ) |>
            bind_rows(
                tibble(
                    x = prev$`Expected percentages`$estimates[, 1] |> labels() |> as.numeric(),
                    State1 = prev$`Expected percentages`$estimates[, 1],
                    State2 = prev$`Expected percentages`$estimates[, 2],
                    State3 = prev$`Expected percentages`$estimates[, 3],
                    State4 = prev$`Expected percentages`$estimates[, 4],
                    group = "Forecast from fitted model"
                )
            ) |>
            pivot_longer(-c(x, group))


        gg_tibble_ci <- tibble(
            x = prev$`Expected percentages`$estimates[, 1] |> labels() |> as.numeric(),
            value = prev$`Expected percentages`$estimates[, 1],
            lcl = prev$`Expected percentages`$ci[, 1, "2.5%"],
            ucl = prev$`Expected percentages`$ci[, 1, "97.5%"],
            name = "State1",
            group = "Forecast from fitted model"
        ) |>
            bind_rows(
                tibble(
                    x = prev$`Expected percentages`$estimates[, 1] |> labels() |> as.numeric(),
                    value = prev$`Expected percentages`$estimates[, 2],
                    lcl = prev$`Expected percentages`$ci[, 2, "2.5%"],
                    ucl = prev$`Expected percentages`$ci[, 2, "97.5%"],
                    name = "State2",
                    group = "Forecast from fitted model"
                )
            ) |>
            bind_rows(
                tibble(
                    x = prev$`Expected percentages`$estimates[, 1] |> labels() |> as.numeric(),
                    value = prev$`Expected percentages`$estimates[, 3],
                    lcl = prev$`Expected percentages`$ci[, 3, "2.5%"],
                    ucl = prev$`Expected percentages`$ci[, 3, "97.5%"],
                    name = "State3",
                    group = "Forecast from fitted model"
                )
            ) |>
            bind_rows(
                tibble(
                    x = prev$`Expected percentages`$estimates[, 1] |> labels() |> as.numeric(),
                    value = prev$`Expected percentages`$estimates[, 4],
                    lcl = prev$`Expected percentages`$ci[, 4, "2.5%"],
                    ucl = prev$`Expected percentages`$ci[, 4, "97.5%"],
                    name = "State4",
                    group = "Forecast from fitted model"
                )
            )
    }

    p1 <- gg_tibble |> mutate(group = factor(group, levels = c("Forecast from fitted model", "Estimate from observed data"))) |>
        filter(name == "State1") |>
        ggplot() +
        aes(x, value, color = group) +
        geom_line() +
        geom_ribbon(
            data = gg_tibble_ci |> filter(name == "State1"),
            aes(x = x, ymin = lcl, ymax = ucl, fill = group),
            alpha = 0.2,
            linetype = 0
        ) +
        scale_x_continuous(limits = c(0, NA)) +
        scale_y_continuous(limits = c(NA, 100)) +
        labs(color = "", x = "Week of study", y = "Prevalence", subtitle = "Susceptible") +
        theme_minimal(15) +
        theme(plot.subtitle = element_text(hjust = 0.5)) +
        guides(fill = "none")

    p2 <- gg_tibble |> mutate(group = factor(group, levels = c("Forecast from fitted model", "Estimate from observed data"))) |>
        filter(name == "State2") |>
        ggplot() +
        aes(x, value, color = group, fill = group) +
        geom_line() +
        geom_ribbon(
            data = gg_tibble_ci |> filter(name == "State2"),
            aes(x = x, ymin = lcl, ymax = ucl),
            alpha = 0.2,
            linetype = 0
        ) +
        scale_x_continuous(limits = c(0, NA)) +
        scale_y_continuous(limits = ylim, labels = label_percent(scale = 1)) +
        labs(color = "", fill = "", x = "Week of study", y = "Prevalence", subtitle = "Infected") +
        theme_minimal(15) +
        theme(plot.subtitle = element_text(hjust = 0.5), legend.position = "top") +
        scale_colour_manual(values = thesis_col[seq(4, length(thesis_col), 4)]) +
        scale_fill_manual(values = thesis_col[seq(4, length(thesis_col), 4)]) +
        guides(fill = guide_legend(override.aes = list(fill = c(thesis_col[4], NA))))
    # guides(fill = "none")

    # patchwork plot with shared legend at the bottom
    if (n_states >= 2) {
        fig <- p1 + p2 + plot_layout(guides = "collect")
    }
    if (n_states >= 3) {
        p3 <- gg_tibble |> mutate(group = factor(group, levels = c("Forecast from fitted model", "Estimate from observed data"))) |>
            filter(name == "State3") |>
            ggplot() +
            aes(x, value, color = group) +
            geom_line() +
            geom_ribbon(
                data = gg_tibble_ci |> filter(name == "State3"),
                aes(x = x, ymin = lcl, ymax = ucl, fill = group),
                alpha = 0.2,
                linetype = 0
            ) +
            scale_x_continuous(limits = c(0, NA)) +
            scale_y_continuous(limits = ylim) +
            labs(color = "", x = "Week of study", y = "Prevalence", subtitle = "Infected (Asymptomatic)") +
            theme_minimal(15) +
            theme(plot.subtitle = element_text(hjust = 0.5), legend.position = "top") +
            scale_colour_manual(values = thesis_col[seq(4, length(thesis_col), 4)]) +
            scale_fill_manual(values = thesis_col[seq(4, length(thesis_col), 4)]) +
            guides(fill = guide_legend(override.aes = list(fill = c(thesis_col[4], NA))))

        fig <- p1 + p2 + p3 + plot_layout(guides = "collect")
    }
    if (n_states == 4) {
        p4 <- gg_tibble |> mutate(group = factor(group, levels = c("Forecast from fitted model", "Estimate from observed data"))) |>
            filter(name == "State4") |>
            ggplot() +
            aes(x, value, color = group) +
            geom_line() +
            geom_ribbon(
                data = gg_tibble_ci |> filter(name == "State4"),
                aes(x = x, ymin = lcl, ymax = ucl, fill = group),
                alpha = 0.2,
                linetype = 0
            ) +
            scale_x_continuous(limits = c(0, NA)) +
            scale_y_continuous(limits = ylim) +
            labs(color = "", x = "Week of study", y = "Prevalence", subtitle = "Infected (Asymptomatic)") +
            theme_minimal(15) +
            theme(plot.subtitle = element_text(hjust = 0.5), legend.position = "top") +
            scale_colour_manual(values = thesis_col[seq(4, length(thesis_col), 4)]) +
            scale_fill_manual(values = thesis_col[seq(4, length(thesis_col), 4)]) +
            guides(fill = guide_legend(override.aes = list(fill = c(thesis_col[4], NA))))

        fig <- p1 + p2 + p3 + p4 + plot_layout(guides = "collect")
    }

    return(p2)
}

make_quantiles <- function(df) {
    df |>
        unlist() |>
        as_tibble() |>
        summarise(
            #median = quantile(value, 0.5),
            mean = mean(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975)
        )
}

bind_quantiles <- function(df1, df2, method) {
    rbind(
        "No vaccination" = make_quantiles(df1),
        "Complete vaccination" = make_quantiles(df2)
    ) |>
        as_tibble(rownames = "vaccine") |>
        mutate(method = method)
}
