# this is a rewrite of the simfitted.msm function from the msm package
simvax_msm <- function(x, vaccine_short_cat = NA, vaccine_cat = NA, rep = NA, fortnightly_model = FALSE) {
    cat("Iteration Number = ", rep, "\n")
    sim.df <- x$data$mf
    x$data <- msm:::expand.data(x)
    # seems like a bug: should be "cens" instead of "(cens)"
    sim.df$"cens" <- ifelse(sim.df$"(state)" %in% 1:x$qmodel$nstates,
        0, sim.df$"(state)"
    )
    if (x$qcmodel$ncovs > 0) {
        sim.df <- cbind(sim.df, x$data$mm.cov)
        cov.effs <- lapply(x$Qmatrices, function(y) {
            t(y)[t(x$qmodel$imatrix) == 1]
        })[x$qcmodel$covlabels]
    } else {
        cov.effs <- NULL
    }
    if (!is.na(vaccine_cat)) {
        if (vaccine_cat == "Fourth dose") {
            # method 1
            # cov.effs.vax <- list(vaccine_short = vaccine_short)

            # method 2
            sim_df_replacement <- tibble(cbind(mllvrahgo_noev$data$mf, mllvrahgo_noev$data$mm.cov)) |>
                filter(vaccine != "Waned third dose") |>
                select(
                    vaccine,
                    `vaccineFourth dose 0-2 months`,
                    `vaccineFourth dose 2-4 months`,
                    `vaccineFourth dose 4+ months`
                ) |>
                unique()
            sim.df <- tibble(sim.df) |>
                select(
                    -vaccine,
                    -`vaccineFourth dose 0-2 months`,
                    -`vaccineFourth dose 2-4 months`,
                    -`vaccineFourth dose 4+ months`
                ) |>
                # more to do here
                mutate(vaccine = case_when(
                    `(time)` < 8 ~ "Fourth dose 0-2 months",
                    `(time)` < 16 ~ "Fourth dose 2-4 months",
                    TRUE ~ "Fourth dose 4+ months"
                )) |>
                left_join(
                    sim_df_replacement,
                    by = c("vaccine")
                )
        } else if (vaccine_cat == "Waned third dose") {
            # method 2
            sim_df_replacement <- tibble(sim.df) |>
                filter(vaccine == "Waned third dose") |>
                select(
                    vaccine,
                    `vaccineFourth dose 0-2 months`,
                    `vaccineFourth dose 2-4 months`,
                    `vaccineFourth dose 4+ months`
                ) |>
                unique()
            sim.df <- tibble(sim.df) |>
                select(
                    -vaccine,
                    -`vaccineFourth dose 0-2 months`,
                    -`vaccineFourth dose 2-4 months`,
                    -`vaccineFourth dose 4+ months`
                ) |>
                # more to do here
                mutate(vaccine = "Waned third dose") |>
                left_join(
                    sim_df_replacement,
                    by = c("vaccine")
                )
        }
    }
    if (!is.na(vaccine_short_cat)) {
        # method 1
        # cov.effs.vax <- list(vaccine_short = vaccine_short)
        if (exists("months_since_pos", where = sim.df)) {
            # method 2
            sim_df_replacement <- tibble(sim.df) |>
                filter(vaccine_short == vaccine_short_cat) |>
                select(
                    vaccine_short, months_since_pos, `vaccine_shortFourth dose`,
                    `months_since_pos1-2 years:vaccine_shortFourth dose`,
                    `months_since_pos6-12 months:vaccine_shortFourth dose`,
                    `months_since_pos0-6 months:vaccine_shortFourth dose`,
                    `months_since_posConfirmed naive:vaccine_shortFourth dose`
                ) |>
                unique()
            sim.df <- tibble(sim.df) |>
                select(
                    -vaccine_short, -`vaccine_shortFourth dose`,
                    -`months_since_pos1-2 years:vaccine_shortFourth dose`,
                    -`months_since_pos6-12 months:vaccine_shortFourth dose`,
                    -`months_since_pos0-6 months:vaccine_shortFourth dose`,
                    -`months_since_posConfirmed naive:vaccine_shortFourth dose`
                ) |>
                mutate(vaccine_short = vaccine_short_cat) |>
                left_join(
                    sim_df_replacement,
                    by = c("vaccine_short", "months_since_pos")
                )
        } else {
            sim_df_replacement <- tibble(sim.df) |>
                filter(vaccine_short == vaccine_short_cat) |>
                select(vaccine_short, `vaccine_shortFourth dose`) |>
                unique()
            sim.df <- tibble(sim.df) |>
                select(-vaccine_short, -`vaccine_shortFourth dose`) |>
                mutate(vaccine_short = vaccine_short_cat) |>
                left_join(
                    sim_df_replacement,
                    by = c("vaccine_short")
                )
        }
    }
    names(sim.df) <- replace(
        names(sim.df),
        match(
            c("(state)", "(time)", "(subject)"),
            names(sim.df)
        ),
        c("state", "time", "subject")
    )

    if(fortnightly_model) {
        sim.df <-  bind_rows(
            sim.df |>
            filter(state == 99) |>
            mutate(
                cens = 0,
                time = case_when(
                    almostEqual(time, 2.71428571428571) ~ 2,
                    almostEqual(time, 7.14285714285714) ~ 6,
                    almostEqual(time, 11.4285714285714) ~ 10,
                    almostEqual(time, 15.8571428571429) ~ 14,
                    almostEqual(time, 20.2857142857143) ~ 18,
                    almostEqual(time, 24.2857142857143) ~ 24
                )
                ),
            sim.df |>
            filter(state == 99) |>
            mutate(
                cens = 0,
                time = case_when(
                    almostEqual(time, 2.71428571428571) ~ 0,
                    almostEqual(time, 7.14285714285714) ~ 8,
                    almostEqual(time, 11.4285714285714) ~ 12,
                    almostEqual(time, 15.8571428571429) ~ 16,
                    almostEqual(time, 20.2857142857143) ~ 20,
                    almostEqual(time, 24.2857142857143) ~ 26
                )
                ),
            sim.df |>
            filter(state == 99) |>
            mutate(
                cens = 0,
                time = case_when(
                    almostEqual(time, 2.71428571428571) ~ 4,
                    almostEqual(time, 20.2857142857143) ~ 22,
                    almostEqual(time, 24.2857142857143) ~ 28
                )
                ) |>
                filter(time %in% c(4, 22, 28))
        ) |> arrange(subject, time)
    }

    boot.df <- simmulti.msm(data = sim.df, qmatrix = qmatrix.msm(x,
        covariates = 0, ci = "none"
    ), covariates = cov.effs)

    boot.df <- boot.df |> janitor::clean_names()

    return(boot.df)
}

n_positive <- function(sim) {
    map(
        sim,
        ~ .x |>
            filter(state == 2) |>
            distinct(subject) |>
            count()
    ) |>
        unlist() |>
        as_tibble() |>
        summarise(
            median = median(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975)
        )
}

almostEqual <- function(x, y, tolerance=1e-8) {
  diff <- abs(x - y)
  mag <- pmax( abs(x), abs(y) )
  ifelse( mag > tolerance, diff/mag <= tolerance, diff <= tolerance)
}
