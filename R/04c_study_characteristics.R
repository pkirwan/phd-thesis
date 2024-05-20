# Figures and tables for Chapter 4

# Load libraries
here::i_am("R/04b_siren_characteristics.R")
library(here)
library(tidyverse)
library(patchwork)
library(scales)
library(ggpp)
library(gghighlight)

load(here("Data/SIREN/siren_processed_interim4_vax_final.RData"))

# filter to participants in the current cohort
interim4_cohort <- siren_cohort |>
    filter(study_id %in% siren_df_interim4$study_id)

# proportion who receive fourth dose vaccine
vaccine_uptake <- function(vaccine_date, data = interim4_cohort) {
    data |>
        group_by({{ vaccine_date }}) |>
        summarise(count = n()) |>
        ungroup() |>
        mutate(
            t = {{ vaccine_date }} - min({{ vaccine_date }}, na.rm = TRUE),
            cumcount = cumsum(count),
            pct_vax = cumcount / sum(count)
        ) |>
        filter(
            !is.na({{ vaccine_date }})
        )
}

# p2 <- bind_rows(
#     vaccine_uptake(vaccine_date1) |> mutate(group = "First dose"),
#     vaccine_uptake(vaccine_date2) |> mutate(group = "Second dose"),
#     vaccine_uptake(vaccine_date3) |> mutate(group = "Third dose"),
#     vaccine_uptake(vaccine_date4) |> mutate(group = "Fourth dose")
# ) |>
#     mutate(group = factor(group, levels = c("First dose", "Second dose", "Third dose", "Fourth dose"))) |>
#     ggplot() +
#     aes(t, pct_vax, colour = group) +
#     geom_line() +
#     labs(
#         y = "", x = "Days"
#     ) +
#     scale_y_continuous(
#         labels = label_percent(),
#         expand = expansion(mult = c(0, 0)),
#         limits = c(0, 1)
#     ) +
#     scale_x_continuous(
#         expand = expansion(mult = c(0, 0)),
#         breaks = seq(0, 91, 7)
#     ) +
#     theme_minimal(15) +
#     theme(
#         panel.grid.minor.x = element_blank(),
#         legend.position = "none"
#     ) +
#     scale_colour_manual(values = thesis_col[seq(1, length(thesis_col), 3)])

# p1 / p2 + plot_annotation(tag_levels = "A")

# ggsave(here("04_SIREN/Figs/vaccination_history.pdf"), width = 10, height = 13)
# ggsave(here("04_SIREN/Figs/vaccination_history.png"), width = 10, height = 13)

bind_rows(
    vaccine_uptake(vaccine_date4, data = interim4_cohort |> filter(region == "East Midlands")) |> mutate(group = "East Midlands"),
    vaccine_uptake(vaccine_date4, data = interim4_cohort |> filter(region == "East of England")) |> mutate(group = "East of England"),
    vaccine_uptake(vaccine_date4, data = interim4_cohort |> filter(region == "London")) |> mutate(group = "London"),
    vaccine_uptake(vaccine_date4, data = interim4_cohort |> filter(region == "North East")) |> mutate(group = "North East"),
    vaccine_uptake(vaccine_date4, data = interim4_cohort |> filter(region == "North West")) |> mutate(group = "North West"),
    vaccine_uptake(vaccine_date4, data = interim4_cohort |> filter(region == "South East")) |> mutate(group = "South East"),
    vaccine_uptake(vaccine_date4, data = interim4_cohort |> filter(region == "South West")) |> mutate(group = "South West"),
    vaccine_uptake(vaccine_date4, data = interim4_cohort |> filter(region == "West Midlands")) |> mutate(group = "West Midlands"),
    vaccine_uptake(vaccine_date4, data = interim4_cohort |> filter(region == "Yorkshire and the Humber")) |> mutate(group = "Yorkshire and the Humber"),
    vaccine_uptake(vaccine_date4, data = interim4_cohort |> filter(region == "Scotland")) |> mutate(group = "Scotland"),
    vaccine_uptake(vaccine_date4, data = interim4_cohort |> filter(region == "Wales")) |> mutate(group = "Wales"),
    vaccine_uptake(vaccine_date4, data = interim4_cohort |> filter(region == "Northern Ireland")) |> mutate(group = "Northern Ireland")
) |> ggplot() +
    aes(t, pct_vax, colour = group) +
    geom_line() +
    gghighlight(
        group == "Scotland"
    ) +
    annotate(
        "text", x = 55, y = 0.44, label = "Scotland", colour = thesis_col[1], size = 5
    ) +
    labs(
        y = "Fourth dose vaccine coverage", x = "Days of vaccine availability", colour = "Region"
    ) +
    scale_y_continuous(
        labels = label_percent(),
        expand = expansion(mult = c(0, 0)),
        limits = c(0, 1)
    ) +
    scale_x_continuous(
        expand = expansion(mult = c(0, 0)),
        breaks = seq(0, 60, 7)
    ) +
    coord_cartesian(xlim = c(0, 60)) +
    theme_minimal(15) +
    theme(
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom"
    ) +
    scale_colour_manual(values = thesis_col)

ggsave(here("04_SIREN/Figs/vaccination_history_region.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/vaccination_history_region.png"), width = 10, height = 6)


vaccine_uptake_plot <- function(covar, legend_pos = "none", xlab = "") {
    interim4_cohort |>
        mutate(vaccine = if_else(!is.na(vaccine_date4), "Fourth dose", "Waned third dose")) |>
        left_join(
            siren_df_interim4 |>
                arrange(study_id, time) |>
                select(study_id, months_since_pos) |>
                distinct(study_id, .keep_all = TRUE),
            by = "study_id"
        ) |>
        filter(months_since_pos != "No evidence of infection") |>
        mutate(
            months_since_pos = fct_relevel(months_since_pos, "0-6 months", "6-12 months", "1-2 years", "2+ years", "Confirmed naive"),
            agegr = fct_relevel(agegr, "<25", "25-34", "35-44")
        ) |>
        ggplot() +
        aes({{ covar }}, fill = vaccine) +
        geom_bar(position = position_fill(reverse = TRUE)) +
        labs(y = "Fourth dose vaccine coverage", x = xlab, fill = "Vaccine") +
        scale_y_continuous(
            labels = label_percent(),
            expand = expansion(mult = c(0, 0))
        ) +
        scale_x_discrete() +
        theme_minimal(15) +
        theme(
            panel.grid.major.x = element_blank(),
            legend.position = legend_pos,
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        ) +
        #coord_flip() +
        scale_fill_manual(values = c(thesis_col[7], NA), na.value = NA, na.translate = FALSE)
}

p1 <- vaccine_uptake_plot(staff_type, xlab = "Staff type")
p2 <- vaccine_uptake_plot(months_since_pos, xlab = "Time since previous infection")
p3 <- vaccine_uptake_plot(agegr, xlab = "Age group")

p3 + p2 + plot_annotation(tag_levels = "A")

ggsave(here("04_SIREN/Figs/vaccination_demog.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/vaccination_demog.png"), width = 10, height = 6)

p1 <- siren_df_interim4 |>
    filter(
        episode_start >= as_date("2022-09-12") & !is.na(infection_date_1),
        !is.na(covid_symptoms),
    ) |>
    arrange(study_id, specimen_date) |>
    distinct(study_id, episode_start, episode_end, .keep_all = TRUE) |>
    mutate(
        covid_symptoms = if_else(covid_symptoms %in% c("Asymptomatic", "Other symptoms"), "Non-COVID symptoms or asymptomatic", covid_symptoms),
        covid_symptoms = factor(covid_symptoms, levels = c("Non-COVID symptoms or asymptomatic", "COVID symptoms"))
    ) |>
    ggplot() +
    aes(vaccine_short, fill = covid_symptoms) +
    geom_bar(stat = "count", position = position_fill()) +
    labs(
        y = "",
        x = "Vaccination status",
        fill = ""
    ) +
    scale_y_continuous(
        labels = label_percent(),
        expand = expansion(mult = c(0, 0))
    ) +
    theme_minimal(15) +
    scale_fill_manual(values = thesis_col[seq(2, length(thesis_col), 6)]) +
    guides(fill = guide_legend(reverse = T)) +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10))
    )

p2 <- siren_df_interim4 |>
    filter(
        episode_start >= as_date("2022-09-12") & !is.na(infection_date_1),
        !is.na(covid_symptoms)
    ) |>
    arrange(study_id, specimen_date) |>
    distinct(study_id, episode_start, episode_end, .keep_all = TRUE) |>
    filter(months_since_pos != "No evidence of infection") |>
    mutate(
        months_since_pos = factor(months_since_pos, labels = c("2+\nyears", "1-2\nyears", "6-12\nmonths", "0-6\nmonths", "Confirmed\nnaive")),
        months_since_pos = fct_relevel(months_since_pos, "Confirmed\nnaive"),
        covid_symptoms = if_else(covid_symptoms %in% c("Asymptomatic", "Other symptoms"), "Non-COVID symptoms or asymptomatic", covid_symptoms),
        covid_symptoms = factor(covid_symptoms, levels = c("Non-COVID symptoms or asymptomatic", "COVID symptoms"))
    ) |>
    ggplot() +
    aes(months_since_pos, fill = covid_symptoms) +
    geom_bar(stat = "count", position = position_fill()) +
    labs(
        y = "",
        x = "Time since previous infection",
        fill = ""
    ) +
    scale_y_continuous(
        labels = label_percent(),
        expand = expansion(mult = c(0, 0))
    ) +
    theme_minimal(15) +
    scale_fill_manual(values = thesis_col[seq(2, length(thesis_col), 6)]) +
    guides(fill = guide_legend(reverse = T)) +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10))
    )

(p1 + p2) + plot_layout(widths = c(1, 2), guides = "collect") & theme(legend.position = "bottom")

ggsave(here("04_SIREN/Figs/symptoms_vaccine_tsp.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/symptoms_vaccine_tsp.png"), width = 10, height = 6)

siren_df_interim4 |>
    select(study_id, months_since_pos, specimen_date) |>
    arrange(study_id, specimen_date) |>
    distinct(study_id, .keep_all = TRUE) |>
    group_by(months_since_pos) |>
    summarise(n = n()) |>
    # add percentage
    mutate(
        pct = n / sum(n),
        period = "Start of follow-up"
    ) |>
    ungroup() |>
    bind_rows(
        siren_df_interim4 |>
            select(study_id, months_since_pos, specimen_date) |>
            arrange(study_id, rev(specimen_date)) |>
            distinct(study_id, .keep_all = TRUE) |>
            group_by(months_since_pos) |>
            summarise(n = n()) |>
            # add percentage
            mutate(
                pct = n / sum(n),
                period = "End of follow-up"
            ) |>
            ungroup()
    ) |>
    filter(months_since_pos != "No evidence of infection") |>
    mutate(
        months_since_pos = fct_relevel(months_since_pos, "0-6 months", "6-12 months", "1-2 years", "2+ years", "Confirmed naive"),
        period = fct_relevel(period, "Start of follow-up", "End of follow-up")
    ) |>
    ggplot() +
    aes(x = months_since_pos, fill = period, y = n, label = scales::percent(pct, accuracy = 0.1)) +
    geom_col(position = position_dodge()) +
    geom_text(size = 4, position = position_dodgenudge(width = 0.9, y = 70)) +
    scale_y_continuous(limits = c(0, 3000), expand = expansion(mult = c(0, 0.05))) +
    labs(title = "", x = "Time since previous infection", y = "", fill = "") +
    theme_minimal(15) +
    theme(
        panel.grid.major.x = element_blank(),
        legend.position = "top"
    ) +
    scale_fill_manual(values = thesis_col[seq(9, length(thesis_col), 2)])

ggsave(here("04_SIREN/Figs/start_end_followup.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/start_end_followup.png"), width = 10, height = 6)
