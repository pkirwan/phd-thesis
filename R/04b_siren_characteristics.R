# Figures and tables for Chapter 4

# Load libraries
here::i_am("R/04b_siren_characteristics.R")
library(here)
library(tidyverse)
library(patchwork)
library(scales)
library(ggpp)

load(here("Data/siren_cohort.RData"))

earliest_date <- min(full_cohort$date_enrolled)
latest_date <- as_date("2023-03-31")

week_cohort <- expand.grid(
    date = seq(earliest_date, latest_date, by = "1 week"),
    study_id = unique(full_cohort$study_id)
) |>
    left_join(
        full_cohort |> select(study_id, date_enrolled, study_end_date, vaccine_date1, vaccine_date2, vaccine_date3, vaccine_date4) |> distinct(),
        by = "study_id"
    ) |>
    filter(
        date >= date_enrolled,
        date <= study_end_date
    ) |>
    mutate(
        vaccine = case_when(
            date < vaccine_date1 | is.na(vaccine_date1) ~ "",
            date < vaccine_date2 | is.na(vaccine_date2) ~ "First dose",
            date < vaccine_date3 | is.na(vaccine_date3) ~ "Second dose",
            date < vaccine_date4 | is.na(vaccine_date4) ~ "Third dose",
            TRUE ~ "Fourth dose"
        ),
        vaccine = factor(vaccine, levels = c("", "First dose", "Second dose", "Third dose", "Fourth dose")),
        cohort = case_when(
            date - date_enrolled <= 370 ~ "Initial cohort",
            date - date_enrolled <= 733 ~ "First extension",
            TRUE ~ "Second extension"
        ),
        cohort = factor(cohort, levels = c("Initial cohort", "First extension", "Second extension"))
    )

p1 <- week_cohort |>
    ggplot() +
    aes(date, fill = vaccine) +
    geom_histogram(
        position = position_fill(),
        binwidth = 7
    ) +
    scale_y_continuous(
        labels = label_percent()
    ) +
    coord_cartesian(ylim = c(0, 1), clip = "off", expand = FALSE) +
    scale_x_date(
        breaks = breaks_pretty(n = 12)
        # date_labels = "%b %Y"
    ) +
    labs(x = "", y = "Proportion vaccinated", title = "", fill = "Vaccine dose") +
    theme_minimal(15) +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        legend.box.margin = margin(0, 0, 20, 0)
    ) +
    scale_fill_manual(values = c(NA, thesis_col_green[seq(1, length(thesis_col_green), 2)]), na.value = NA, na.translate = FALSE)

p2 <- full_cohort |>
    select(study_id, vaccine_date1, vaccine_date2, vaccine_date3, vaccine_date4) |>
    pivot_longer(-study_id, names_to = "vaccine", values_to = "date") |>
    mutate(vaccine = factor(vaccine, labels = c("First dose", "Second dose", "Third dose", "Fourth dose"))) |>
    filter(!is.na(date)) |>
    ggplot() +
    aes(date, fill = vaccine) +
    geom_histogram(binwidth = 7) +
    labs(y = "Number vaccinated", x = "", fill = "Vaccine dose") +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_date(
        limits = c(earliest_date, as_date("2023-04-01")),
        breaks = breaks_pretty(n = 12),
        expand = expansion(mult = c(0, 0))
    ) +
    theme_minimal(15) +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.position = "none"
    ) +
    scale_fill_manual(values = thesis_col_green[seq(1, length(thesis_col_green), 2)])

p1 / p2 + plot_annotation(tag_levels = "A")

ggsave(here("04_SIREN/Figs/proportion_vaccinated.pdf"), width = 10, height = 13)
ggsave(here("04_SIREN/Figs/proportion_vaccinated.png"), width = 10, height = 13)

week_cohort |>
    ggplot() +
    aes(date, fill = cohort) +
    geom_histogram(
        position = position_stack(),
        binwidth = 7
    ) +
    # annotate("text", label = "Wild type", x = as_date("2020-09-01"), y = 46500) +
    # annotate("segment", colour = thesis_col[8], linewidth = 1, x = as_date("2020-06-15"), xend = as_date("2020-12-01"), y = 45000, yend = 45000) +
    # annotate("text", label = "Alpha", x = as_date("2021-02-20"), y = 46500) +
    # annotate("segment", colour = thesis_col[9], linewidth = 1, x = as_date("2020-12-15"), xend = as_date("2021-05-01"), y = 45000, yend = 45000) +
    # annotate("text", label = "Delta", x = as_date("2021-08-15"), y = 46500) +
    # annotate("segment", colour = thesis_col[10], linewidth = 1, x = as_date("2021-05-15"), xend = as_date("2021-12-01"), y = 45000, yend = 45000) +
    # annotate("text", label = "BA.1", x = as_date("2022-01-15"), y = 46500) +
    # annotate("segment", colour = thesis_col[11], linewidth = 1, x = as_date("2021-12-15"), xend = as_date("2022-02-15"), y = 45000, yend = 45000) +
    # annotate("text", label = "BA.2", x = as_date("2022-04-15"), y = 46500) +
    # annotate("segment", colour = thesis_col[12], linewidth = 1, x = as_date("2022-03-01"), xend = as_date("2022-06-01"), y = 45000, yend = 45000) +
    # annotate("text", label = "BA.4/5", x = as_date("2022-08-05"), y = 46500) +
    # annotate("segment", colour = thesis_col[1], linewidth = 1, x = as_date("2022-06-15"), xend = as_date("2022-10-01"), y = 45000, yend = 45000) +
    # annotate("text", label = "BQ.1/CH.1.1/XBB.1.5", x = as_date("2023-01-05"), y = 46500) +
    # annotate("segment", colour = thesis_col[2], linewidth = 1, x = as_date("2022-10-15"), xend = as_date("2023-03-31"), y = 45000, yend = 45000) +
    coord_cartesian(ylim = c(0, 45000), clip = "off", expand = FALSE) +
    scale_x_date(
        breaks = breaks_pretty(n = 6),
        date_labels = "%b %Y"
    ) +
    labs(x = "", y = "Number of participants", title = "", fill = "") +
    theme_minimal(15) +
    theme(
        # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.position = "top",
        legend.box.margin = margin(0, 0, 20, 0)
    ) +
    scale_fill_manual(values = thesis_col[seq(5, length(thesis_col), 3)])

ggsave(here("04_SIREN/Figs/participants.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/participants.png"), width = 10, height = 6)

pcr |>
    mutate(
        week = floor_date(specimen_date, "week")
    ) |> # only primary and reinfections
    filter(prim == 1 | rein == 1) |>
    ggplot() +
    aes(week) +
    geom_line(
        stat = "count",
        position = position_stack(reverse = TRUE),
        colour = thesis_col[3]
    ) +
    annotate("text", label = "Wild type", x = as_date("2020-08-01"), y = 850) +
    annotate("segment", colour = thesis_col[8], linewidth = 1, x = as_date("2020-03-15"), xend = as_date("2020-12-01"), y = 820, yend = 820) +
    annotate("text", label = "Alpha", x = as_date("2021-02-20"), y = 850) +
    annotate("segment", colour = thesis_col[9], linewidth = 1, x = as_date("2020-12-15"), xend = as_date("2021-05-01"), y = 820, yend = 820) +
    annotate("text", label = "Delta", x = as_date("2021-08-15"), y = 850) +
    annotate("segment", colour = thesis_col[10], linewidth = 1, x = as_date("2021-05-15"), xend = as_date("2021-12-01"), y = 820, yend = 820) +
    annotate("text", label = "BA.1", x = as_date("2022-01-15"), y = 850) +
    annotate("segment", colour = thesis_col[11], linewidth = 1, x = as_date("2021-12-15"), xend = as_date("2022-02-15"), y = 820, yend = 820) +
    annotate("text", label = "BA.2", x = as_date("2022-04-15"), y = 850) +
    annotate("segment", colour = thesis_col[12], linewidth = 1, x = as_date("2022-03-01"), xend = as_date("2022-06-01"), y = 820, yend = 820) +
    annotate("text", label = "BA.4/5", x = as_date("2022-08-05"), y = 850) +
    annotate("segment", colour = thesis_col[1], linewidth = 1, x = as_date("2022-06-15"), xend = as_date("2022-10-01"), y = 820, yend = 820) +
    annotate("text", label = "BQ.1/XBB.1.5", x = as_date("2023-01-05"), y = 850) +
    annotate("segment", colour = thesis_col[2], linewidth = 1, x = as_date("2022-10-15"), xend = as_date("2023-03-31"), y = 820, yend = 820) +
    coord_cartesian(ylim = c(0, 820), clip = "off", expand = FALSE) +
    scale_y_continuous(
        expand = expansion(mult = c(0, 0.05))
    ) +
    scale_x_date(
        breaks = breaks_pretty(n = 12),
        date_labels = "%b %Y"
    ) +
    labs(x = "", y = "Number of infections", title = "", fill = "") +
    theme_minimal(15) +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor.x = element_blank(),
        legend.position = "top",
        plot.margin = margin(20, 10, 10, 10)
    )

ggsave(here("04_SIREN/Figs/infections.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/infections.png"), width = 10, height = 6)

fig_gp <- full_cohort |>
    filter(study_end_date >= as_date("2022-06-01")) |>
    select(date_enrolled, study_id, vaccine_date1, vaccine_date2, vaccine_date3, vaccine_date4) |>
    sample_n(25)

# remove large numbers
fig_gp <- fig_gp |>
    filter(
        !study_id %in% (pcr |> group_by(study_id) |> count() |> filter(n > 80) |> pull(study_id))
    )

fig_gp |>
    ggplot() +
    aes(date_enrolled, reorder(study_id, date_enrolled)) +
    geom_segment(
        aes(
            yend = study_id,
            xend = if_else(is.na(vaccine_date1), lubridate::ymd("2023-03-01"), if_else(vaccine_date1 > date_enrolled, vaccine_date1, date_enrolled)),
            color = "Unvaccinated", linetype = "Unvaccinated"
        ),
        linewidth = 3, alpha = 0.8
    ) +
    geom_segment(
        aes(
            yend = study_id,
            x = if_else(vaccine_date1 < date_enrolled, date_enrolled, vaccine_date1),
            xend = if_else(is.na(vaccine_date2), lubridate::ymd("2023-03-01"), if_else(vaccine_date2 > date_enrolled & vaccine_date1 < vaccine_date2, vaccine_date2, date_enrolled)),
            color = "First dose", linetype = "First dose"
        ),
        linewidth = 3, alpha = 0.8
    ) +
    geom_segment(
        aes(
            yend = study_id,
            x = if_else(vaccine_date2 < date_enrolled, date_enrolled, vaccine_date2),
            xend = if_else(vaccine_date2 < vaccine_date3 & !is.na(vaccine_date3), vaccine_date3, lubridate::ymd("2023-03-01")),
            color = "Second dose", linetype = "Second dose"
        ),
        linewidth = 3, alpha = 0.8
    ) +
    geom_segment(
        aes(
            yend = study_id,
            x = if_else(vaccine_date3 < date_enrolled, date_enrolled, vaccine_date3),
            xend = if_else(vaccine_date3 < vaccine_date4 & !is.na(vaccine_date4), vaccine_date4, lubridate::ymd("2023-03-01")),
            color = "Third dose", linetype = "Third dose"
        ),
        linewidth = 3, alpha = 0.8
    ) +
    geom_segment(
        aes(
            yend = study_id,
            x = if_else(vaccine_date4 < date_enrolled, date_enrolled, vaccine_date4),
            xend = lubridate::ymd("2023-03-01"), color = "Fourth dose", linetype = "Fourth dose"
        ),
        linewidth = 3, alpha = 0.8
    ) +
    geom_point(aes(shape = "Enrolment"), color = "black") +
    geom_point(
        data = pcr |> filter(study_id %in% fig_gp$study_id) |> mutate(specimen_date = lubridate::ymd(specimen_date)),
        aes(specimen_date, study_id, shape = "PCR test"), fill ="white", color = "black", size = 1
    ) +
    scale_y_discrete(labels = NULL) +
    scale_x_date(
        labels = date_format("%b %Y"),
        breaks = seq(lubridate::ymd("2020-05-01"), lubridate::ymd("2023-03-01"), by = "6 months"),
        limits = c(lubridate::ymd("2020-05-01"), lubridate::ymd("2023-03-01"))
    ) +
    theme_minimal(15) +
    theme(
        legend.position = "top",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank()
    ) +
    scale_color_manual(
        breaks = c("Unvaccinated", "First dose", "Second dose", "Third dose", "Fourth dose"),
        values = c("lightgrey", thesis_col_green[seq(1, length(thesis_col_green), 2)])
    ) +
    scale_shape_manual(values = c(
        "Enrolment" = 19,
        "PCR test" = 24,
        "Unvaccinated" = 0, "First dose" = 0, "Second dose" = 0, "Third dose" = 0, "Fourth dose" = 0
    ), guide = "none") +
    scale_linetype_manual(values = c(
        "Enrolment" = 0,
        "Unvaccinated" = 1, "First dose" = 1, "Second dose" = 1, "Third dose" = 1, "Fourth dose" = 1
    ), guide = "none") +
    labs(title = "", y = "", x = "", color = "")

ggsave(here("04_SIREN/Figs/siren_timeline.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/siren_timeline.png"), width = 10, height = 6)

p1 <- symptoms |>
    left_join(
        full_cohort |> select(study_id, agegr, gender),
        by = "study_id"
    ) |>
    filter(
        !is.na(covid_symptoms), !is.na(gender)
    ) |>
    mutate(
        covid_symptoms = if_else(covid_symptoms %in% c("Asymptomatic", "Other symptoms"), "Non-COVID symptoms or asymptomatic", covid_symptoms),
        covid_symptoms = factor(covid_symptoms, levels = c("Non-COVID symptoms or asymptomatic", "COVID symptoms")),
        circulating_variant = case_when(
            specimen_date < as_date("2020-12-01") ~ "Wild type",
            specimen_date < as_date("2021-05-01") ~ "Alpha",
            specimen_date < as_date("2021-12-01") ~ "Delta",
            specimen_date < as_date("2022-02-15") ~ "BA.1",
            specimen_date < as_date("2022-06-01") ~ "BA.2",
            specimen_date < as_date("2022-10-01") ~ "BA.4/5",
            specimen_date < as_date("2023-03-31") ~ "BQ.1/XBB.1.5",
            TRUE ~ "Unknown"
        ),
        circulating_variant = factor(circulating_variant, levels = c("Wild type", "Alpha", "Delta", "BA.1", "BA.2", "BA.4/5", "BQ.1/XBB.1.5"))
    ) |>
    filter(circulating_variant != "Unknown") |>
    ggplot() +
    aes(circulating_variant, fill = covid_symptoms) +
    geom_bar(position = position_fill()) +
    labs(
        y = "",
        x = "Variant circulating period",
        fill = ""
    ) +
    scale_y_continuous(
        labels = label_percent(),
        expand = expansion(mult = c(0, 0))
    ) +
    scale_x_discrete() +
    theme_minimal(15) +
    scale_fill_manual(values = thesis_col[seq(2, length(thesis_col), 6)]) +
    guides(fill = guide_legend(reverse = T)) +
    theme(
        legend.position = "top",
        # panel.grid.major.x = element_blank(),
        # panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10)),
        # rotate x labels
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )

p2 <- symptoms |>
    left_join(
        full_cohort |> select(study_id, agegr, gender),
        by = "study_id"
    ) |>
    # mutate(agegr = fct_relevel(agegr, "<25", "25-34", "35-44")) |>
    filter(
        !is.na(covid_symptoms), !is.na(gender)
    ) |>
    mutate(
        covid_symptoms = if_else(covid_symptoms %in% c("Asymptomatic", "Other symptoms"), "Non-COVID symptoms or asymptomatic", covid_symptoms),
        covid_symptoms = factor(covid_symptoms, levels = c("Non-COVID symptoms or asymptomatic", "COVID symptoms"))
    ) |>
    ggplot() +
    aes(agegr, fill = covid_symptoms) +
    geom_bar(stat = "count", position = position_fill()) +
    labs(
        y = "",
        x = "Age group",
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
        legend.position = "",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    ) +
    facet_grid(~gender)

p1 + p2 + plot_layout(widths = c(1, 2), guides = "collect") + plot_annotation(tag_levels = "A") & theme(legend.position = "bottom")

ggsave(here("04_SIREN/Figs/symptoms_age_sex.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/symptoms_age_sex.png"), width = 10, height = 6)


p1 <- full_cohort |>
    ggplot() +
    aes(staff_type, fill = gender) +
    geom_bar(stat = "count", position = position_stack(reverse = TRUE)) +
    scale_y_continuous(
        expand = expansion(mult = c(0, 0.05))
    ) +
    scale_x_discrete(limits = rev) +
    coord_flip() +
    theme_minimal(15) +
    theme(
        legend.position = "top",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank()
    ) +
    labs(x = "", y = "", fill = "Gender") +
    scale_fill_manual(values = thesis_col[seq(3, length(thesis_col), 4)])

# household type
p2 <- full_cohort |>
    ggplot() +
    aes(agegr, fill = household) +
    geom_bar(stat = "count", position = position_stack(reverse = TRUE)) +
    labs(y = "", x = "Age group", fill = "Household structure") +
    scale_y_continuous(expand = expansion(mult = c(0, 0))) +
    theme_minimal(15) +
    # facet_wrap(~gender) +
    scale_fill_manual(values = thesis_col_purple[c(2, 4, 6)]) +
    theme(legend.position = "top")

# free function is in patchwork v1.2.0
free(p1) / p2 + plot_layout(heights = c(3, 2)) + plot_annotation(tag_levels = "A")

ggsave(here("04_SIREN/Figs/occupation_household.pdf"), width = 10, height = 13)
ggsave(here("04_SIREN/Figs/occupation_household.png"), width = 10, height = 13)

fortnightly_testing |>
    filter(!is.na(variant_period)) |>
    ggplot() +
    aes(variant_period, fill = on_schedule) +
    geom_bar(position = position_fill(reverse = TRUE)) +
    labs(
        y = "Proportion undergoing fortnightly PCR testing",
        x = "Variant circulating period",
        fill = ""
    ) +
    scale_y_continuous(
        labels = label_percent(),
        expand = expansion(mult = c(0, 0))
    ) +
    scale_x_discrete() +
    theme_minimal(15) +
    scale_fill_manual(values = c(thesis_col[2], NA), na.value = NA, na.translate = FALSE) +
    theme(
        legend.position = "none",
        # panel.grid.major.x = element_blank(),
        # panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )

ggsave(here("04_SIREN/Figs/fortnightly_schedule.pdf"), width = 10, height = 6)
ggsave(here("04_SIREN/Figs/fortnightly_schedule.png"), width = 10, height = 6)
