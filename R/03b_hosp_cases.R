# Hosp cases and severity
here::i_am("R/03b_hosp_cases.R")

library(here)
library(tidyverse)
library(roll)
library(ggridges)
library(Cairo)

load(here("Data/hosp_cases.RData"))
load(here("Data/hosp_sentinel_data.RData"))

# Create hosp_cases figure

cases |>
    filter(areaName == "England") |>
    arrange(date) |>
    mutate(
        weekly_av = roll_sum(newCasesBySpecimenDate, 7) / 7,
        weekly_av = lead(weekly_av, 4)
    ) |>
    filter(date >= "2020-03-01", date <= "2022-04-30") |>
    ggplot() +
    aes(x = date, y = weekly_av) +
    geom_line(colour = thesis_col[2], linewidth = 1.05) +
    geom_col(
        data = cases |>
            filter(areaName == "England") |>
            filter(date >= "2020-03-01", date <= "2022-04-30") |>
            select(date, newCasesBySpecimenDate),
        aes(x = date, y = newCasesBySpecimenDate), fill = thesis_col[1], alpha = 0.3
    ) +
    labs(
        x = "Date",
        y = "Daily number of confirmed cases"
    ) +
    coord_cartesian(ylim = c(0, 220000), clip = "off", expand = FALSE) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    scale_y_continuous(
        labels = scales::comma,
        expand = expansion(mult = c(0, 0.05))
    ) +
    theme_minimal(15) +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        plot.margin = margin(30, 10, 10, 10)
    ) +
    annotate("text", size = 5, label = "First wave", x = as_date("2020-04-23"), y = 18000) +
    annotate("segment", colour = thesis_col[8], linewidth = 1, x = as_date("2020-03-05"), xend = as_date("2020-06-15"), y = 12000, yend = 12000) +
    annotate("text", size = 5, label = "Second wave", x = as_date("2020-12-15"), y = 83000) +
    annotate("segment", colour = thesis_col[10], linewidth = 1, x = as_date("2020-09-01"), xend = as_date("2021-04-01"), y = 77000, yend = 77000) +
    annotate("text", size = 5, label = "Third wave", x = as_date("2021-11-01"), y = 196000) +
    annotate("segment", colour = thesis_col[12], linewidth = 1, x = as_date("2021-05-20"), xend = as_date("2022-04-20"), y = 190000, yend = 190000)

ggsave(here("03_HospitalSeverity/Figs/pandemic_waves.pdf"), width = 10, height = 6)
ggsave(here("03_HospitalSeverity/Figs/pandemic_waves.png"), width = 10, height = 6)

# sari hosp v icu admission

p1 <- hospsen |>
    filter(hospadmdt <= "2021-02-28") |>
    mutate(outcomestatus = factor(outcomestatus, labels = c("Death", "Discharge", "Still in hospital/ICU", "Transfer", "Unknown final outcome"))) |>
    ggplot() + 
    aes(monthyear, fill = outcomestatus) +
    geom_bar(stat = "count", position = position_fill(reverse = TRUE), width = 0.8) +
    theme_minimal(15) +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    ) +
    scale_y_continuous(
        expand = expansion(mult = c(0, 0.05)),
        labels = scales::percent_format()
    ) +
    theme(
        legend.position = "top",
        panel.grid.major.x = element_blank(),
    ) +
    labs(
        x = "Month of admission",
        y = "Proportion",
        fill = ""
    ) +
    scale_fill_manual(
        values = c(
            thesis_col[c(1, 4)], thesis_col_blue[seq(1,length(thesis_col_blue), 2)])) +
    facet_grid(~ agegroup)

p2 <- hospsen |>
    filter(hospadmdt <= "2021-02-28") |>
    mutate(icu = if_else(icu == "Not admitted to ICU", "", icu)) |>
    ggplot() +
    aes(monthyear, fill = icu) +
    geom_bar(stat = "count", position = position_fill(), width = 0.8) +
    theme_minimal(15) +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    ) +
    scale_y_continuous(
        expand = expansion(mult = c(0, 0.05)),
        labels = scales::percent_format()
    ) +
    coord_cartesian(ylim = c(0, 0.3)) +
    theme(
        legend.position = "none",
        panel.grid.major.x = element_blank(),
    ) +
    labs(
        x = "Month of admission",
        y = "Proportion admitted to ICU",
        fill = ""
    ) +
    scale_fill_manual(values = c(NA, thesis_col[7]), na.value = NA) +
    facet_grid(~ agegroup)

p1 / p2 + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = "A")

ggsave(here("03_HospitalSeverity/Figs/sari_icu_admission.pdf"), width = 10, height = 13)
ggsave(here("03_HospitalSeverity/Figs/sari_icu_admission.png"), width = 10, height = 13)

# sari time distribution

hospsen |>
    mutate(timem = if_else(timem == 0.5, 1, timem + 1)) |>
    filter(!is.na(eventm), !is.na(monthyear)) |>
    ggplot() +
    aes(x = timem, height = after_stat(density), y = monthyear, fill = eventm) +
    geom_density_ridges(
        stat = "binline",
        binwidth = 1,
        scale = 1,
        draw_baseline = FALSE
    ) +
    scale_x_continuous(limits = c(0, 30), breaks = c(0, 10, 20, 30)) +
    scale_y_discrete(expand = expansion(mult = c(0, 0.05)), limits = rev) +
    theme_minimal(15) +
    theme(
        axis.text.y = element_text(vjust = -1.5),
        legend.position = "none"
    ) +
    scale_fill_manual(values = thesis_col[seq(1, length(thesis_col), 3)]) +
    labs(
        x = "Days following hospital admission",
        y = "Month of admission",
        fill = "Outcome"
    ) +
    facet_wrap(~eventm)

ggsave(here("03_HospitalSeverity/Figs/sari_time_distribution.pdf"), width = 10, height = 6)
ggsave(here("03_HospitalSeverity/Figs/sari_time_distribution.png"), width = 10, height = 6)

# sus vaccine month
load(here("Data/hosp_pdata.RData"))

hosp |>
    mutate(ageGrp7 = factor(ageGrp7, labels = c("0-15", "15-25", "25-45", "45-65", "65-75", "75-85", "85+"))) |>
    filter(
        monthyear == "Dec 2020" | grepl("2021", monthyear) | grepl("2022", monthyear),
        monthyear %in% level_order_thinned,
        vaccine != "",
        vaccine != "<21 days after first dose"
    ) |>
    mutate(
        vaccine = fct_drop(vaccine),
        vaccine = factor(vaccine, labels = c("Unvaccinated", "\u226521 days after first dose", "\u226514 days after second dose", "\u226514 days after third dose"))
    ) |>
    group_by(monthyear, ageGrp7, vaccine) |>
    summarise(n = n(), .groups = "drop_last") |>
    mutate(pct = n / sum(n)) |>
    ggplot() +
    aes(monthyear, n, fill = vaccine) +
    geom_col(position = position_fill(), width = 0.7) +
    labs(fill = "", y = "Proportion", x = "") +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0))) +
    scale_x_discrete(breaks = level_order_thinned) +
    theme_minimal(15) +
    theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    ) +
    facet_grid(~ageGrp7) +
    scale_fill_manual(values = c("#f9f9f9", thesis_col[seq(2, length(thesis_col), 4)]))

ggsave(here("03_HospitalSeverity/Figs/sus_vaccine_month.pdf"), width = 10, height = 6, device=cairo_pdf)
ggsave(here("03_HospitalSeverity/Figs/sus_vaccine_month.png"), width = 10, height = 6)

