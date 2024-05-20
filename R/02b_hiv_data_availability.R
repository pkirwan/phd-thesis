# Figures and tables for Chapter 2

# Load libraries
here::i_am("R/02b_hiv_data_availability.R")

library(here)
library(tidyverse)
library(patchwork)
library(ggforce)
library(scales)
library(kableExtra)
library(gtsummary)

# CD4 trajectory
# replicate the CD4 trajectory figure of Fauci et al.
cd4_counts_week <- tribble(
    ~week, ~cd4, ~vl,
    1, 1030, 10,
    6, 320, 1100,
    9, NA, 500,
    10, 670, NA,
    12, 660, 520
    # 14.9, 610
)

cd4_counts_year <- tribble(
    ~year, ~cd4, ~vl,
    1, 850, 320,
    8, NA, 850,
    9, 6, 1000,
    11, 5, 100000
)

cd4_week <- cd4_counts_week |>
    ggplot() +
    aes(week, cd4) +
    geom_smooth(se = FALSE, color = thesis_col[11], linewidth = 1.2) +
    geom_smooth(aes(y = vl), se = FALSE, color = thesis_col[3], linewidth = 1.2) +
    # geom_point(color = thesis_col[11], shape = 15, size = 3) +
    # geom_ellipse(aes(x0 = 6, y0 = 320, a = 1.1, b = 60, angle = 0), color = thesis_col[3]) +
    scale_y_continuous(
        # limits = c(0, 1200),
        breaks = seq(0, 1200, 100),
        expand = expansion(mult = c(0, 0))
    ) +
    scale_x_continuous(
        # limits = c(0, 13),
        breaks = c(0, 3, 6, 9, 12),
        expand = expansion(mult = c(0.05, 0))
    ) +
    labs(x = "Weeks", y = expression(paste("CD4+ T Lymphocyte Count (cells/m", m^3, ")"))) +
    theme_minimal(15) +
    theme(
        panel.grid = element_blank(),
        axis.line = element_line(),
        axis.ticks = element_line(),
        plot.margin = margin(5, 0, 5, 5)
    ) +
    annotate("text", y = 0, x = 13, label = "/") +
    coord_cartesian(clip = "off", xlim = c(0, 13), ylim = c(0, 1200))

cd4_year <- cd4_counts_year |>
    ggplot() +
    aes(year, vl) +
    geom_smooth(se = FALSE, aes(color = "HIV viral load"), linewidth = 1.2) +
    geom_smooth(aes(y = cd4, color = "CD4 cell count"), se = FALSE, linewidth = 1.2) +
    # geom_point(color = thesis_col[11], shape = 15, size = 3) +
    scale_y_continuous(
        limits = c(3, 100000),
        breaks = c(0, 10, 100, 1000, 10000, 100000),
        expand = expansion(mult = c(0, 0)),
        position = "right",
        trans = "log10",
        labels = trans_format("log10", math_format(10^.x))
    ) +
    scale_x_continuous(
        breaks = seq(1, 11, 1),
        expand = expansion(mult = c(0, 0.05))
    ) +
    labs(x = "Years", y = "HIV viral load (copies/mL)", color = "") +
    theme_minimal(15) +
    theme(
        panel.grid = element_blank(),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        # axis.text.y = element_blank(),
        plot.margin = margin(5, 5, 5, 5),
        legend.position = c(0.4, 0.8),
        legend.text = element_text(size = 15)
    ) +
    annotate("text", y = 0, x = 0.5, label = "/") +
    coord_cartesian(clip = "off") +
    scale_color_manual(values = thesis_col[c(11, 3)])

cd4_week + cd4_year + plot_layout(widths = c(1, 2))

ggsave(here("02_HIVBackCalc/Figs/cd4_trajectory.pdf"), width = 10, height = 6)
ggsave(here("02_HIVBackCalc/Figs/cd4_trajectory.png"), width = 10, height = 6)


# RITA test availability by CD4 strata
load(here("Data/hiv_msm_2023.RData"))

p1 <- msm |>
    filter(earliesteventyear > 2010, earliesteventyear < 2020) |>
    mutate(assay_test = ifelse(assay_test == "Assay result not available", "", assay_test)) |>
    ggplot() +
    aes(earliesteventyear, fill = assay_test) +
    geom_bar(position = position_fill()) +
    guides(fill = guide_legend(reverse = T)) +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.01))) +
    scale_x_continuous(breaks = seq(2011, 2019, 2)) +
    labs(x = "Year of HIV diagnosis", fill = "", y = "") +
    theme_minimal(15) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
    theme(legend.position = "top") +
    scale_fill_manual(values = c(NA, thesis_col_purple[7]),na.value = NA, na.translate = FALSE)

p2 <- msm |>
    filter(earliesteventyear > 2010, earliesteventyear < 2020) |>
    ggplot() +
    aes(earliesteventyear, group = cd4cat, fill = cd4cat) +
    geom_bar(position = position_fill(reverse = TRUE)) +
    facet_grid(cols = vars(assay_test)) +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.01))) +
    scale_x_continuous(breaks = seq(2011, 2019, 2)) +
    labs(x = "Year of HIV diagnosis", fill = "CD4 strata", y = "") +
    theme_minimal(15) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme(legend.position = "top") +
    scale_fill_manual(values = c(thesis_col_blue[seq(1, length(thesis_col_blue), 2)], "grey")) +
    guides(fill = guide_legend(nrow = 2))

p3 <- msm |>
    filter(earliesteventyear > 2010, earliesteventyear < 2020, assay_test == "Assay result available") |>
    count(earliesteventyear, test_type, cd4cat, recent) |>
    group_by(earliesteventyear, test_type, cd4cat) |>
    mutate(percentage = n / sum(n)) |>
    filter(recent == "Y" & test_type != "") |>
    ggplot() +
    aes(x = earliesteventyear, y = percentage, color = cd4cat, linetype = test_type) +
    geom_line(linewidth = 1) +
    geom_point() +
    scale_y_continuous(labels = scales::percent_format(scale = 100, accuracy = 1), limits = c(0, 0.5)) +
    scale_x_continuous(breaks = seq(2011, 2019, 2)) +
    labs(x = "Year of HIV diagnosis", color = "CD4 strata", lty = "HIV avidity assay", y = "Proportion of diagnoses recently acquired") +
    theme_minimal(15) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_colour_manual(values = c(thesis_col_blue[seq(1, length(thesis_col_blue), 2)], "grey"), guide = "none")

p4 <- msm |>
    filter(earliesteventyear > 2010, earliesteventyear < 2020, assay_test == "Assay result available", cd4cat != "No CD4") |>
    dplyr::count(earliesteventyear, cd4cat, agegpdiag, recent) |>
    group_by(earliesteventyear, cd4cat, agegpdiag) |>
    mutate(percentage = n / sum(n)) |>
    ungroup() |>
    complete(earliesteventyear, cd4cat, agegpdiag, recent, fill = list("percentage" = 0)) |>
    filter(recent == "Y", agegpdiag != "<15", cd4cat != "No CD4") |>
    ggplot() +
    aes(x = earliesteventyear, y = percentage, colour = cd4cat) +
    geom_line(linewidth = 1) +
    geom_point() +
    # geom_bar(position = position_fill(reverse = "TRUE")) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = seq(2011, 2019, 2)) +
    facet_grid(~agegpdiag) +
    labs(x = "Year of HIV diagnosis", colour = "CD4 strata", y = "Proportion of diagnoses recently acquired", caption = "'No CD4' group not shown") +
    theme_minimal(15) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme(legend.position = "top", panel.grid.minor = element_blank()) +
    scale_colour_manual(values = thesis_col_blue[seq(1, length(thesis_col_blue), 2)])

(free(p1) + free(p2) + plot_layout(widths = c(1,2))) / p3 / p4 + plot_annotation(tag_levels = "A") + plot_layout(heights = c(2, 1, 1))

ggsave(here("02_HIVBackCalc/Figs/rita_availability_by_cd4.pdf"), width = 10, height = 13)
ggsave(here("02_HIVBackCalc/Figs/rita_availability_by_cd4.png"), width = 10, height = 13)

# last negative test by CD4 strata

p5 <- msm |>
    filter(earliesteventyear > 1994, earliesteventyear < 2020) |>
    ggplot() +
    aes(x = earliesteventyear, fill = lasttestyear) +
    geom_bar(position = position_fill(reverse = TRUE)) +
    guides(fill = guide_legend(reverse = T)) +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.01))) +
    scale_x_continuous(breaks = seq(1995, 2022, 2)) +
    labs(x = "Year of HIV diagnosis", fill = "Time since last negative test", y = "") +
    guides(fill = "none") +
    theme_minimal(15) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_fill_manual(values = thesis_col_red[c(7, 5, 3, 1)])

p6 <- msm |>
    filter(earliesteventyear > 2010, earliesteventyear < 2020) |>
    ggplot() +
    aes(x = earliesteventyear, fill = lasttestyear) +
    geom_bar(position = position_fill(reverse = TRUE)) +
    facet_grid(~agegpdiag) +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.01))) +
    scale_x_continuous(breaks = seq(2011, 2019, 2)) +
    guides(fill = guide_legend(reverse = T)) +
    labs(x = "Year of HIV diagnosis", fill = "", y = "") +
    theme_minimal(15) +
    theme(legend.position = "top") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_fill_manual(values = thesis_col_red[c(7, 5, 3, 1)]) +
    guides(fill = guide_legend(nrow = 2))

p7 <- msm |>
    filter(earliesteventyear > 2010, earliesteventyear < 2020, cd4cat != "No CD4") |>
    count(earliesteventyear, cd4cat, agegpdiag, lasttestyear) |>
    group_by(earliesteventyear, cd4cat, agegpdiag) |>
    mutate(percentage = n / sum(n)) |>
    ungroup() |>
    complete(earliesteventyear, cd4cat, agegpdiag, lasttestyear, fill = list("percentage" = 0)) |>
    filter(lasttestyear == "Negative test <6 months", agegpdiag != "<15", cd4cat != "No CD4") |>
    ggplot() +
    aes(x = earliesteventyear, y = percentage, color = cd4cat) +
    geom_line(linewidth = 1) +
    geom_point() +
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.5)) +
    scale_x_continuous(breaks = seq(2011, 2019, 2)) +
    facet_grid(~agegpdiag) +
    labs(x = "Year of HIV diagnosis", color = "CD4 strata", y = "Proportion with last negative test within 6 months", caption = "'No CD4' group not shown") +
    theme_minimal(15) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_colour_manual(values = thesis_col_blue[seq(1, length(thesis_col_blue), 2)])

(p5 / p6 / p7) + plot_annotation(tag_levels = "A")

ggsave(here("02_HIVBackCalc/Figs/lntest_by_cd4.pdf"), width = 10, height = 13)
ggsave(here("02_HIVBackCalc/Figs/lntest_by_cd4.png"), width = 10, height = 13)

# viral load test availability by CD4 strata

p8 <- msm |>
    filter(earliesteventyear > 2010, earliesteventyear < 2020) |>
    mutate(vl_avail = ifelse(vl_avail == "Baseline VL not available", "", vl_avail)) |>
    ggplot() +
    aes(earliesteventyear, fill = vl_avail) +
    geom_bar(position = position_fill()) +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.01))) +
    scale_x_continuous(breaks = seq(2011, 2019, 2)) +
    labs(x = "Year of HIV diagnosis", fill = "", y = "") +
    theme_minimal(15) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme(legend.position = "top") +
    scale_fill_manual(values = c(NA, thesis_col_green[7]), na.translate = FALSE)

p9 <- msm |>
    filter(earliesteventyear > 2010, earliesteventyear < 2020) |>
    ggplot() +
    aes(earliesteventyear, group = cd4cat, fill = cd4cat) +
    geom_bar(position = position_fill(reverse = TRUE)) +
    facet_grid(cols = vars(vl_avail)) +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.01))) +
    scale_x_continuous(breaks = seq(2011, 2019, 2)) +
    labs(x = "Year of HIV diagnosis", fill = "CD4 strata", y = "") +
    theme_minimal(15) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme(legend.position = "top") +
    scale_fill_manual(values = c(thesis_col_blue[seq(1, length(thesis_col_blue), 2)], "grey")) +
    guides(fill = guide_legend(nrow = 2))

p10 <- msm |>
    filter(earliesteventyear > 2010, earliesteventyear < 2020, vl_avail == "Baseline VL available", cd4cat != "No CD4") |>
    rename(`CD4 strata` = cd4cat) |>
    ggplot() +
    aes(x = agegpdiag, fill = vlcat) +
    geom_bar(position = position_fill(reverse = TRUE)) +
    facet_grid(~`CD4 strata`, labeller = label_both) +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.01))) +
    # scale_x_continuous(breaks = seq(2011, 2019, 2)) +
    labs(x = "Age at HIV diagnosis", fill = "Baseline VL", y = "", caption = "'No CD4' group not shown") +
    theme_minimal(15) +
    theme(legend.position = "bottom") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_fill_manual(values = thesis_col_green[seq(1, length(thesis_col_green), 2)])

p11 <- msm |>
    filter(earliesteventyear > 2010, earliesteventyear < 2020, vl_avail == "Baseline VL available", cd4cat != "No CD4") |>
    count(earliesteventyear, cd4cat, vlcat) |>
    group_by(earliesteventyear, cd4cat) |>
    mutate(percentage = n / sum(n)) |>
    ungroup() |>
    complete(earliesteventyear, cd4cat, vlcat, fill = list("percentage" = 0)) |>
    filter(cd4cat != "No CD4") |>
    rename(`CD4 strata` = cd4cat) |>
    ggplot() +
    aes(x = earliesteventyear, y = percentage, color = vlcat) +
    geom_line(linewidth = 1) +
    geom_point() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = seq(2011, 2019, 2)) +
    facet_grid(~`CD4 strata`, labeller = label_both) +
    labs(x = "Year of HIV diagnosis", color = "Baseline VL", y = "Proportion of diagnoses", caption = "'No CD4' group not shown") +
    theme_minimal(15) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_colour_manual(values = thesis_col_green[seq(1, length(thesis_col_green), 2)])

(free(p8) + free(p9) + plot_layout(widths = c(1,2))) / p10 / p11 + plot_annotation(tag_levels = "A") + plot_layout(heights = c(2, 1, 1))

ggsave(here("02_HIVBackCalc/Figs/vl_availability_by_cd4.pdf"), width = 10, height = 13)
ggsave(here("02_HIVBackCalc/Figs/vl_availability_by_cd4.png"), width = 10, height = 13)

p12 <- msm |>
    filter(
        earliesteventyear > 2010, earliesteventyear < 2020,
        vl_avail == "Baseline VL available",
        cd4cat != "No CD4", recent != ""
    ) |>
    mutate(recent = factor(recent, levels = c("Y", "N"), labels = c("Recent", "Non-recent"))) |>
    ggplot() +
    aes(y = reorder(recent, desc(recent)), fill = vlcat) +
    geom_bar(position = position_fill(reverse = TRUE)) +
    scale_x_continuous(labels = scales::percent) +
    facet_grid(rows = vars(cd4cat)) +
    labs(x = "Proportion", fill = "Baseline VL", y = "") +
    theme_minimal(15) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_fill_manual(values = thesis_col_green[seq(1, length(thesis_col_green), 2)])

p13 <- msm |>
    filter(
        earliesteventyear > 2010, earliesteventyear < 2020,
        vl_avail == "Baseline VL available",
        cd4cat != "No CD4"
    ) |>
    ggplot() +
    aes(y = reorder(lasttestyear, desc(lasttestyear)), fill = vlcat) +
    geom_bar(position = position_fill(reverse = TRUE)) +
    scale_x_continuous(labels = scales::percent) +
    facet_grid(rows = vars(cd4cat)) +
    labs(x = "Proportion", fill = "", y = "", caption = "'No CD4' group not shown") +
    theme_minimal(15) +
    theme(legend.position = "none", panel.grid.minor = element_blank()) +
    scale_fill_manual(values = thesis_col_green[seq(1, length(thesis_col_green), 2)])

p12 / p13 + plot_annotation(tag_levels = "A") + plot_layout(heights = c(2, 3))

ggsave(here("02_HIVBackCalc/Figs/vl_cd4_recent.pdf"), width = 10, height = 13)
ggsave(here("02_HIVBackCalc/Figs/vl_cd4_recent.png"), width = 10, height = 13)


# Late diagnosis reclassification figure
load("Data/hiv_ld_paper.RData")

p1 <- nd_archive |>
    filter(!is.na(pexpgp), !is.na(reclassifygp)) |>
    bind_rows(nd_archive |> mutate(pexpgp = "All") |> filter(!is.na(reclassifygp))) |>
    mutate(
        pexpgp = factor(pexpgp, levels = c("All", "Sex between men", "Heterosexual contact (men)", "Heterosexual contact (women)")),
        reclassifygp = factor(reclassifygp, labels = c("CD4\u2265350", "CD4<350, reclassified", "CD4<350, not reclassified"))
    ) |>
    ggplot() +
    aes(earliesteventyear, fill = reclassifygp) +
    geom_bar(position = position_fill(reverse = FALSE)) +
    # geom_text(aes(earliesteventyear, prop, label = scales::percent(prop, accuracy = 1), fill = NULL, color = reclassifygp),
    #     data = proportions,
    #     position = position_fill(vjust = 0.5, reverse = FALSE), size = 3.5
    # ) +
    geom_text(aes(earliesteventyear, 1.05, label = total, fill = NULL), data = totals, size = 3.5) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
    ) +
    facet_wrap(~pexpgp) +
    scale_x_continuous(breaks = seq(2011, 2019, 1)) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.03)), labels = percent_format()) +
    labs(color = "", fill = "", x = "Year of HIV diagnosis", y = "Proportion", subtitle = "A") +
    theme(
        legend.position = "top",
        text = element_text(size = 15)
    ) +
    scale_fill_manual(values = c(thesis_col[12], thesis_col[8], thesis_col[5])) +
    scale_color_manual(values = c("white", "black", "black"), guide = "none")

p2 <- nd_archive |>
    filter(!is.na(pexpgp), reclassifygp == "Reclassified") |>
    bind_rows(nd_archive |> mutate(pexpgp = "All") |> filter(reclassifygp == "Reclassified")) |>
    mutate(
        pexpgp = factor(pexpgp, levels = c("All", "Sex between men", "Heterosexual contact (men)", "Heterosexual contact (women)")),
        reclassify = factor(reclassify, levels = c("Reclassified (RITA + Negative test)", "Reclassified (RITA)", "Reclassified (Negative test)"))
    ) |>
    ggplot() +
    aes(earliesteventyear, fill = reclassify) +
    geom_bar(position = position_stack(reverse = TRUE)) +
    geom_point(data = df2, aes(x = earliesteventyear, y = value), alpha = 0, fill = "white") +
    # geom_text(aes(earliesteventyear, total, label = total, fill = NULL),
    #     data = counts,
    #     position = position_stack(vjust = 0.5, reverse = FALSE), size = 3.5
    # ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
    ) +
    facet_wrap(~pexpgp, scales = "free_y") +
    scale_x_continuous(breaks = seq(2011, 2019, 1)) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.03))) +
    labs(color = "", fill = "", x = "Year of HIV diagnosis", y = "Number reclassified", subtitle = "B") +
    theme(
        legend.position = "top",
        text = element_text(size = 15)
    ) +
    scale_fill_manual(values = c(thesis_col[11], thesis_col[9], thesis_col[7]))

(p1 / p2)

ggsave(here("02_HIVBackCalc/Figs/latediag.pdf"), width = 10, height = 13)
ggsave(here("02_HIVBackCalc/Figs/latediag.png"), width = 10, height = 13)
