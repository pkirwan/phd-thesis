# Hosp cases
here::i_am("R/03c_hosp_sari.R")

library(here)
library(tidyverse)
library(flexsurv)
library(patchwork)

load(here("data/hosp_mixcure.RData"))

p1 <- ajfit_month_plot(ajh_month) + facet_grid(~monthofadmission)
p2 <- ajfit_month_plot(aji_month, xlab = "Days after ICU admission") + facet_grid(~monthofadmission)
p3 <- ajfit_month_plot(ajh_sex) + facet_grid(sex ~ monthofadmission)
p4 <- ajfit_month_plot(aji_sex, xlab = "Days after ICU admission") + facet_grid(sex ~ monthofadmission)

(p1 / p3) + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect", heights = c(1, 2)) & theme(legend.position = "bottom")

ggsave(here("03_HospitalSeverity/Figs/sari_gof_hosp.pdf"), width = 10, height = 13)
ggsave(here("03_HospitalSeverity/Figs/sari_gof_hosp.png"), width = 10, height = 13)

(p2 / p4) + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect", heights = c(1, 2)) & theme(legend.position = "bottom")

ggsave(here("03_HospitalSeverity/Figs/sari_gof_icu.pdf"), width = 10, height = 13)
ggsave(here("03_HospitalSeverity/Figs/sari_gof_icu.png"), width = 10, height = 13)

# event probs
p1 <- probs_month_plot(ph_month) + ylab("Probability")
p2 <- probs_month_plot(ph_sex) + facet_grid(~sex)
p3 <- probs_month_plot(ph_age) + facet_grid(~agegroup) + ylab("Probability")
p4 <- probs_month_plot(ph_comorbid) + facet_grid(~comorbid_multip) + ylab("Probability")

(p1 + p2 + plot_layout(widths = c(1, 2))) / p3 / p4 +
    plot_annotation(tag_levels = "A") + plot_layout(guides = "collect", axis_titles = "collect_x") & theme(legend.position = "bottom")

ggsave(here("03_HospitalSeverity/Figs/sari_event_prob.pdf"), width = 10, height = 13)
ggsave(here("03_HospitalSeverity/Figs/sari_event_prob.png"), width = 10, height = 13)

# ICU event probs
p1 <- probs_month_plot_death(pi_month |> filter(event == "Death")) + ylab("Probability")
p2 <- probs_month_plot_death(pi_sex |> filter(event == "Death")) + facet_grid(~sex)
p3 <- probs_month_plot_death(pi_age |> filter(event == "Death")) + facet_grid(~agegroup) + ylab("Probability")
p4 <- probs_month_plot_death(pi_comorbid |> filter(event == "Death")) + facet_grid(~comorbid_multip) + ylab("Probability")

(p1 + p2 + plot_layout(widths = c(1, 2))) / p3 / p4 +
    plot_annotation(tag_levels = "A") + plot_layout(guides = "collect", axis_titles = "collect_x") & theme(legend.position = "none")

ggsave(here("03_HospitalSeverity/Figs/sari_icu_prob.pdf"), width = 10, height = 13)
ggsave(here("03_HospitalSeverity/Figs/sari_icu_prob.png"), width = 10, height = 13)

# mixture probs
p1 <- probs_month_plot_death(pm_month, ylim = 0.6, colour = thesis_col[8]) + theme(legend.position = "none")
p2 <- probs_month_plot_death(pm_sex, ylim = 0.6, colour = thesis_col[8]) + facet_grid(~sex) + ylab("") + theme(legend.position = "none")
p3 <- probs_month_plot_death(pm_age, ylim = 0.6, colour = thesis_col[8]) + facet_grid(~agegroup) + theme(legend.position = "none")
p4 <- probs_month_plot_death(pm_comorbid, ylim = 0.6, colour = thesis_col[8]) + facet_grid(~comorbid_multip) + theme(legend.position = "none")

(p1 + p2 + plot_layout(widths = c(1, 2))) / p3 / p4 +
    plot_annotation(tag_levels = "A") + plot_layout(guides = "collect", axis_titles = "collect_x")

ggsave(here("03_HospitalSeverity/Figs/sari_hfr.pdf"), width = 10, height = 13)
ggsave(here("03_HospitalSeverity/Figs/sari_hfr.png"), width = 10, height = 13)

p1 <- quantile_month_plot(qh_month)
p2 <- quantile_month_plot(qh_sex) + facet_grid(~sex) + ylab("")
p3 <- quantile_month_plot(qh_age) + facet_grid(~agegroup)
p4 <- quantile_month_plot(qh_comorbid) + facet_grid(~comorbid_multip)

(p1 + p2 + plot_layout(widths = c(1, 2))) / p3 / p4 +
    plot_annotation(tag_levels = "A") + plot_layout(guides = "collect", axis_titles = "collect_x") & theme(legend.position = "bottom")

ggsave(here("03_HospitalSeverity/Figs/sari_med_time.pdf"), width = 10, height = 13)
ggsave(here("03_HospitalSeverity/Figs/sari_med_time.png"), width = 10, height = 13)

p1 <- quantile_month_plot(qi_month, ylim = 40)
p2 <- quantile_month_plot(qi_sex, ylim = 40) + facet_grid(~sex) + ylab("")
p3 <- quantile_month_plot(qi_age, ylim = 40) + facet_grid(~agegroup)
p4 <- quantile_month_plot(qi_comorbid, ylim = 40) + facet_grid(~comorbid_multip)

(p1 + p2 + plot_layout(widths = c(1, 2))) / p3 / p4 +
    plot_annotation(tag_levels = "A") + plot_layout(guides = "collect", axis_titles = "collect_x") & theme(legend.position = "bottom")

ggsave(here("03_HospitalSeverity/Figs/sari_med_time_icu.pdf"), width = 10, height = 13)
ggsave(here("03_HospitalSeverity/Figs/sari_med_time_icu.png"), width = 10, height = 13)

# trig functions

p1 <- probs_week_plot(ph_month, pwh_week)
p2 <- probs_week_plot(pi_month, pwi_week, guide = FALSE)
p3 <- probs_week_plot(ph_age, pwh_age) + facet_grid(~agegroup)
p4 <- probs_week_plot(pi_age, pwi_age, guide = FALSE) + facet_grid(~agegroup)
p5 <- probs_week_plot(ph_comorbid, pwh_comorbid, xlab = "Calendar week of admission") + facet_grid(~comorbid_multip)
p6 <- probs_week_plot(pi_comorbid, pwi_comorbid, xlab = "Calendar week of admission", guide = FALSE) + facet_grid(~comorbid_multip)

(p1 + p2) / (p3 + p4) / (p5 + p6) +
    plot_annotation(tag_levels = "A") + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave(here("03_HospitalSeverity/Figs/sari_weekly_prob.pdf"), width = 10, height = 13)
ggsave(here("03_HospitalSeverity/Figs/sari_weekly_prob.png"), width = 10, height = 13)
