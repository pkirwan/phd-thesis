# Hosp cases
here::i_am("R/03e_hosp_sus.R")

library(here)
library(tidyverse)
library(patchwork)
library(survival)
library(finalfit)
library(scales)

load(here("Data/hosp_sus.RData"))

ajsus_month |>
  mutate(model = "Aalen-Johansen") |>
  filter(state == "Death") |>
  bind_rows(st |> mutate(model = "Fine and Gray")) |>
  mutate(monthyear = factor(monthyear, levels = level_order)) |>
  filter(monthyear %in% level_order[c(FALSE, TRUE, FALSE)]) |>
  ggplot() +
  aes(x = time, y = val, ymin = lower, ymax = upper, col = model, fill = model) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(alpha = 0.3, lty = 1, linewidth = 0.3) +
  labs(x = "Days after hospital admission", y = "Probability of fatality", col = "Model", fill = "Model") +
  scale_x_continuous(breaks = c(0, 20, 40), limits = c(0, 40)) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::percent
  ) +
  theme_minimal(15) +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "top"
  ) +
  scale_colour_manual(values = thesis_col[seq(4, length(thesis_col), 4)]) +
  scale_fill_manual(values = thesis_col[seq(4, length(thesis_col), 4)]) +
  facet_wrap(~monthyear, nrow = 3, scales = "free_y")

ggsave(here("03_HospitalSeverity/Figs/fg_aj_month_comparison.pdf"), width = 10, height = 6)
ggsave(here("03_HospitalSeverity/Figs/fg_aj_month_comparison.png"), width = 10, height = 6)

level_order_n <- sub(" ", "\n", level_order)
level_order_thinned_n <- sub(" ", "\n", level_order_thinned)

# HFR and LOS by month
aj_full_fit(ajsus_month, nd = nd_month, maxdays = 12, thin = FALSE) & theme(legend.position = "bottom")

ggsave(here("03_HospitalSeverity/Figs/hfr_month.pdf"), width = 10, height = 6)
ggsave(here("03_HospitalSeverity/Figs/hfr_month.png"), width = 10, height = 6)

# HFR and LOS by month and age group
aj_full_fit(ajsus_age, nd = nd_month_age, flip = TRUE, maxdays = 20)

ggsave(here("03_HospitalSeverity/Figs/hfr_month_age.pdf"), width = 10, height = 13)
ggsave(here("03_HospitalSeverity/Figs/hfr_month_age.png"), width = 10, height = 13)

# HFR and LOS by month and hospital load
aj_full_fit(ajsus_load, nd = nd_month_load, flip = TRUE, maxdays = 20)

ggsave(here("03_HospitalSeverity/Figs/hfr_month_load.pdf"), width = 10, height = 13)
ggsave(here("03_HospitalSeverity/Figs/hfr_month_load.png"), width = 10, height = 13)

# figure size exceptions 10 x 9
aj_full_fit(ajsus_sex, nd = nd_month_sex, flip = TRUE, maxdays = 12, thin = FALSE)

ggsave(here("03_HospitalSeverity/Figs/hfr_month_sex.pdf"), width = 10, height = 9)
ggsave(here("03_HospitalSeverity/Figs/hfr_month_sex.png"), width = 10, height = 9)

aj_full_fit(ajsus_ethn, nd = nd_month_ethn, flip = TRUE, maxdays = 16)

ggsave(here("03_HospitalSeverity/Figs/hfr_month_ethn.pdf"), width = 10, height = 9)
ggsave(here("03_HospitalSeverity/Figs/hfr_month_ethn.png"), width = 10, height = 9)

aj_full_fit(ajsus_cci, nd = nd_month_cci, flip = TRUE, maxdays = 16)

ggsave(here("03_HospitalSeverity/Figs/hfr_month_cci.pdf"), width = 10, height = 9)
ggsave(here("03_HospitalSeverity/Figs/hfr_month_cci.png"), width = 10, height = 9)


# FG for vaccine and other covariates
load(here("Data/Hosp/coxfit_multi_vaccine.RData"))

tidy_fg_fit <- broom::tidy(coxfit_multi, conf.int = TRUE, exponentiate = TRUE)

labels <- tibble(
  label = c(
    "Unvaccinated",
    # "0-3 weeks after first dose",
    "3-6 weeks after first dose",
    "6-12 weeks after first dose",
    ">12 weeks after first dose",
    # "0-2 weeks after second dose",
    "2-6 weeks after second dose",
    "6-12 weeks after second dose",
    ">12 weeks after second dose",
    # "0-2 weeks after third dose",
    "2-6 weeks after third dose",
    "6-12 weeks after third dose",
    ">12 weeks after third dose"
  ),
  group = "Vaccine"
)

levels <- labels |> filter(!label %in% c("Unvaccinated"))

fg_plot_vaccine(
  tidy_fg_fit |> filter(
    grepl("vaccine_long", term),
    !term %in% c(
      "vaccine_long0-3 weeks after first dose",
      "vaccine_long0-2 weeks after second dose",
      "vaccine_long0-2 weeks after third dose"
    )
  ),
  labels, levels,
  var_group = c("Vaccine")
) + ylab("Vaccine status")

ggsave(here("03_HospitalSeverity/Figs/fg_vaccine.pdf"), width = 10, height = 6)
ggsave(here("03_HospitalSeverity/Figs/fg_vaccine.png"), width = 10, height = 6)

# all other covariates
labels <- tibble(
  label = c(
    "Male", "Female",
    "White", "Asian", "Black", "Mixed/Other/Unknown",
    "1 (Most deprived)", "2", "3", "4", "5 (Least deprived)",
    "0", "1-2", "3-4", "5+",
    "0-20%", "20-40%", "40-60%", "60-80%", "80-90%", "90-100%"
  ),
  group = c(
    rep("Sex", 2),
    rep("Ethnicity", 4),
    rep("IMD", 5),
    rep("CCI", 4),
    rep("Hospital load", 6)
  )
)

levels <- labels |> filter(!label %in% c("Male", "White", "5 (Least deprived)", "0", "0-20%"))

p2 <- fg_plot(
  tidy_fg_fit |> filter(!grepl("vaccine", term), term != "ethGrp4Prefer not to say"),
  labels, levels,
  var_group = c("Sex", "Ethnicity", "IMD", "Hospital load")
)

p3 <- fg_plot_vaccine(
  tidy_fg_fit |> filter(!grepl("vaccine", term), term != "ethGrp4Prefer not to say"),
  labels, levels,
  var_group = c("CCI"), colour = thesis_col[12]
)

p2 / p3 + plot_annotation(tag_levels = "A") + plot_layout(heights = c(17, 4))

ggsave(here("03_HospitalSeverity/Figs/fg_hazards.pdf"), width = 10, height = 13)
ggsave(here("03_HospitalSeverity/Figs/fg_hazards.png"), width = 10, height = 13)

# FG for month
load(here("Data/Hosp/coxfit_multi_month.RData"))

tidy_fg_fit <- broom::tidy(coxfit_multi, conf.int = TRUE, exponentiate = TRUE)

labels <- tibble(
  label = level_order,
  group = rep("Month", 26)
)

levels <- labels |> filter(!label %in% c("Jun 2020"))

fg_plot_vaccine(tidy_fg_fit |> filter(grepl("month", term)),
  labels, levels,
  var_group = c("Month"), plot_title = "", colour = thesis_col[1]
) + ylab("Month of hospital admission")

ggsave(here("03_HospitalSeverity/Figs/fg_month.pdf"), width = 10, height = 6)
ggsave(here("03_HospitalSeverity/Figs/fg_month.png"), width = 10, height = 6)

# Hosp FG shift

load(here("Data/hosp_fg_shift.RData"))

labels <- tibble(label = c(level_order, level_order, level_order, level_order, level_order), shift = c(rep("0 days", 26), rep("1 day", 26), rep("2 days", 26), rep("3 days", 26), rep("4 days", 26)))

levels <- labels |> filter(!label %in% c("Jun 2020"))

fg_plot_shift(tidy_fg_fit_shift |> filter(grepl("monthyear", term)))

ggsave(here("03_HospitalSeverity/Figs/fg_shift.pdf"), width = 10, height = 9)
ggsave(here("03_HospitalSeverity/Figs/fg_shift.png"), width = 10, height = 9)
