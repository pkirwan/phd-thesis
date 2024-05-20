# HospSev thesis data

here::i_am("R/03f_hosp_thesis_data.R")

library(here)
library(janitor)
library(scales)

load(here("data/hosp_mixcure.RData"))

# event probs from hospital
sari_hosp_icu_mar <- paste0(
    round(filter(ph_month, event == "ICU", monthofadmission == "Mar 2020")$val * 100, 1),
    "\\% (95\\% CI ", round(filter(ph_month, event == "ICU", monthofadmission == "Mar 2020")$lower * 100, 1),
    " to ", round(filter(ph_month, event == "ICU", monthofadmission == "Mar 2020")$upper * 100, 1), "\\%)"
)

sari_hosp_icu_may <- paste0(
    round(filter(ph_month, event == "ICU", monthofadmission == "May 2020")$val * 100, 1),
    "\\% (95\\% CI ", round(filter(ph_month, event == "ICU", monthofadmission == "May 2020")$lower * 100, 1),
    " to ", round(filter(ph_month, event == "ICU", monthofadmission == "May 2020")$upper * 100, 1), "\\%)"
)

sari_hosp_icu_oct <- paste0(
    round(filter(ph_month, event == "ICU", monthofadmission == "Oct 2020")$val * 100, 1),
    "\\% (95\\% CI ", round(filter(ph_month, event == "ICU", monthofadmission == "Oct 2020")$lower * 100, 1),
    " to ", round(filter(ph_month, event == "ICU", monthofadmission == "Oct 2020")$upper * 100, 1), "\\%)"
)

sari_hosp_icu_novdec <- paste0(
    round(mean(filter(ph_month, event == "ICU", monthofadmission %in% c("Nov 2020", "Dec 2020"))$val) * 100, 0),
    "\\%"
)

sari_hosp_icu_mar_4565 <- paste0(
    round(filter(ph_age, event == "ICU", monthofadmission == "Mar 2020", agegroup == "45-65")$val * 100, 1),
    "\\% (95\\% CI ", round(filter(ph_age, event == "ICU", monthofadmission == "Mar 2020", agegroup == "45-65")$lower * 100, 1),
    " to ", round(filter(ph_age, event == "ICU", monthofadmission == "Mar 2020", agegroup == "45-65")$upper * 100, 1), "\\%)"
)

sari_hosp_icu_mar_75 <- paste0(
    round(filter(ph_age, event == "ICU", monthofadmission == "Mar 2020", agegroup == "75+")$val * 100, 1),
    "\\% (95\\% CI ", round(filter(ph_age, event == "ICU", monthofadmission == "Mar 2020", agegroup == "75+")$lower * 100, 1),
    " to ", round(filter(ph_age, event == "ICU", monthofadmission == "Mar 2020", agegroup == "75+")$upper * 100, 1), "\\%)"
)

# event probs from ICU
sari_icu_death_mar <- paste0(
    round(filter(pi_month, event == "Death", monthofadmission == "Mar 2020")$val * 100, 1),
    "\\% (95\\% CI ", round(filter(pi_month, event == "Death", monthofadmission == "Mar 2020")$lower * 100, 1),
    " to ", round(filter(pi_month, event == "Death", monthofadmission == "Mar 2020")$upper * 100, 1), "\\%)"
)

sari_icu_death_jun <- paste0(
    round(filter(pi_month, event == "Death", monthofadmission == "Jun-Aug 2020")$val * 100, 1),
    "\\% (95\\% CI ", round(filter(pi_month, event == "Death", monthofadmission == "Jun-Aug 2020")$lower * 100, 1),
    " to ", round(filter(pi_month, event == "Death", monthofadmission == "Jun-Aug 2020")$upper * 100, 1), "\\%)"
)

sari_icu_death_oct <- paste0(
    round(filter(pi_month, event == "Death", monthofadmission == "Oct 2020")$val * 100, 1),
    "\\% (95\\% CI ", round(filter(pi_month, event == "Death", monthofadmission == "Oct 2020")$lower * 100, 1),
    " to ", round(filter(pi_month, event == "Death", monthofadmission == "Oct 2020")$upper * 100, 1), "\\%)"
)

sari_icu_death_novdec <- paste0(
    round(mean(filter(pi_month, event == "Death", monthofadmission %in% c("Nov 2020", "Dec 2020"))$val) * 100, 0),
    "\\%"
)

# Length of stay in ICU and hospital
sari_los_hosp_icu <- paste0(
    round(mean(filter(qh_month, event == "ICU")$median), 1)
)

sari_los_hosp_death_mar <- paste0(
    round(filter(qh_month, event == "Death", monthofadmission == "Mar 2020")$median, 1),
    " days (95\\% CI ", round(filter(qh_month, event == "Death", monthofadmission == "Mar 2020")$lower_0.5, 1),
    " to ", round(filter(qh_month, event == "Death", monthofadmission == "Mar 2020")$upper_0.5, 1), " days)"
)

sari_los_hosp_death_dec <- paste0(
    round(filter(qh_month, event == "Death", monthofadmission == "Dec 2020")$median, 1),
    " days (95\\% CI ", round(filter(qh_month, event == "Death", monthofadmission == "Dec 2020")$lower_0.5, 1),
    " to ", round(filter(qh_month, event == "Death", monthofadmission == "Dec 2020")$upper_0.5, 1), " days)"
)

sari_los_hosp_disc_mar <- paste0(
    round(filter(qh_month, event == "Discharge", monthofadmission == "Mar 2020")$median, 1),
    " days (95\\% CI ", round(filter(qh_month, event == "Discharge", monthofadmission == "Mar 2020")$lower_0.5, 1),
    " to ", round(filter(qh_month, event == "Discharge", monthofadmission == "Mar 2020")$upper_0.5, 1), " days)"
)

sari_los_hosp_disc_jun <- paste0(
    round(filter(qh_month, event == "Discharge", monthofadmission == "Jun-Aug 2020")$median, 1),
    " days (95\\% CI ", round(filter(qh_month, event == "Discharge", monthofadmission == "Jun-Aug 2020")$lower_0.5, 1),
    " to ", round(filter(qh_month, event == "Discharge", monthofadmission == "Jun-Aug 2020")$upper_0.5, 1), " days)"
)

sari_los_hosp_disc_dec <- paste0(
    round(filter(qh_month, event == "Discharge", monthofadmission == "Dec 2020")$median, 1),
    " days (95\\% CI ", round(filter(qh_month, event == "Discharge", monthofadmission == "Dec 2020")$lower_0.5, 1),
    " to ", round(filter(qh_month, event == "Discharge", monthofadmission == "Dec 2020")$upper_0.5, 1), " days)"
)

# Length of stay by age group
sari_los_hosp_disc_mar_75 <- paste0(
    round(filter(qh_age, event == "Discharge", monthofadmission == "Mar 2020", agegroup == "75+")$median, 1),
    " days (95\\% CI ", round(filter(qh_age, event == "Discharge", monthofadmission == "Mar 2020", agegroup == "75+")$lower_0.5, 1),
    " to ", round(filter(qh_age, event == "Discharge", monthofadmission == "Mar 2020", agegroup == "75+")$upper_0.5, 1), " days)"
)

sari_los_hosp_disc_mar_1545 <- paste0(
    round(filter(qh_age, event == "Discharge", monthofadmission == "Mar 2020", agegroup == "15-45")$median, 1),
    " days (95\\% CI ", round(filter(qh_age, event == "Discharge", monthofadmission == "Mar 2020", agegroup == "15-45")$lower_0.5, 1),
    " to ", round(filter(qh_age, event == "Discharge", monthofadmission == "Mar 2020", agegroup == "15-45")$upper_0.5, 1), " days)"
)

# Length of stay in ICU
sari_los_icu_disc_mar <- paste0(
    round(filter(qi_month, event == "Discharge", monthofadmission == "Mar 2020")$median, 1),
    " days (95\\% CI ", round(filter(qi_month, event == "Discharge", monthofadmission == "Mar 2020")$lower_0.5, 1),
    " to ", round(filter(qi_month, event == "Discharge", monthofadmission == "Mar 2020")$upper_0.5, 1), " days)"
)

sari_los_icu_disc_jun <- paste0(
    round(filter(qi_month, event == "Discharge", monthofadmission == "Jun-Aug 2020")$median, 1),
    " days (95\\% CI ", round(filter(qi_month, event == "Discharge", monthofadmission == "Jun-Aug 2020")$lower_0.5, 1),
    " to ", round(filter(qi_month, event == "Discharge", monthofadmission == "Jun-Aug 2020")$upper_0.5, 1), " days)"
)

sari_los_icu_disc_dec <- paste0(
    round(filter(qi_month, event == "Discharge", monthofadmission == "Dec 2020")$median, 1),
    " days (95\\% CI ", round(filter(qi_month, event == "Discharge", monthofadmission == "Dec 2020")$lower_0.5, 1),
    " to ", round(filter(qi_month, event == "Discharge", monthofadmission == "Dec 2020")$upper_0.5, 1), " days)"
)

sari_los_icu_death <- paste0(
    min(round(filter(qi_month, event == "Death")$median, 0)),
    " and ",
    max(round(filter(qi_month, event == "Death")$median, 0))
)

# Mixture event probs
sari_hfr_mar <- paste0(
    round(filter(pm_month, monthofadmission == "Mar 2020")$val * 100, 1),
    "\\% (95\\% CI ", round(filter(pm_month, monthofadmission == "Mar 2020")$lower * 100, 1),
    " to ", round(filter(pm_month, monthofadmission == "Mar 2020")$upper * 100, 1), "\\%)"
)

sari_hfr_jun <- paste0(
    round(filter(pm_month, monthofadmission == "Jun-Aug 2020")$val * 100, 1),
    "\\% (95\\% CI ", round(filter(pm_month, monthofadmission == "Jun-Aug 2020")$lower * 100, 1),
    " to ", round(filter(pm_month, monthofadmission == "Jun-Aug 2020")$upper * 100, 1), "\\%)"
)

sari_hfr_dec <- paste0(
    round(filter(pm_month, monthofadmission == "Dec 2020")$val * 100, 1),
    "\\% (95\\% CI ", round(filter(pm_month, monthofadmission == "Dec 2020")$lower * 100, 1),
    " to ", round(filter(pm_month, monthofadmission == "Dec 2020")$upper * 100, 1), "\\%)"
)

sari_hfr_feb <- paste0(
    round(filter(pm_month, monthofadmission == "Feb 2021")$val * 100, 1),
    "\\% (95\\% CI ", round(filter(pm_month, monthofadmission == "Feb 2021")$lower * 100, 1),
    " to ", round(filter(pm_month, monthofadmission == "Feb 2021")$upper * 100, 1), "\\%)"
)

# Mixture event probs by age and comorbidity
sari_hfr_dec_age_75 <- paste0(
    round(filter(pm_age, monthofadmission == "Dec 2020", agegroup == "75+")$val * 100, 1),
    "\\% (95\\% CI ", round(filter(pm_age, monthofadmission == "Dec 2020", agegroup == "75+")$lower * 100, 1),
    " to ", round(filter(pm_age, monthofadmission == "Dec 2020", agegroup == "75+")$upper * 100, 1), "\\%)"
)

sari_hfr_dec_comorb_3 <- paste0(
    round(filter(pm_comorbid, monthofadmission == "Dec 2020", comorbid_multip == "3+")$val * 100, 1),
    "\\% (95\\% CI ", round(filter(pm_comorbid, monthofadmission == "Dec 2020", comorbid_multip == "3+")$lower * 100, 1),
    " to ", round(filter(pm_comorbid, monthofadmission == "Dec 2020", comorbid_multip == "3+")$upper * 100, 1), "\\%)"
)

write(paste0("sari_hosp_icu_mar, ", sari_hosp_icu_mar), here("thesis_data.dat"), append = TRUE)
write(paste0("sari_hosp_icu_may, ", sari_hosp_icu_may), here("thesis_data.dat"), append = TRUE)
write(paste0("sari_hosp_icu_oct, ", sari_hosp_icu_oct), here("thesis_data.dat"), append = TRUE)
write(paste0("sari_hosp_icu_novdec, ", sari_hosp_icu_novdec), here("thesis_data.dat"), append = TRUE)
write(paste0("sari_hosp_icu_mar_4565, ", sari_hosp_icu_mar_4565), here("thesis_data.dat"), append = TRUE)
write(paste0("sari_hosp_icu_mar_75, ", sari_hosp_icu_mar_75), here("thesis_data.dat"), append = TRUE)

write(paste0("sari_icu_death_mar, ", sari_icu_death_mar), here("thesis_data.dat"), append = TRUE)
write(paste0("sari_icu_death_jun, ", sari_icu_death_jun), here("thesis_data.dat"), append = TRUE)
write(paste0("sari_icu_death_oct, ", sari_icu_death_oct), here("thesis_data.dat"), append = TRUE)
write(paste0("sari_icu_death_novdec, ", sari_icu_death_novdec), here("thesis_data.dat"), append = TRUE)

write(paste0("sari_los_hosp_icu, ", sari_los_hosp_icu), here("thesis_data.dat"), append = TRUE)
write(paste0("sari_los_hosp_death_mar, ", sari_los_hosp_death_mar), here("thesis_data.dat"), append = TRUE)
write(paste0("sari_los_hosp_death_dec, ", sari_los_hosp_death_dec), here("thesis_data.dat"), append = TRUE)

write(paste0("sari_los_hosp_disc_mar, ", sari_los_hosp_disc_mar), here("thesis_data.dat"), append = TRUE)
write(paste0("sari_los_hosp_disc_jun, ", sari_los_hosp_disc_jun), here("thesis_data.dat"), append = TRUE)
write(paste0("sari_los_hosp_disc_dec, ", sari_los_hosp_disc_dec), here("thesis_data.dat"), append = TRUE)

write(paste0("sari_los_hosp_disc_mar_75, ", sari_los_hosp_disc_mar_75), here("thesis_data.dat"), append = TRUE)
write(paste0("sari_los_hosp_disc_mar_1545, ", sari_los_hosp_disc_mar_1545), here("thesis_data.dat"), append = TRUE)

write(paste0("sari_los_icu_disc_mar, ", sari_los_icu_disc_mar), here("thesis_data.dat"), append = TRUE)
write(paste0("sari_los_icu_disc_jun, ", sari_los_icu_disc_jun), here("thesis_data.dat"), append = TRUE)
write(paste0("sari_los_icu_disc_dec, ", sari_los_icu_disc_dec), here("thesis_data.dat"), append = TRUE)
write(paste0("sari_los_icu_death, ", sari_los_icu_death), here("thesis_data.dat"), append = TRUE)

write(paste0("sari_hfr_mar, ", sari_hfr_mar), here("thesis_data.dat"), append = TRUE)
write(paste0("sari_hfr_jun, ", sari_hfr_jun), here("thesis_data.dat"), append = TRUE)
write(paste0("sari_hfr_dec, ", sari_hfr_dec), here("thesis_data.dat"), append = TRUE)
write(paste0("sari_hfr_feb, ", sari_hfr_feb), here("thesis_data.dat"), append = TRUE)

write(paste0("sari_hfr_dec_age_75, ", sari_hfr_dec_age_75), here("thesis_data.dat"), append = TRUE)
write(paste0("sari_hfr_dec_comorb_3, ", sari_hfr_dec_comorb_3), here("thesis_data.dat"), append = TRUE)

# SUS figures
load(here("data/hosp_sus.RData"))

# sus_died <- paste0(
#     hosp |> filter(eventm == "Death") |> count() |> pull(n) |> format(big.mark = ","), " (",
#     round((hosp |> filter(eventm == "Death") |> count()) / (hosp |> count()) * 100, 1), "\\%)"
# )

# sus_disc <- paste0(
#     hosp |> filter(eventm == "Discharge") |> count() |> pull(n) |> format(big.mark = ","), " (",
#     round((hosp |> filter(eventm == "Discharge") |> count()) / (hosp |> count()) * 100, 1), "\\%)"
# )

# sus_right <- paste0(
#     hosp |> filter(is.na(eventm)) |> count(), " (",
#     round((hosp |> filter(is.na(eventm)) |> count()) / (hosp |> count()) * 100, 1), "\\%)"
# )

# HFR and LOS by month
hfr_month <- ajhfr(ajsus_month, mat = TRUE)
los_month <- ajlos(ajsus_month, mat = TRUE)

sus_hfr_mar20 <- paste0(
    round(hfr_month |> filter(monthyear == "Mar 2020") |> pull("val") * 100, 1),
    "\\% (95\\% CI ", round(hfr_month |> filter(monthyear == "Mar 2020") |> pull("lower") * 100, 1),
    " to ", round(hfr_month |> filter(monthyear == "Mar 2020") |> pull("upper") * 100, 1), "\\%)"
)

sus_hfr_aug20 <- paste0(
    round(hfr_month |> filter(monthyear == "Aug 2020") |> pull("val") * 100, 1),
    "\\% (95\\% CI ", round(hfr_month |> filter(monthyear == "Aug 2020") |> pull("lower") * 100, 1),
    " to ", round(hfr_month |> filter(monthyear == "Aug 2020") |> pull("upper") * 100, 1), "\\%)"
)

sus_hfr_jan21 <- paste0(
    round(hfr_month |> filter(monthyear == "Jan 2021") |> pull("val") * 100, 1),
    "\\% (95\\% CI ", round(hfr_month |> filter(monthyear == "Jan 2021") |> pull("lower") * 100, 1),
    " to ", round(hfr_month |> filter(monthyear == "Jan 2021") |> pull("upper") * 100, 1), "\\%)"
)

sus_hfr_mar21 <- paste0(
    round(hfr_month |> filter(monthyear == "Mar 2021") |> pull("val") * 100, 1),
    "\\% (95\\% CI ", round(hfr_month |> filter(monthyear == "Mar 2021") |> pull("lower") * 100, 1),
    " to ", round(hfr_month |> filter(monthyear == "Mar 2021") |> pull("upper") * 100, 1), "\\%)"
)

sus_hfr_oct21 <- paste0(
    round(hfr_month |> filter(monthyear == "Oct 2021") |> pull("val") * 100, 1),
    "\\% (95\\% CI ", round(hfr_month |> filter(monthyear == "Oct 2021") |> pull("lower") * 100, 1),
    " to ", round(hfr_month |> filter(monthyear == "Oct 2021") |> pull("upper") * 100, 1), "\\%)"
)

sus_los_disc_mar20 <- paste0(
    round(los_month |> filter(state == "Discharge", monthyear == "Mar 2020") |> pull("med"), 1),
    " days (95\\% CI ", round(los_month |> filter(state == "Discharge", monthyear == "Mar 2020") |> pull("upper"), 1),
    " to ", round(los_month |> filter(state == "Discharge", monthyear == "Mar 2020") |> pull("lower"), 1), " days)"
)

sus_los_disc_aug20 <- paste0(
    round(los_month |> filter(state == "Discharge", monthyear == "Aug 2020") |> pull("med"), 1),
    " days (95\\% CI ", round(los_month |> filter(state == "Discharge", monthyear == "Aug 2020") |> pull("upper"), 1),
    " to ", round(los_month |> filter(state == "Discharge", monthyear == "Aug 2020") |> pull("lower"), 1), " days)"
)

sus_los_death_mar20 <- paste0(
    round(los_month |> filter(state == "Death", monthyear == "Mar 2020") |> pull("med"), 1),
    " days (95\\% CI ", round(los_month |> filter(state == "Death", monthyear == "Mar 2020") |> pull("upper"), 1),
    " to ", round(los_month |> filter(state == "Death", monthyear == "Mar 2020") |> pull("lower"), 1), " days)"
)

sus_los_death_aug20 <- paste0(
    round(los_month |> filter(state == "Death", monthyear == "Aug 2020") |> pull("med"), 1),
    " days (95\\% CI ", round(los_month |> filter(state == "Death", monthyear == "Aug 2020") |> pull("upper"), 1),
    " to ", round(los_month |> filter(state == "Death", monthyear == "Aug 2020") |> pull("lower"), 1), " days)"
)

# HFR by bed occupancy
hfr_load <- ajhfr(ajsus_load, mat = TRUE)

sus_hfr_load_mar_90 <- paste0(
    round(hfr_load |> filter(monthyear == "Mar 2020", bed_occupancy_cat == "90-100%") |> pull("val") * 100, 1),
    "\\% (95\\% CI ", round(hfr_load |> filter(monthyear == "Mar 2020", bed_occupancy_cat == "90-100%") |> pull("lower") * 100, 1),
    " to ", round(hfr_load |> filter(monthyear == "Mar 2020", bed_occupancy_cat == "90-100%") |> pull("upper") * 100, 1), "\\%)"
)

sus_hfr_load_mar_0 <- paste0(
    round(hfr_load |> filter(monthyear == "Mar 2020", bed_occupancy_cat == "0-20%") |> pull("val") * 100, 1),
    "\\% (95\\% CI ", round(hfr_load |> filter(monthyear == "Mar 2020", bed_occupancy_cat == "0-20%") |> pull("lower") * 100, 1),
    " to ", round(hfr_load |> filter(monthyear == "Mar 2020", bed_occupancy_cat == "0-20%") |> pull("upper") * 100, 1), "\\%)"
)

sus_hfr_load_nov_90 <- paste0(
    round(hfr_load |> filter(monthyear == "Nov 2020", bed_occupancy_cat == "90-100%") |> pull("val") * 100, 1),
    "\\% (95\\% CI ", round(hfr_load |> filter(monthyear == "Nov 2020", bed_occupancy_cat == "90-100%") |> pull("lower") * 100, 1),
    " to ", round(hfr_load |> filter(monthyear == "Nov 2020", bed_occupancy_cat == "90-100%") |> pull("upper") * 100, 1), "\\%)"
)

sus_hfr_load_nov_0 <- paste0(
    round(hfr_load |> filter(monthyear == "Nov 2020", bed_occupancy_cat == "0-20%") |> pull("val") * 100, 1),
    "\\% (95\\% CI ", round(hfr_load |> filter(monthyear == "Nov 2020", bed_occupancy_cat == "0-20%") |> pull("lower") * 100, 1),
    " to ", round(hfr_load |> filter(monthyear == "Nov 2020", bed_occupancy_cat == "0-20%") |> pull("upper") * 100, 1), "\\%)"
)

# vaccination hazards
load(here("Data/Hosp/coxfit_multi_vaccine.RData"))

tidy_fg_fit <- broom::tidy(coxfit_multi, conf.int = TRUE, exponentiate = TRUE)

sus_haz_vac1_36 <- paste0(
    tidy_fg_fit |> filter(term == "vaccine_long3-6 weeks after first dose") |> pull("estimate") |> round(2),
    " (95\\% CI ", tidy_fg_fit |> filter(term == "vaccine_long3-6 weeks after first dose") |> pull("conf.low") |> round(2),
    " to ", tidy_fg_fit |> filter(term == "vaccine_long3-6 weeks after first dose") |> pull("conf.high") |> round(2), ")"
)

sus_haz_vac1_612 <- paste0(
    tidy_fg_fit |> filter(term == "vaccine_long6-12 weeks after first dose") |> pull("estimate") |> round(2),
    " (95\\% CI ", tidy_fg_fit |> filter(term == "vaccine_long3-6 weeks after first dose") |> pull("conf.low") |> round(2),
    " to ", tidy_fg_fit |> filter(term == "vaccine_long3-6 weeks after first dose") |> pull("conf.high") |> round(2), ")"
)

sus_haz_vac2_26 <- paste0(
    tidy_fg_fit |> filter(term == "vaccine_long2-6 weeks after second dose") |> pull("estimate") |> round(2),
    " (95\\% CI ", tidy_fg_fit |> filter(term == "vaccine_long2-6 weeks after second dose") |> pull("conf.low") |> round(2),
    " to ", tidy_fg_fit |> filter(term == "vaccine_long2-6 weeks after second dose") |> pull("conf.high") |> round(2), ")"
)

sus_haz_vac3_26 <- paste0(
    tidy_fg_fit |> filter(term == "vaccine_long6-12 weeks after third dose") |> pull("estimate") |> round(2),
    " (95\\% CI ", tidy_fg_fit |> filter(term == "vaccine_long6-12 weeks after third dose") |> pull("conf.low") |> round(2),
    " to ", tidy_fg_fit |> filter(term == "vaccine_long6-12 weeks after third dose") |> pull("conf.high") |> round(2), ")"
)

sus_haz_asian <- paste0(
    tidy_fg_fit |> filter(term == "ethGrp4Asian") |> pull("estimate") |> round(2),
    " (95\\% CI ", tidy_fg_fit |> filter(term == "ethGrp4Asian") |> pull("conf.low") |> round(2),
    " to ", tidy_fg_fit |> filter(term == "ethGrp4Asian") |> pull("conf.high") |> round(2), ")"
)

sus_haz_black <- paste0(
    tidy_fg_fit |> filter(term == "ethGrp4Black") |> pull("estimate") |> round(2),
    " (95\\% CI ", tidy_fg_fit |> filter(term == "ethGrp4Black") |> pull("conf.low") |> round(2),
    " to ", tidy_fg_fit |> filter(term == "ethGrp4Black") |> pull("conf.high") |> round(2), ")"
)

sus_haz_female <- paste0(
    tidy_fg_fit |> filter(term == "sexFemale") |> pull("estimate") |> round(2),
    " (95\\% CI ", tidy_fg_fit |> filter(term == "sexFemale") |> pull("conf.low") |> round(2),
    " to ", tidy_fg_fit |> filter(term == "sexFemale") |> pull("conf.high") |> round(2), ")"
)

sus_haz_cci5 <- paste0(
    tidy_fg_fit |> filter(term == "charlson_index5+") |> pull("estimate") |> round(2),
    " (95\\% CI ", tidy_fg_fit |> filter(term == "charlson_index5+") |> pull("conf.low") |> round(2),
    " to ", tidy_fg_fit |> filter(term == "charlson_index5+") |> pull("conf.high") |> round(2), ")"
)

sus_haz_dep <- paste0(
    tidy_fg_fit |> filter(term == "imd_quintile1") |> pull("estimate") |> round(2),
    " (95\\% CI ", tidy_fg_fit |> filter(term == "imd_quintile1") |> pull("conf.low") |> round(2),
    " to ", tidy_fg_fit |> filter(term == "imd_quintile1") |> pull("conf.high") |> round(2), ")"
)

# Doesn't work with comma-separated numbers
# write(paste0("sus_total, ", hosp |> count() |> pull(n) |> format(big.mark = ",")), here("thesis_data.dat"), append = TRUE)
# write(paste0("sus_died, ", sus_died), here("thesis_data.dat"), append = TRUE)
# write(paste0("sus_disc, ", sus_disc), here("thesis_data.dat"), append = TRUE)
# write(paste0("sus_right, ", sus_right), here("thesis_data.dat"), append = TRUE)

write(paste0("sus_hfr_mar20, ", sus_hfr_mar20), here("thesis_data.dat"), append = TRUE)
write(paste0("sus_hfr_aug20, ", sus_hfr_aug20), here("thesis_data.dat"), append = TRUE)
write(paste0("sus_hfr_jan21, ", sus_hfr_jan21), here("thesis_data.dat"), append = TRUE)
write(paste0("sus_hfr_mar21, ", sus_hfr_mar21), here("thesis_data.dat"), append = TRUE)
write(paste0("sus_hfr_oct21, ", sus_hfr_oct21), here("thesis_data.dat"), append = TRUE)

write(paste0("sus_los_disc_mar20, ", sus_los_disc_mar20), here("thesis_data.dat"), append = TRUE)
write(paste0("sus_los_disc_aug20, ", sus_los_disc_aug20), here("thesis_data.dat"), append = TRUE)
write(paste0("sus_los_death_mar20, ", sus_los_death_mar20), here("thesis_data.dat"), append = TRUE)
write(paste0("sus_los_death_aug20, ", sus_los_death_aug20), here("thesis_data.dat"), append = TRUE)

write(paste0("sus_hfr_load_mar_90, ", sus_hfr_load_mar_90), here("thesis_data.dat"), append = TRUE)
write(paste0("sus_hfr_load_mar_0, ", sus_hfr_load_mar_0), here("thesis_data.dat"), append = TRUE)
write(paste0("sus_hfr_load_nov_90, ", sus_hfr_load_nov_90), here("thesis_data.dat"), append = TRUE)
write(paste0("sus_hfr_load_nov_0, ", sus_hfr_load_nov_0), here("thesis_data.dat"), append = TRUE)

write(paste0("sus_haz_vac1_36, ", sus_haz_vac1_36), here("thesis_data.dat"), append = TRUE)
write(paste0("sus_haz_vac1_612, ", sus_haz_vac1_612), here("thesis_data.dat"), append = TRUE)
write(paste0("sus_haz_vac2_26, ", sus_haz_vac2_26), here("thesis_data.dat"), append = TRUE)
write(paste0("sus_haz_vac3_26, ", sus_haz_vac3_26), here("thesis_data.dat"), append = TRUE)

write(paste0("sus_haz_asian, ", sus_haz_asian), here("thesis_data.dat"), append = TRUE)
write(paste0("sus_haz_black, ", sus_haz_black), here("thesis_data.dat"), append = TRUE)
write(paste0("sus_haz_female, ", sus_haz_female), here("thesis_data.dat"), append = TRUE)
write(paste0("sus_haz_cci5, ", sus_haz_cci5), here("thesis_data.dat"), append = TRUE)
write(paste0("sus_haz_dep, ", sus_haz_dep), here("thesis_data.dat"), append = TRUE)
