# Thesis main file
# This file is used to generate the thesis

# Run this file to download the paperpile references,
# source R files in the thesis to generate the figures and tables,
# and run tinytex to compile the thesis

# Load libraries
here::i_am("R/_thesis_main.R")
library(here)
library(makeit)
library(tinytex)
library(conflicted)

# Functions
source(here("R/00_forest_plot.R"))
source(here("R/00_ribbon_plot.R"))
source(here("R/00_hiv_model_sim.R"))
source(here("R/00_simvax_msm.R"))
source(here("R/00_ve_plot.R"))
source(here("R/00_ajfit_plot.R"))
source(here("R/00_fg_functions.R"))
source(here("R/00_aj_functions.R"))

# Resolve conflicts
conflicts_prefer(dplyr::filter, dplyr::count, ggpp::annotate)

# Colour palettes
source(here("R/00_colour_palettes.R"))

# theme_minimal(15) is used throughout
# full page pdf figures are saved as w10:h13
# smaller figures are saved as w10:h6
theme_set(theme_minimal(15))

# stop scientific notation
options(scipen = 999)

# Download latest paperpile references (usually done via Overleaf)
# download.file(
#     "https://paperpile.com/eb/dbctHLuanT/paperpile.bib",
#     destfile = here("References/paperpile.bib")
# )

# Source chapter files using make
# RData files contain data objects
# R files contain plotting and table generation code
source(here("R/02_hiv_main.R"))
source(here("R/03_hosp_main.R"))
source(here("R/04_siren_main.R"))

# Make the thesis_data.dat file
make(
    recipe = "R/00_thesis_data.R",
    prereq = c(
        "R/02e_hiv_thesis_data.R",
        "R/03f_hosp_thesis_data.R",
        "R/04g_siren_thesis_data.R",
        "Data/hiv_msm_2023.RData",
        "Data/siren_cohort.RData",
        "Data/siren_expected_pos.RData",
        "Data/siren_expected_pos_ft.RData",
        "Data/siren_ppass.RData"
    ),
    target = "thesis_data.dat"
)

# Compile the thesis using tinytex (usually done via Overleaf)
# tinytex::pdflatex(here("thesis.tex"), clean = TRUE)
