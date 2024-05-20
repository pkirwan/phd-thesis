# Purpose: Create thesis_data.dat file with data objects for thesis

here::i_am("R/00_thesis_data.R")

# remove thesis_data.dat if it exists and re-initialise
if (file.exists(here("thesis_data.dat"))) {
    file.remove(here("thesis_data.dat"))
    write("hivww, 74.9", file = here("thesis_data.dat"))
}

source(here("R/02e_hiv_thesis_data.R"))
source(here("R/03f_hosp_thesis_data.R"))
source(here("R/04g_siren_thesis_data.R"))