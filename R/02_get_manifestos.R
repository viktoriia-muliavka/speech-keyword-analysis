## Setup -------------------------------------------------------------------

library(here)
here::i_am("R/02_get_manifestos.R")

library(dplyr)
library(manifestoR)

## Retrieve manifesto data for Germany -------------------------------------

api_key <- Sys.getenv("MANIFESTO_API_KEY")
if (api_key == "") {
  stop("Missing MANIFESTO_API_KEY. Set it in your environment before running.")
}
mp_setapikey(api_key)

manifestos_path <- here("data", "processed", "manifestos_germany.rds")
dir.create(dirname(manifestos_path), recursive = TRUE, showWarnings = FALSE)

if (file.exists(manifestos_path)) {

  message("Loading cached manifesto data: ", manifestos_path)
  manifesto_data <- readRDS(manifestos_path)

} else {

  message("Downloading manifesto data from Manifesto Project API...")

  manifesto_data <- mp_corpus(countryname == "Germany", as_tibble = TRUE)

  main_dataset <- mp_maindataset()

  party_info_unique <- main_dataset %>%
    select(party, partyname, partyabbrev) %>%
    distinct()

  manifesto_data <- manifesto_data %>%
    left_join(party_info_unique, by = "party")

  saveRDS(manifesto_data, manifestos_path)
  message("Saved cached manifesto data to: ", manifestos_path)
}







