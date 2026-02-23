# remove all objects from the environment
rm(list = ls())

# declare location of the current file within the project
here::i_am("code/02_get_manifestos.R")

# load libraries
library(here)
library(tidyverse)
library(manifestoR)


# Retrieve manifesto data for Germany -------------------------------------

# define api key
api_key <- '5c348b0c0afc2937c9a17781e2a3325f'
mp_setapikey(key = api_key)

# get corpus for Germany
manifesto_data <- mp_corpus(countryname == "Germany",
                            as_tibble = TRUE)

# Load the main dataset
main_dataset <- mp_maindataset()

# Select the relevant columns
party_info <- main_dataset %>%
  select(party, partyname, partyabbrev)

# Remove duplicate entries to get unique party codes and names
party_info_unique <- party_info %>%
  distinct()

# merge the data with the party information
manifesto_data  <- manifesto_data  %>%
  left_join(party_info_unique, by = c("party"))

# Save the data
rio::export(manifesto_data, here('data', 'manifestos_germany.rds'))







