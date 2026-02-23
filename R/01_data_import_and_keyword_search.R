## Setup -------------------------------------------------------------------

## remove all objects
rm(list = ls())

## declare location of the current file within the project
here::i_am("code/01_data_import_and_keyword_search.R")

## packages 
pacman::p_load(
  here,
  rio,
  tidyverse,
  lubridate,
  rvest, # web scraping
  WDI # retrieving world bank data
)

# Import dataset - speeches -----------------------------------------------

data_path <- here('data', 'germaparl_btvote_linkage')

speeches <- rio::import(paste0(data_path, '/3.2-speeches.RData'))

speakers <- rio::import(paste0(data_path, '/11-debates_with_BTVote_ids_correct_speaker_ids_debate_ids.RData'))

# merge speakers + speeches and data modification -------------------------

# The dataset containing speeches was supplied separately
# so we need to merge speakers + speeches

# check if speech_id uniquely identifies speeches --> yes
speeches %>% 
  distinct(speech_id) %>% 
  nrow()

# left_join datasets
speeches <- speakers %>% 
  left_join(speeches, by = join_by(speech_id))

# remove speakers dataset
rm(speakers)

# reduce to relevant variables 
speeches <- speeches %>% 
  select(date,
         legislative_period,
         speaker_speech, 
         speech_id, 
         mp_id, # ID variable that uniquely identifies each MP
         speaker_parliamentary_group, # party variable
         debate_desc,
         debate_type,
         debate_what) %>%
  # rename party variable
  rename(party = speaker_parliamentary_group)

# create new object df (for convenience) and remove speeches object
df <- speeches
rm(speeches)

df <- df %>% 
  mutate(year = lubridate::year(date))

# prepare dictionary ------------------------------------------------------

# In the following, the dictionary will be applied to all speeches

# import the dictionary
dictionary <- rio::import(here('data', 'dictionary_classification_handcoded.xlsx'))

dictionary <- dictionary %>% 
  filter(code == 1) %>% 
  filter(n >= 10)

# code into two categories (employees and workers)
dictionary <- dictionary %>% 
  mutate(
    category = case_when(
      str_detect(matches, 'arbeiter|arbeitende') ~ 'worker',
      str_detect(matches, 'arbeitnehmer') ~ 'employee',
      str_detect(matches, 'arbeitskraft|arbeitskräfte') ~ 'employee',
      
    )
  )

# create a list with search terms for working class appeals
search_terms_workers <- dictionary %>% 
  filter(category == 'worker') %>% 
  pull(matches)

search_terms_employees <- dictionary %>% 
  filter(category == 'employee') %>% 
  pull(matches)

# add word boundaries
search_terms_employees <- str_c("\\b", search_terms_employees, "\\b")
search_terms_workers <- str_c("\\b", search_terms_workers, "\\b")

# paste the search terms together separated by '|'
search_terms_workers <- paste(search_terms_workers, collapse = '|')
search_terms_employees <- paste(search_terms_employees, collapse = '|')

# create regular expressions that are case insensitive
search_terms_workers <- regex(search_terms_workers, ignore_case = T)
search_terms_employees <- regex(search_terms_employees, ignore_case = T)

# Apply dictionary --------------------------------------------------------

# apply search terms to the text column in df
# using stringr::str_count
df <- df %>% 
  mutate(worker = str_count(speaker_speech, search_terms_workers),
         employee = str_count(speaker_speech, search_terms_employees)) 

# Data modification -------------------------------------------------------

# The following contains data modification steps and data filtering

# select parties
parties <- c('CDU/CSU',
             'SPD',
             'FDP',
             'LINKE',
             'GRUENE',
             'AfD')

df <- df %>% 
  filter(party %in% parties)

# recode year as a running number so that the first year is 1, the second year is 2, etc.
# do this using dplyr's mutate() function
df <- df %>% 
  mutate(year_run = as.numeric(factor(year)))

df <- df %>% 
  # filter out AfD
  #filter(party != 'AfD') %>% 
  # create new year variable that starts at 0 (for estimation)
  # create dummy for before and after 1970 (two different processes/time trends)
  mutate(before_1970 = if_else(year < 1970, 'before 1970', 'after 1970'))  

# Merge with economic data ------------------------------------------------

# Now we need to merge the speech data with data on the economy 
# (specifically share of workers in industry and unemployment)
# share of industrial workers is supplied by destatis
# unemployment by statista (see links provided below)

# unemployment data 
# retrieved from: https://de.statista.com/statistik/daten/studie/1127090/umfrage/arbeitslosenquote-der-bundesrepublik-deutschland/
data_path <- here('data',
                  'unemployment', 
                  'statistic_arbeitslosigkeit-in-deutschland-1950-bis-2023.xlsx')

# import file as a list (it contains two worksheets)
unemployment_germany <- rio::import_list(data_path)

# reduce to worksheet that contains the data
unemployment_germany <- unemployment_germany$Daten

# create one unemployment column for based on west germany 
# (before unification) and germany (after unification)
unemployment_germany <- unemployment_germany %>% 
  mutate(unemployment = if_else(is.na(`Wiedervereinigtes Deutschland`),
                                `Früheres Bundesgebiet¹`,
                                `Wiedervereinigtes Deutschland`)) %>% 
  rename(year = Year) %>% 
  mutate(year = as.numeric(year))

# investigate unemployment over time
unemployment_germany %>% 
  ggplot(aes(x = year, y = unemployment)) +
  geom_line()

# check which datasets are available from the World Bank (gdp)
gdp_datasets <- WDIsearch('gdp')
gdp_datasets %>% 
  filter(str_detect(indicator, fixed('GDP.PCAP.CD')))

# retrieve dataset on gdp
gdp_germany <- WDI(indicator='NY.GDP.PCAP.CD', country=c('DE'), start=1960, end=2024)

# retrieve data on on employees by industry
# data path where the data on employees is located
data_path <- here('data', 'employees_by_sector.rds')

# test if the table exists 
test <- file.exists(data_path)

# if the file does not exist, scrape the table
if (test == F) {
  
  # retrieve data on employees by industry (de-industrialization/labor market structure)
  url <- 'https://www.destatis.de/EN/Themes/Economy/Short-Term-Indicators/Long-Term-Series/Labour-Market/lrerw14a.html'
  html <- read_html(url)
  
  html_table <- html %>% 
    html_node("table") %>% 
    html_table() %>% 
    # delete first row (it contains variable descriptions)
    slice(-1)
  
  rio::export(html_table, data_path)

} 

employees_by_sector <- rio::import(data_path)

# rename year variable before merging 
employees_by_sector <- employees_by_sector %>% 
  rename(year = Year) %>% 
  mutate(year = as.numeric(year))

# join economic datasets with speech data
df <- df %>% 
  left_join(unemployment_germany, by = join_by(year)) %>% 
  left_join(gdp_germany, by = join_by(year)) %>% 
  mutate(gdp = NY.GDP.PCAP.CD) %>% 
  left_join(employees_by_sector, by = join_by(year)) 

# rename production industry variable
index <- which(names(df) == "Production\nindustry\n(secondary sector)\nProportion in %")
names(df)[index] <- 'industry_share'

# Merge with party data (PAGED project) -----------------------------------

# besides economic data we also need info on the party composition of 
# each government for the period under consideration. Specifically, we 
# want to differentiate between government and opposition parties
# for this purpose the Party Government in Europe Database (PAGED) will
# be used
# Note: As we aggregate the data by years, the cabinet with most days in
# each year will be selected for merging and determining government/opposition
# status.
# If the datasets should be merged by dates instead, comment out the two lines
# at the end of this code section.

data_path <- here('data',
                  'PAGED', 
                  'PAGED-basic-yearly.xlsx')

# import PAGED dataset 
paged_germany <- rio::import(data_path) %>% 
  filter(country_name == 'Germany')

# some rows are missing for the Kiesinger cabinet --> add manually
kiesinger_rows_add <- paged_germany %>% 
  filter(str_detect(cab_name, 'Kiesinger')) %>% 
  slice(1)

# mutate dates for years 1968 and 1969 (these years were missing from the dataset)
kiesinger_rows_add <- kiesinger_rows_add %>% 
  mutate(year = as.Date('1968-01-01')) %>% 
  add_row(kiesinger_rows_add %>% mutate(year = as.Date('1969-01-01'))) 

paged_germany <- paged_germany %>% 
  rbind.data.frame(kiesinger_rows_add)

paged_germany_reduced <- paged_germany %>% 
  select(year, year_in, year_out, date_out, date_in, cab_name, cab_id, cab_composition1) %>% 
  mutate(days_in_year = case_when(
    year_in == year_out ~ as.numeric(date_out - date_in),
    year(year) == year_out ~ as.numeric(date_out - year),
    year(year) == year_in ~ as.numeric(as.Date(paste0(year_in, '-12-31')) - as.Date(date_in)),
    T ~ 365
  )) %>% 
  mutate(year_row = year(year)) 
  
paged_germany_reduced <- paged_germany_reduced %>% 
  group_by(year_row) %>% 
  slice_max(days_in_year) %>% 
  ungroup()

paged_germany_reduced <- paged_germany_reduced %>% 
  select(-year) %>% 
  rename(year = year_row)

df <- df %>%
  left_join(paged_germany_reduced, by = join_by(year))

# check if cabinets are correct --> looks good
df %>% 
  distinct(cab_name, year)

# check party names in cab_composition variable
df %>% 
  distinct(cab_composition1)

# convert party variable to character (for string operations)
df <- df %>% 
  mutate(party = as.character(party))

df <- df %>% 
  mutate(cab_composition1 = str_replace(cab_composition1, 
                                        pattern = 'CDU-CSU',
                                        replacement = 'CDU/CSU'))


# create opposition variable
df <- df %>%
  mutate(opposition = if_else(str_detect(cab_composition1, pattern = party),
                              'Government',
                              'Opposition'))

# check result --> looks good
df %>% 
  distinct(cab_composition1, party, opposition)


# Aggregate data ----------------------------------------------------------

# finally, we aggregate the data by year + party

# aggregate by year + party
df_aggregate <- df %>% 
  group_by(year, party) %>% 
  summarise(worker_n = sum(worker),
            employee_n = sum(employee),
            year = first(year),
            party = first(party),
            gdp = first(gdp),
            unemployment = first(unemployment),
            industry_share = first(industry_share),
            before_1970 = first(before_1970),
            opposition = first(opposition),
            n_mps = n()) %>% 
  ungroup() %>% 
  # reduce to CDU, FDP and SPD 
  # (for all other parties the number of year is too limited)
  #filter(party %in% c('CDU/CSU', 'FDP', 'SPD')) %>% 
  # calculate mean mentions per party 
  mutate(n_per_mp = worker_n/n_mps,
         n_per_mp_employee = employee_n/n_mps)  %>% 
  mutate(industry_share = as.numeric(industry_share)) %>% 
  # convert party to character and back to factor 
  # to get rid of levels that do not appear in the data anymore
  # (because the parties were filtered from the data)
  mutate(party = as.character(party),
         party = as.factor(party)) %>% 
  mutate(year_orig = year,
         year = year - 1949)

df_aggregate %>% 
  distinct(opposition, party, year_orig) %>% 
  tail()

# Save dataset ------------------------------------------------------------

data_path <- here('data', 'analysis_df.rds')
rio::export(df_aggregate, data_path)






