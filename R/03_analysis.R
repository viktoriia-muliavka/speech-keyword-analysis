## Setup -------------------------------------------------------------------

## remove all objects
rm(list = ls())

## declare location of the current file within the project
here::i_am("code/03_analysis.R")

## packages 
pacman::p_load(
  here,
  rio,
  splines,
  tidyverse,
  lubridate,
  modelsummary, # For model summaries
  marginaleffects, # For marginal effects
  nlme # for gls()
)

# Import dataset - speeches -----------------------------------------------

data_path <- here('data', 'analysis_df.rds')
result_path <- here('results')

df_aggregate <- rio::import(data_path)

df_aggregate <- df_aggregate %>% 
  mutate(industry_cat = case_when(
    between(industry_share, 50, 60) ~ 'between 50 and 60',
    between(industry_share, 40, 50) ~ 'between 40 and 50',
    between(industry_share, 30, 40) ~ 'between 30 and 40',
    between(industry_share, 20, 30) ~ 'between 20 and 30',
    between(industry_share, 10, 20) ~ 'between 10 and 20'
  ))

# add decade
df_aggregate <- df_aggregate %>% 
  mutate(decade = round(year_orig, -1))

# Plot defaults -----------------------------------------------------------

fig_num <- 1  # Initialize figure counter

add_figure_number <- function(title_prefix = 'Figure') {
  plot_prefix <- paste(title_prefix, fig_num)
  fig_num <<- fig_num + 1  # Increment counter globally
  return(plot_prefix)
}

library(showtext)
library(sysfonts)
library(ggrepel)
library(ggpubr)

# Font registration and auto rendering
# showtext_auto()  # Enable showtext for font rendering
# showtext_opts(dpi = 300) # ensure proper scaling
# font_add_google("Barlow Semi Condensed", "Barlow")
# 
# # Revised theme function with explicit sizing
# theme_clean <- function(base_size = 11, base_family = "Barlow") {
#   theme_minimal(base_family = base_family, base_size = base_size) +
#     theme(
#       panel.grid.minor = element_blank(),
#       plot.background = element_rect(fill = "white", color = NA),
#       plot.title = element_text(face = "bold", size = rel(1.2)),
#       axis.title = element_text(face = "bold", size = rel(1)),
#       strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
#       strip.background = element_rect(fill = "grey80", color = NA),
#       legend.title = element_text(face = "bold"),
#       legend.position = 'top',
#       axis.title.x = element_text(margin = margin(t = 10)),
#       axis.title.y = element_text(margin = margin(r = 10))
#     )
# }
# 
# # Update geom defaults
# update_geom_defaults("label_repel", list(family = "Barlow", fontface = "bold"))
# update_geom_defaults("label", list(family = "Barlow", fontface = "bold"))


# Alternative pubr theme as basis
theme_clean <- function() {
  theme_pubr() +
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(face = "bold", size = rel(1.2)),
      axis.title = element_text(face = "bold", size = rel(1)),
      strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
      strip.background = element_rect(fill = "grey80", color = NA),
      legend.title = element_text(face = "bold"),
      legend.position = 'top',
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
    )
}



# Set theme
theme_set(theme_clean())



# define party colors
party_colors <- c('CDU/CSU' = '#000000',
                  'SPD' = '#E3000F',
                  'FDP' = '#FFED00',
                  'GRUENE' = '#64A12D',
                  'LINKE' = '#BE3075',
                  'AfD' = 'blue')

parties <- c('CDU/CSU',
             'SPD',
             'FDP',
             'LINKE',
             'GRUENE',
             'AfD')

# import manifestos and keyword search ------------------------------------

# import dataset
data_path <- here('data', 'manifestos_germany.rds')
manifesto_data <- rio::import(data_path)

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

manifesto_data <- manifesto_data %>% 
  mutate(worker = str_count(text, search_terms_workers),
         employee = str_count(text, search_terms_employees)) 

# extract year and month from date
# the year is the first four characters of the date
manifesto_data <- manifesto_data %>%
  mutate(year = substr(date, 1, 4),
         month = substr(date, 5, 6)) %>% 
  mutate(year = as.numeric(year),
         month = as.numeric(month))

manifesto_data <- manifesto_data %>% 
  mutate(partyabbrev = case_when(
    partyabbrev %in% c('Greens/90', '90/Greens') ~ 'GRUENE',
    partyabbrev %in% c('PDS', 'L-PDS', 'LINKE') ~ 'LINKE',
    T ~ partyabbrev,
  ))

# create data for plotting
plot_data <- manifesto_data %>% 
  group_by(year, partyabbrev) %>% 
  summarise(worker = sum(worker, na.rm = T),
            employee = sum(employee, na.rm = T))

# plot mentions of worker keywords per party over time
plot_manifestos_workers <- plot_data %>% 
  filter(partyabbrev %in% parties) %>% 
  ggplot(aes(x = year, y = worker, fill = partyabbrev)) +
  geom_col() +
  facet_wrap(~ partyabbrev) +
  scale_fill_manual(values = party_colors) +
  theme(legend.position = 'none') +
  labs(y = 'Mentions of worker keywords', x = 'Year')

# save plot
# ggsave(plot = plot_manifestos_workers,
#        filename = 'Working class appeals used in German part manifestos (1949-2021).PNG',
#        path = result_path, 
#        dpi = 300, width = 7, height = 7)


# plot mentions of employee keywords per party over time
plot_manifestos_employees <- plot_data %>% 
  filter(partyabbrev %in% parties) %>% 
  ggplot(aes(x = year, y = employee, fill = partyabbrev)) +
  geom_col() +
  facet_wrap(~ partyabbrev) +
  scale_fill_manual(values = party_colors) +
  theme(legend.position = 'none')

# Descriptives ------------------------------------------------------------

# worker mentions per MP and party
plot_desc_trend <- df_aggregate %>% 
  ggplot(aes(y = n_per_mp, x = year_orig, color = party)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ factor(party, levels = c('SPD', 'CDU/CSU', 'FDP', 'GRUENE', 'LINKE', 'AfD')))

vline_data <- data.frame(xintercept = 1957, party = "SPD")

plot_desc_trend <- plot_desc_trend +  # Create facets by "group"
  geom_vline(data = vline_data, 
             aes(xintercept = xintercept), 
             linetype = "dashed", 
             color = "red") +
  scale_color_manual(values = party_colors) +
  theme(legend.position = 'none') +
  labs(y = 'Mentions of worker keywords (party average)', x = 'Year')


# # save plot
# ggsave(plot = plot_desc_trend,
#        filename = 'Working class appeals used by German parties (1949-2021).PNG',
#        path = result_path, 
#        dpi = 300, width = 7, height = 6)


# calculate difference between SPD and CSU over time
data_plot_diff <- df_aggregate %>% 
  select(party, n_per_mp, year_orig) %>% 
  pivot_wider(names_from = party, values_from = n_per_mp) %>% 
  mutate(diff_spd_cdu = SPD - `CDU/CSU`)

# plot difference between SPD and CSU over time
plot_diff <- data_plot_diff %>% 
  ggplot(aes(y = diff_spd_cdu, x = year_orig)) +
  geom_line() +
  geom_point() +
  labs(x = 'Year', y = 'Difference in mentions of worker keywords (SPD - CDU/CSU)')


# # save plot
# ggsave(plot = plot_diff,
#        filename = 'Differences in working class appeals between Conservatives and SPD over time (1949-2021).PNG',
#        path = result_path, 
#        dpi = 300, width = 7, height = 6)

# plot difference between SPD and CSU over time
plot_diff_fdp <- data_plot_diff %>% 
  mutate(diff_spd_fdp = SPD - FDP) %>% 
  ggplot(aes(y = diff_spd_fdp, x = year_orig)) +
  geom_line() +
  geom_point() +
  labs(x = 'Year', y = 'Difference in mentions of worker keywords (SPD - FDP)')

# # save plot
# ggsave(plot = plot_diff_fdp,
#        filename = 'Differences in working class appeals between FDP and SPD over time (1949-2021).PNG',
#        path = result_path, 
#        dpi = 300, width = 7, height = 6)


# plot mentions of employee per MP and party
plot_desc_trend_employee <- df_aggregate %>% 
  ggplot(aes(y = n_per_mp_employee, x = year_orig, color = party)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ factor(party, levels = c('SPD', 'CDU/CSU', 'FDP', 'GRUENE', 'LINKE', 'AfD'))) +  # Create facets by "group"
  geom_vline(data = vline_data, 
             aes(xintercept = xintercept), 
             linetype = "dashed", 
             color = "red") +
  scale_color_manual(values = party_colors) +
  theme(legend.position = 'none') +
  labs(y = "Mentions of 'employee' (party average)", x = 'Year')

# # save plot
# ggsave(plot = plot_desc_trend_employee,
#        filename = "Mentions of 'employee' in Bundestag speeches (1949-2021).PNG",
#        path = result_path, 
#        dpi = 300, width = 7, height = 6)

# plot share of industry over time
plot_desc_trend_industry <- df_aggregate %>% 
  distinct(year_orig, industry_share) %>% 
  ggplot(aes(y = industry_share, x = year_orig)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100))


# # save plot
# ggsave(plot = plot_desc_trend_industry,
#        filename = "Employment in production industry in Germany, proportion in % (1954-2021).PNG",
#        path = result_path, 
#        dpi = 300, width = 7, height = 6)

# plot share of industry over time --> alternative version with different y-axis ticks
plot_desc_trend_industry <- df_aggregate %>% 
  distinct(year_orig, industry_share) %>% 
  ggplot(aes(y = industry_share, x = year_orig)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = seq(20, 60, by = 10), limits = c(20,60)) +
  labs(y = 'Employment in production industry in Germany, proportion in %', x = 'Year')


# # save plot
# ggsave(plot = plot_desc_trend_industry,
#        filename = "Employment in production industry in Germany, proportion in % (1954-2021).PNG",
#        path = result_path, 
#        dpi = 300, width = 7, height = 6)

# Regression models -------------------------------------------------------

# Splines -----------------------------------------------------------------

# filter for SPD + CDU/CSU + FDP
df_aggregate <- df_aggregate %>% 
  filter(party %in% c('CDU/CSU', 'FDP', 'SPD'))

df_aggregate$party <- as.character(df_aggregate$party )

library(splines)

model <- gls(n_per_mp ~ ns(year, df = 4) + industry_share*party,
             correlation = corCAR1(form = ~ year | party),
             data =  df_aggregate %>%
               filter(!is.na(industry_cat)))

model_list <- list()

model <- gls(n_per_mp ~ industry_cat + party,
             correlation = corCAR1(form = ~ year | party),
             data =  df_aggregate %>% 
               filter(!is.na(industry_cat)))


model_list$baseline_model <- model

# industry_cat
model <- gls(n_per_mp ~ industry_cat*party + ns(year, df = 4),
             correlation = corCAR1(form = ~ year | party),
             data =  df_aggregate %>% 
               filter(!is.na(industry_cat)))

model_list$first_model <- model

# industry_cat + opposition
model <- gls(n_per_mp ~ industry_cat*party + ns(year, df = 4) + unemployment*party,
             correlation = corCAR1(form = ~ year | party),
             data =  df_aggregate %>% 
               filter(!is.na(industry_cat)))

model_list$second_model <- model


# industry_cat + opposition + unemployment
model <- gls(n_per_mp ~ industry_cat*party + ns(year, df = 4) + unemployment*party + opposition,
             correlation = corCAR1(form = ~ year | party),
             data =  df_aggregate %>% 
               filter(!is.na(industry_cat)))

model_list$third_model <- model

unique(df_aggregate$industry_cat)
summary(model)
plot_pred <- plot_predictions(model, by = c("industry_cat", 'party'))

plot_pred <- plot_pred  +
  scale_color_manual(values = party_colors) +
  labs(x = 'Employment in production industry in Germany, proportion', 
       y = 'Predicted mentions of worker keywords (party average)') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_pred

# save plot
# ggsave(plot = plot_pred,
#        filename = "Conditional predictions (Effect of share of industrial workers on working class appeals).PNG",
#        path = result_path, 
#        dpi = 300, width = 7, height = 6)


# Alternative specification: year + period dummy --------------------------


model_list_alternative <- list()

model <- gls(n_per_mp ~ industry_cat + party,
             correlation = corCAR1(form = ~ year | party),
             data =  df_aggregate %>% 
               filter(!is.na(industry_cat)))


model_list_alternative$baseline_model <- model

# industry_cat
model <- gls(n_per_mp ~ industry_cat*party + year*before_1970,
             correlation = corCAR1(form = ~ year | party),
             data =  df_aggregate %>% 
               filter(!is.na(industry_cat)))

model_list_alternative$first_model <- model

# industry_cat + opposition
model <- gls(n_per_mp ~ industry_cat*party + year*before_1970 + unemployment*party,
             correlation = corCAR1(form = ~ year | party),
             data =  df_aggregate %>% 
               filter(!is.na(industry_cat)))

model_list_alternative$second_model <- model


# industry_cat + opposition + unemployment
model <- gls(n_per_mp ~ industry_cat*party + year*before_1970 + unemployment*party + opposition,
             correlation = corCAR1(form = ~ year | party),
             data =  df_aggregate %>% 
               filter(!is.na(industry_cat)))


model_list_alternative$third_model <- model

plot_pred_alternative <- plot_predictions(model, by = c("industry_cat", 'party'))

plot_pred_alternative <- plot_pred_alternative  +
  scale_color_manual(values = party_colors)





