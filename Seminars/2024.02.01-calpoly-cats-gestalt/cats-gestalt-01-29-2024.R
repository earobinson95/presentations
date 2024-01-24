# https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-05-09/readme.md

library(tidyverse)

# read in data

childcare_costs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/childcare_costs.csv')
counties <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/counties.csv')

# join + clean data

ca_childcare <- full_join(counties,
                 childcare_costs,
                 ) |> 
  filter(state_abbreviation == "CA",
         county_name %in% c("San Luis Obispo County", 
                            "Orange County", 
                            "San Francisco County")
         ) |> 
  pivot_longer(cols = starts_with("mfcc_"),
               names_to = "development_stage",
               values_to = "median_weekly_childcare_cost") |>
  filter(development_stage %in% c("mfcc_infant", "mfcc_toddler")) |> 
  mutate(across(.cols = c(county_name, state_abbreviation, development_stage), 
                .fns = ~ as.factor(.x)
                ),
         development_stage = as.factor(str_remove(development_stage, pattern = "mfcc_"))
         ) |> 
  select(county_name, study_year, development_stage, median_weekly_childcare_cost)

summary(ca_childcare)

# Do historical trends of full-time weekly median price charged for family child 
# vary based on development stage in the three given California counties?


# graph 1

ca_childcare |> 
  ggplot(aes(x = study_year,
             y = median_weekly_childcare_cost,
             fill = county_name)
         ) +
  geom_bar(stat = "identity",
           position = "dodge") +
  facet_wrap(~ development_stage) +
  scale_x_continuous(breaks = seq(2008, 2018, 2)) +
  scale_fill_brewer(palette = "Accent") +
  theme_bw() +
  labs(x = "Year",
       y = "Weekly Median Childcare Cost ($)",
       fill = "County")

# graph 2

ca_childcare |> 
  ggplot(aes(x = county_name,
             y = median_weekly_childcare_cost,
             fill = as.factor(study_year))
  ) +
  geom_bar(stat = "identity",
           position = "dodge") +
  facet_wrap(~ development_stage) +
  # scale_x_continuous(breaks = seq(2008, 2018, 2)) +
  scale_fill_brewer(palette = "Accent") +
  theme_bw() +
  labs(x = "Year",
       y = "Weekly Median Childcare Cost ($)",
       fill = "County")


# graph 3

ca_childcare |> 
  ggplot(aes(x = study_year,
             y = median_weekly_childcare_cost,
             color = development_stage,
             shape = development_stage)
  ) +
  geom_point() +
  geom_line() +
  facet_wrap(~ county_name) +
  scale_x_continuous(breaks = seq(2008, 2018, 2)) +
  scale_color_manual(values = c("steelblue", "orange3")) +
  theme_bw() +
  labs(x = "Year",
       y = "Weekly Median Childcare Cost ($)",
       color = "Development \nStage",
       shape = "Development \nStage")

# graph 4

ca_childcare |> 
  ggplot(aes(x = study_year,
             y = median_weekly_childcare_cost,
             color = development_stage,
             shape = development_stage)
  ) +
  geom_point() +
  geom_line() +
  facet_wrap(~ county_name, ncol = 1) +
  scale_x_continuous(breaks = seq(2008, 2018, 2)) +
  scale_color_manual(values = c("steelblue", "orange3")) +
  theme_bw() +
  labs(x = "Year",
       y = "Weekly Median Childcare Cost ($)",
       color = "Development \nStage",
       shape = "Development \nStage")
