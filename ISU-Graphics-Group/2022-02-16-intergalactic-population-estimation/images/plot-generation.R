library(readr)
library(tidyverse)
library(scales)
library(patchwork)

simulated_data  <- read_csv("ISU-Graphics-Group/2022-02-16-intergalatic-population-estimation/data/simulated-data.csv")

simulated_data %>%
  ggplot(aes(x = x, y = y, color = dataset)) +
  geom_point()

rand <- tibble(
  dataset = sample(c("dataset1", "dataset2"), 2, replace = F),
  creature = sample(c("tribble", "ewok"), 2, replace = F),
  scale    = sample(c("linear", "log2"), 2, replace = F)
) 

simulated_data <- rand %>%
  right_join(simulated_data, by = "dataset") %>%
  mutate(date = ifelse(creature == "tribble", x + 1500, x - 3000))
simulated_data

simulated_data %>%
  ggplot(aes(x = x, y = y, color = creature)) +
  geom_point()

# tribble plot (linear)
tribblePlot <- simulated_data %>%
  filter(creature == "tribble") %>%
  ggplot(aes(x = date, y = y)) +
  geom_point() +
  theme_bw() +
  theme(aspect.ratio = 1,
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
  ) +
  facet_grid(~creature + scale) +
  scale_x_continuous("Stardate", expand = c(0.01,0.01)) + 
  scale_y_continuous("Tribble Population",
                     limits = c(100, 55000),
                     breaks = seq(0, 55000, 5000),
                     labels = comma,
                     minor_breaks = c())


# ewok plot (log2)
ewokPlot <- simulated_data %>%
  filter(creature == "ewok") %>%
  ggplot(aes(x = date, y = y)) +
  geom_point() +
  theme_bw() +
  theme(aspect.ratio = 1,
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
  ) +
  facet_grid(~creature + scale) +
    scale_x_continuous("ABY \n (After the Battle of Yavin)", expand = c(0.01,0.01)) + 
    scale_y_continuous("Ewok Population",
                       trans = "log2",
                       limits = c(100, 55000),
                       breaks = 2^seq(0,10000,1),
                       labels = comma,
                       # minor_breaks = c(2^seq(0,10000,1) + (2^seq(0,10000,1)/2), 2^seq(0,10000,1) + 3*(2^seq(0,10000,1)/4))
                       # minor_breaks = 2^seq(0,10000,1) + (2^seq(0,10000,1)/2)
                       # minor_breaks = seq(0, 55000, 5000)
                       # minor_breaks = c(156, 312)
                       minor_breaks = c()
                       )


finalPlot <- tribblePlot + ewokPlot
finalPlot
ggsave(finalPlot, filename = "ISU-Graphics-Group/2022-02-16-intergalatic-population-estimation/images/simulated-data-plot.jpg", width = 11, height = 5)
