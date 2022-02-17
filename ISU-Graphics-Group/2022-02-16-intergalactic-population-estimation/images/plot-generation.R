library(readr)
library(tidyverse)
library(scales)
library(patchwork)

simulated_data  <- read_csv("ISU-Graphics-Group/2022-02-16-intergalactic-population-estimation/data/simulated-data.csv")

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
ggsave(finalPlot, filename = "ISU-Graphics-Group/2022-02-16-intergalactic-population-estimation/images/simulated-data-plot.jpg", width = 11, height = 5)


# Breaks

baseLogPlot <- simulated_data %>%
  filter(dataset == "dataset1") %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  theme_bw() +
  theme(aspect.ratio = 1,
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
  ) +
  scale_x_continuous("Year", expand = c(0.01,0.01))

p1 <- baseLogPlot + 
  scale_y_continuous("Population",
                     trans = "log2",
                     limits = c(100, 55000),
                     breaks = 2^seq(0,10000,1),
                     labels = comma,
                     minor_breaks = c()
  )

ggsave(p1, filename = "ISU-Graphics-Group/2022-02-16-intergalactic-population-estimation/images/log-no-minor-breaks.jpg", width = 4, height = 4, dpi = 300)


p2 <- baseLogPlot + 
  scale_y_continuous("Population",
                     trans = "log2",
                     limits = c(100, 55000),
                     breaks = 2^seq(0,10000,1),
                     labels = comma,
                     minor_breaks = 2^seq(0,10000,1) + (2^seq(0,10000,1)/2)
  )
ggsave(p2, filename = "ISU-Graphics-Group/2022-02-16-intergalactic-population-estimation/images/log-half-minor-breaks.jpg", width = 4, height = 4)

p3 <- baseLogPlot + 
  scale_y_continuous("Population",
                     trans = "log2",
                     limits = c(100, 55000),
                     breaks = 2^seq(0,10000,1),
                     labels = comma,
                     minor_breaks = c(2^seq(0,10000,1) + (2^seq(0,10000,1)/2), 2^seq(0,10000,1) + 3*(2^seq(0,10000,1)/4))
  )
ggsave(p3, filename = "ISU-Graphics-Group/2022-02-16-intergalactic-population-estimation/images/log-half-threequarter-minor-breaks.jpg", width = 4, height = 4)

p4 <- baseLogPlot + 
  scale_y_continuous("Population",
                     trans = "log2",
                     limits = c(100, 55000),
                     breaks = 2^seq(0,10000,1),
                     labels = comma,
                     minor_breaks = c(2^seq(0,10000,1) + (2^seq(0,10000,1)/4), 2^seq(0,10000,1) + (2^seq(0,10000,1)/2), 2^seq(0,10000,1) + 3*(2^seq(0,10000,1)/4))
  )
ggsave(p4, filename = "ISU-Graphics-Group/2022-02-16-intergalactic-population-estimation/images/log-quarter-half-threequarter-minor-breaks.jpg", width = 4, height = 4)

p5 <- baseLogPlot + 
  scale_y_continuous("Population",
                     trans = "log2",
                     limits = c(100, 55000),
                     breaks = 2^seq(0,10000,1),
                     labels = comma)
ggsave(p5, filename = "ISU-Graphics-Group/2022-02-16-intergalactic-population-estimation/images/log-default-minor-breaks.jpg", width = 4, height = 4)
