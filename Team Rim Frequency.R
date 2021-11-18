library(tidyverse)
library(extrafont)
library(jsonlite)
library(httr)
library(hablar)
library(janitor)
library(paletteer)
library(prismatic)
library(scales)
library(tidyverse)
library(nbastatR)
library(extrafont)
library(ballr)
library(rvest)
library(ggforce)
library(ggbrace)
library(magick)
library(ggtext)
library(dplyr)
library(gt)
library(ggpubr)


theme_owen <- function () { 
  theme_minimal(base_size=12, base_family="Consolas") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

url <- "https://api.pbpstats.com/get-game-logs/nba?Season=2020-21&SeasonType=Playoffs&EntityId=1610612749&EntityType=Team"

json_data <- fromJSON(paste(readLines(url), collapse=""))
data <- json_data[["multi_row_table_data"]]
data[is.na(data)] = 0
data <- data %>% 
  clean_names() %>%
  #select(at_rim_frequency, off_poss, points) %>%
  slice(12:16) %>%
  mutate(Game = c(1:5)) %>%
  mutate(Diff = at_rim_frequency - .2909) %>%
  mutate(RDiff = at_rim_accuracy - .6765) %>%
  mutate(off_rtg = points / off_poss * 100 - 117.5)

ggplot(data = data, aes(x = Game, y = Diff, fill=Diff)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low="#00471B", mid="#2C5234", high="#EEE1C6") +
  scale_x_continuous(breaks = seq(1, 5, by = 1)) +
  ylim(-.10, .15) +
  theme_owen() +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        legend.position = "none",
        plot.title = element_text(face =  'bold', size = 15),
        plot.subtitle = element_text(size = 11),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
  labs(x = "Game #",
       y = "Rim Freq. +/- Avg.",
       title = "Bucks Rim Pressure",
       subtitle = paste0("Stats per pbpstats.com\nUpdated ", format(Sys.Date(), "%B %d, %Y")))

ggplot(data = data, aes(x = Game, y = RDiff, fill=RDiff)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low="#00471B", mid="#2C5234", high="#EEE1C6") +
  scale_x_continuous(breaks = seq(1, 5, by = 1)) +
  ylim(-.10, .20) +
  theme_owen() +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        legend.position = "none",
        plot.title = element_text(face =  'bold', size = 15),
        plot.subtitle = element_text(size = 11),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
  labs(x = "Game #",
       y = "Rim Accuracy +/- Avg.",
       title = "Bucks Rim Pressure",
       subtitle = paste0("Stats per pbpstats.com\nUpdated ", format(Sys.Date(), "%B %d, %Y")))


ggplot(data = data, aes(x = Game, y = off_rtg, fill=off_rtg)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low="#00471B", mid="#2C5234", high="#EEE1C6") +
  scale_x_continuous(breaks = seq(1, 5, by = 1)) +
  #ylim(-.10, .15) +
  theme_owen() +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        legend.position = "none",
        plot.title = element_text(face =  'bold', size = 15),
        plot.subtitle = element_text(size = 11),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
  labs(x = "Game #",
       y = "Offensive Rating +/- Avg.",
       title = "Bucks Offense",
       subtitle = paste0("Stats per pbpstats.com\nUpdated ", format(Sys.Date(), "%B %d, %Y")))
