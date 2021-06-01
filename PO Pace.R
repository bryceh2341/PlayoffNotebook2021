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

theme_owen <- function () { 
  theme_minimal(base_size=12, base_family="Consolas") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

POPace <- read.csv("POPace.csv")
POPace <- POPace %>%
  select(Season, Pace, Pace.Off.Missed.FG, Pace.Off.Steal)

RPace <- read.csv("RPace.csv")
RPace <- RPace %>%
  select(Season, Pace, Pace.Off.Missed.FG, Pace.Off.Steal)

AllPace <- merge(POPace, RPace, by="Season")

ggplot() +
  geom_line(data = AllPace, aes(x=Season, y=Pace.x), color = "red", size=3) +
  geom_line(data = AllPace, aes(x=Season, y=Pace.y), color = "blue", size=3) +
  theme_owen() +
  theme(legend.position="top") +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 10),
        plot.subtitle = element_text(size = 8),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) + 
  labs(x = "Season", 
       y = "Seconds per Possession", 
       title = "Pace Over the Past 20 Seasons", 
       subtitle = paste0("2000/01 - 2020/21\nRgular Season in Blue | Playoffs in Red\nUpdated ", format(Sys.Date(), "%B %d, %Y")))

ggplot() +
  geom_line(data = AllPace, aes(x=Season, y=Pace.Off.Missed.FG.x), color = "red", size=3) +
  geom_line(data = AllPace, aes(x=Season, y=Pace.Off.Missed.FG.y), color = "blue", size=3) +
  theme_owen() +
  theme(legend.position="top") +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 10),
        plot.subtitle = element_text(size = 8),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) + 
  labs(x = "Season", 
       y = "Seconds per Possession", 
       title = "Pace After Missed Shots Over the Past 20 Seasons", 
       subtitle = paste0("2000/01 - 2020/21\nRgular Season in Blue | Playoffs in Red\nUpdated ", format(Sys.Date(), "%B %d, %Y")))

ggplot() +
  geom_line(data = AllPace, aes(x=Season, y=Pace.Off.Steal.x), color = "red", size=3) +
  geom_line(data = AllPace, aes(x=Season, y=Pace.Off.Steal.y), color = "blue", size=3) +
  theme_owen() +
  theme(legend.position="top") +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 10),
        plot.subtitle = element_text(size = 8),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) + 
  labs(x = "Season", 
       y = "Seconds per Possession", 
       title = "Pace After Steals Over the Past 20 Seasons", 
       subtitle = paste0("2000/01 - 2020/21\nRgular Season in Blue | Playoffs in Red\nUpdated ", format(Sys.Date(), "%B %d, %Y")))
