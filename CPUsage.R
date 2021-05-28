library(tidyverse)
library(extrafont)
library(jsonlite)
library(httr)
library(hablar)
library(janitor)
library(paletteer)
library(prismatic)
library(scales)
library(nbastatR)
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

All <- read.csv("CP3Usage.csv")

All <- All %>%
  slice(1,2,3,4,8,9,10,11,12,13,14,15,16) %>%
  mutate(Year = c(8,9,11,12,13,14,15,16,17,18,19,20,21))

ggplot() +
  geom_line(data = All, aes(x=Year, y=E_USG_PCT.x), color = "red", size=3) +
  geom_line(data = All, aes(x=Year, y=E_USG_PCT.y), color = "blue", size=3) +
  theme_owen() +
  ylim(.1, .35) +
  theme(legend.position="top") +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 10),
        plot.subtitle = element_text(size = 8),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) + 
  labs(x = "Year", 
       y = "Usage Rate", 
       title = "Chris Paul Usage", 
       subtitle = paste0("Career Usage\nRgular Season in Red | Playoffs in Blue\nUpdated ", format(Sys.Date(), "%B %d, %Y")))
