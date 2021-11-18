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

headers <- c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/shooting/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

ATL<- "https://api.pbpstats.com/get-game-logs/nba?Season=2020-21&SeasonType=Playoffs&EntityId=1610612737&EntityType=Team"
MIL <- "https://api.pbpstats.com/get-game-logs/nba?Season=2020-21&SeasonType=Playoffs&EntityId=1610612749&EntityType=Team"
League <- "https://api.pbpstats.com/get-totals/nba?Season=2020-21&SeasonType=Regular%2BSeason&StartType=All&Type=Team"

json_data <- fromJSON(paste(readLines(League), collapse=""))
League <- json_data[["multi_row_table_data"]]
League <- League %>%
  clean_names() %>%
  filter(name == "ATL" | name == "MIL") %>%
  mutate(Shot_Making = efg_pct - shot_quality_avg)

json_data <- fromJSON(paste(readLines(ATL), collapse=""))
ATL<- json_data[["multi_row_table_data"]]
ATL<- ATL%>%
  clean_names() %>%
  slice(13:17) %>%
  mutate(Shot_Making = efg_pct - shot_quality_avg - League$Shot_Making[2]) %>%
  mutate(PtsEx = Shot_Making * (fg2a+fg3a) * 2) %>%
  mutate(Team = "Hawks") %>%
  mutate(Game = c(1, 2, 3, 4, 5)) %>%
  select(Team, PtsEx, Shot_Making, Game)

json_data <- fromJSON(paste(readLines(MIL), collapse=""))
MIL <- json_data[["multi_row_table_data"]]
MIL <- MIL %>%
  clean_names() %>%
  slice(12:16) %>%
  mutate(Shot_Making = efg_pct - shot_quality_avg - League$Shot_Making[1]) %>%
  mutate(PtsEx = Shot_Making * (fg2a+fg3a) * 2) %>%
  mutate(Team = "Bucks") %>%
  mutate(Game = c(1, 2, 3, 4, 5)) %>%
  select(Team, PtsEx, Shot_Making, Game)

data = rbind(ATL, MIL)

ggplot(data, aes(fill=Team, y=PtsEx, x=Game)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("#00471B", "#E03A3E")) +
  theme_owen() +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 15),
        plot.subtitle = element_text(size = 11),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
  labs(x = "Game #",
       y = "Points +/- Expected",
       title = "Eastern Conference Finals Shot Making",
       subtitle = paste0("Points vs. Expected | Shot Making is eFG - Shot Quality\nUpdated ", format(Sys.Date(), "%B %d, %Y")))