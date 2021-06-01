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
library(future)
library(furrr)
library(ggrepel)
library(ggtext)

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

get_data <- function(year) {
  
  url <- paste0("https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=SpeedDistance&Season=", year, "&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=")
  
  res <- GET(url = url, add_headers(.headers=headers))
  json_resp <- fromJSON(content(res, "text"))
  df <- data.frame(json_resp$resultSets$rowSet)
  colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]
  
  return(df)
  
}

get_data_PO <- function(year) {
  
  url <- paste0("https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=SpeedDistance&Season=", year, "&SeasonSegment=&SeasonType=Playoffs&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=")
  
  res <- GET(url = url, add_headers(.headers=headers))
  json_resp <- fromJSON(content(res, "text"))
  df <- data.frame(json_resp$resultSets$rowSet)
  colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]
  
  return(df)
  
}

years <- c("2014-15", "2015-16", "2016-17", "2017-18", "2019-20", "2020-21")

RSD <- future_map_dfr(years, get_data)
RSD[c("DIST_MILES", "AVG_SPEED")] <- sapply(RSD[c("DIST_MILES", "AVG_SPEED")], as.numeric)
RSD <- RSD %>%
  filter(PLAYER_NAME == "LeBron James") %>%
  mutate(Season = c(15, 16, 17, 18, 20, 21)) %>%
  select(Season, DIST_MILES, AVG_SPEED)

POSD <- future_map_dfr(years, get_data_PO)
POSD[c("DIST_MILES", "AVG_SPEED")] <- sapply(POSD[c("DIST_MILES", "AVG_SPEED")], as.numeric)
POSD <- POSD %>%
  filter(PLAYER_NAME == "LeBron James") %>%
  mutate(Season = c(15, 16, 17, 18, 20, 21)) %>%
  select(Season, DIST_MILES, AVG_SPEED)

All <- merge(RSD, POSD, by="Season")

ggplot() +
  geom_line(data = RSD, aes(x=Season, y=DIST_MILES), color = "red", size=3) +
  geom_line(data = POSD, aes(x=Season, y=DIST_MILES), color = "blue", size=3) +
  theme_owen() +
  ylim(2,2.75) +
  theme(legend.position="top") +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 10),
        plot.subtitle = element_text(size = 8),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
  labs(x = "Year",
       y = "Distance (miles)",
       title = "LeBron James Distance",
       subtitle = paste0("Distance Per Game\nRgular Season in Red | Playoffs in Blue\nUpdated ", format(Sys.Date(), "%B %d, %Y")))

ggplot() +
  geom_line(data = RSD, aes(x=Season, y=AVG_SPEED), color = "red", size=3) +
  geom_line(data = POSD, aes(x=Season, y=AVG_SPEED), color = "blue", size=3) +
  theme_owen() +
  ylim(3.4,4) +
  theme(legend.position="top") +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 10),
        plot.subtitle = element_text(size = 8),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
  labs(x = "Year",
       y = "Avergae Speed (MPH)",
       title = "LeBron James Speed",
       subtitle = paste0("Average Speed\nRgular Season in Red | Playoffs in Blue\nUpdated ", format(Sys.Date(), "%B %d, %Y")))
