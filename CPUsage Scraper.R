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
  
  url <- paste0("https://stats.nba.com/stats/playerdashboardbyyearoveryear?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=101108&PlusMinus=N&Rank=N&Season=", year,"&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&Split=yoy&VsConference=&VsDivision=")
  
  res <- GET(url = url, add_headers(.headers=headers))
  json_resp <- fromJSON(content(res, "text"))
  df <- data.frame(json_resp$resultSets$rowSet)
  colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]
  
  return(df)
  
}

get_data_PO <- function(year) {
  
  url <- paste0("https://stats.nba.com/stats/playerdashboardbyyearoveryear?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=101108&PlusMinus=N&Rank=N&Season=", year,"&SeasonSegment=&SeasonType=Playoffs&ShotClockRange=&Split=yoy&VsConference=&VsDivision=")
  
  res <- GET(url = url, add_headers(.headers=headers))
  json_resp <- fromJSON(content(res, "text"))
  df <- data.frame(json_resp$resultSets$rowSet)
  colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]
  
  return(df)
  
}

years <- c("2007-08", "2008-09", "2010-11", "2011-12", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18", "2018-19", "2019-20", "2020-21")

RUsage <- future_map_dfr(years, get_data)
POUsage <- future_map_dfr(years, get_data_PO)

RUsage <- RUsage %>%
  slice(1, 17, 33, 49, 65, 81, 97, 113, 129, 145, 161, 177, 193, 209) %>%
  select(GROUP_VALUE, E_USG_PCT)
POUsage <- POUsage %>%
  slice(1, 14, 27, 40, 53, 66, 79, 92, 105, 118, 131, 144, 157, 172) %>%
  select(GROUP_VALUE, E_USG_PCT)

All_Usage <- merge(RUsage, POUsage, by="GROUP_VALUE")

write.csv(All_Usage, "CP3Usage.csv", row.names = FALSE)