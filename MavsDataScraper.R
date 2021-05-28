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

url <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=Isolation&PlayerOrTeam=T&SeasonType=Playoffs&SeasonYear=2020-21&TypeGrouping=offensive"
url_1 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=Transition&PlayerOrTeam=T&SeasonType=Playoffs&SeasonYear=2020-21&TypeGrouping=offensive"  
url_2 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=PRBallHandler&PlayerOrTeam=T&SeasonType=Playoffs&SeasonYear=2020-21&TypeGrouping=offensive"
url_3 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=PRRollman&PlayerOrTeam=T&SeasonType=Playoffs&SeasonYear=2020-21&TypeGrouping=offensive"
url_4 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=Postup&PlayerOrTeam=T&SeasonType=Playoffs&SeasonYear=2020-21&TypeGrouping=offensive"
url_5 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=Spotup&PlayerOrTeam=T&SeasonType=Playoffs&SeasonYear=2020-21&TypeGrouping=offensive"
url_6 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=Handoff&PlayerOrTeam=T&SeasonType=Playoffs&SeasonYear=2020-21&TypeGrouping=offensive"
url_7 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=Cut&PlayerOrTeam=T&SeasonType=Playoffs&SeasonYear=2020-21&TypeGrouping=offensive"
url_8 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=OffScreen&PlayerOrTeam=T&SeasonType=Playoffs&SeasonYear=2020-21&TypeGrouping=offensive"
url_9 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=OffRebound&PlayerOrTeam=T&SeasonType=Playoffs&SeasonYear=2020-21&TypeGrouping=offensive"
url_10 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=Misc&PlayerOrTeam=T&SeasonType=Playoffs&SeasonYear=2020-21&TypeGrouping=offensive"
url_11 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=Isolation&PlayerOrTeam=T&SeasonType=Regular+Season&SeasonYear=2020-21&TypeGrouping=offensive"
url_12 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=Transition&PlayerOrTeam=T&SeasonType=Regular+Season&SeasonYear=2020-21&TypeGrouping=offensive"  
url_13 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=PRBallHandler&PlayerOrTeam=T&SeasonType=Regular+Season&SeasonYear=2020-21&TypeGrouping=offensive"
url_14 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=PRRollman&PlayerOrTeam=T&SeasonType=Regular+Season&SeasonYear=2020-21&TypeGrouping=offensive"
url_15 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=Postup&PlayerOrTeam=T&SeasonType=Regular+Season&SeasonYear=2020-21&TypeGrouping=offensive"
url_16 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=Spotup&PlayerOrTeam=T&SeasonType=Regular+Season&SeasonYear=2020-21&TypeGrouping=offensive"
url_17 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=Handoff&PlayerOrTeam=T&SeasonType=Regular+Season&SeasonYear=2020-21&TypeGrouping=offensive"
url_18 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=Cut&PlayerOrTeam=T&SeasonType=Regular+Season&SeasonYear=2020-21&TypeGrouping=offensive"
url_19 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=OffScreen&PlayerOrTeam=T&SeasonType=Regular+Season&SeasonYear=2020-21&TypeGrouping=offensive"
url_20 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=OffRebound&PlayerOrTeam=T&SeasonType=Regular+Season&SeasonYear=2020-21&TypeGrouping=offensive"
url_21 <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=Misc&PlayerOrTeam=T&SeasonType=Regular+Season&SeasonYear=2020-21&TypeGrouping=offensive"


res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
POIso <- data.frame(json_resp$resultSets$rowSet)
colnames(POIso) <- json_resp[["resultSets"]][["headers"]][[1]]

res <- GET(url = url_1, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
POTransition <- data.frame(json_resp$resultSets$rowSet)
colnames(POTransition) <- json_resp[["resultSets"]][["headers"]][[1]]

res <- GET(url = url_2, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
POPnRHand <- data.frame(json_resp$resultSets$rowSet)
colnames(POPnRHand) <- json_resp[["resultSets"]][["headers"]][[1]]

res <- GET(url = url_3, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
POPnRRoll <- data.frame(json_resp$resultSets$rowSet)
colnames(POPnRRoll) <- json_resp[["resultSets"]][["headers"]][[1]]

res <- GET(url = url_4, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
POPostup <- data.frame(json_resp$resultSets$rowSet)
colnames(POPostup) <- json_resp[["resultSets"]][["headers"]][[1]]

res <- GET(url = url_5, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
POSpotup <- data.frame(json_resp$resultSets$rowSet)
colnames(POSpotup) <- json_resp[["resultSets"]][["headers"]][[1]]

res <- GET(url = url_6, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
POHandoff <- data.frame(json_resp$resultSets$rowSet)
colnames(POHandoff) <- json_resp[["resultSets"]][["headers"]][[1]]

res <- GET(url = url_7, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
POCut <- data.frame(json_resp$resultSets$rowSet)
colnames(POCut) <- json_resp[["resultSets"]][["headers"]][[1]]

res <- GET(url = url_8, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
POOffScreen <- data.frame(json_resp$resultSets$rowSet)
colnames(POOffScreen) <- json_resp[["resultSets"]][["headers"]][[1]]

res <- GET(url = url_9, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
POPutback <- data.frame(json_resp$resultSets$rowSet)
colnames(POPutback) <- json_resp[["resultSets"]][["headers"]][[1]]

res <- GET(url = url_10, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
POMisc <- data.frame(json_resp$resultSets$rowSet)
colnames(POMisc) <- json_resp[["resultSets"]][["headers"]][[1]]

AllPO <- rbind(POIso, POCut, POHandoff, POMisc, POOffScreen, POPnRHand, POPnRRoll, POPostup, POPutback, POSpotup)
AllPO <- AllPO %>%
  select(TEAM_ABBREVIATION, PLAY_TYPE, PERCENTILE, POSS_PCT, PPP, EFG_PCT) %>%
  filter(TEAM_ABBREVIATION == "DAL")
colnames(AllPO) <- c("ABB", "PLAY_TYPE", "PERCENTILE_PO", "POSS_PCT_PO", "PPP_PO", "EFG_PCT_PO")

res <- GET(url = url_11, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
RIso <- data.frame(json_resp$resultSets$rowSet)
colnames(RIso) <- json_resp[["resultSets"]][["headers"]][[1]]

res <- GET(url = url_12, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
RTransition <- data.frame(json_resp$resultSets$rowSet)
colnames(RTransition) <- json_resp[["resultSets"]][["headers"]][[1]]

res <- GET(url = url_13, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
RPnRHand <- data.frame(json_resp$resultSets$rowSet)
colnames(RPnRHand) <- json_resp[["resultSets"]][["headers"]][[1]]

res <- GET(url = url_14, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
RPnRRoll <- data.frame(json_resp$resultSets$rowSet)
colnames(RPnRRoll) <- json_resp[["resultSets"]][["headers"]][[1]]

res <- GET(url = url_15, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
RPostup <- data.frame(json_resp$resultSets$rowSet)
colnames(RPostup) <- json_resp[["resultSets"]][["headers"]][[1]]

res <- GET(url = url_16, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
RSpotup <- data.frame(json_resp$resultSets$rowSet)
colnames(RSpotup) <- json_resp[["resultSets"]][["headers"]][[1]]

res <- GET(url = url_17, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
RHandoff <- data.frame(json_resp$resultSets$rowSet)
colnames(RHandoff) <- json_resp[["resultSets"]][["headers"]][[1]]

res <- GET(url = url_18, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
RCut <- data.frame(json_resp$resultSets$rowSet)
colnames(RCut) <- json_resp[["resultSets"]][["headers"]][[1]]

res <- GET(url = url_19, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
ROffScreen <- data.frame(json_resp$resultSets$rowSet)
colnames(ROffScreen) <- json_resp[["resultSets"]][["headers"]][[1]]

res <- GET(url = url_20, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
RPutback <- data.frame(json_resp$resultSets$rowSet)
colnames(RPutback) <- json_resp[["resultSets"]][["headers"]][[1]]

res <- GET(url = url_21, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
RMisc <- data.frame(json_resp$resultSets$rowSet)
colnames(RMisc) <- json_resp[["resultSets"]][["headers"]][[1]]

AllR <- rbind(RIso, RCut, RHandoff, RMisc, ROffScreen, RPnRHand, RPnRRoll, RPostup, RPutback, RSpotup)
AllR <- AllR %>%
  filter(TEAM_ABBREVIATION == "DAL") %>%
  select(PLAY_TYPE, PERCENTILE, POSS_PCT, PPP, EFG_PCT)
colnames(AllR) <- c("PLAY_TYPE", "PERCENTILE_R", "POSS_PCT_R", "PPP_R", "EFG_PCT_R")

All <- merge(AllPO, AllR, by="PLAY_TYPE")

write.csv(All, "MavsData.csv", row.names = FALSE)