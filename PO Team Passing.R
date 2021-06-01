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
library(janitor)
library(hablar)
library(ggforce)
library(ggbrace)
library(magick)
library(ggtext)
library(dplyr)
library(ggimage)
library(grImport2)
library(ggplot2)

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

#get nba playerIds and headshots
teams <- nbastatR::nba_teams()
teams <- teams %>% select(nameTeam, urlThumbnailTeam)
colnames(teams)[which(names(teams) == "nameTeam")] <- "TEAM_NAME"
teams[teams$TEAM_NAME == "Los Angeles Clippers", "TEAM_NAME"] <- "LA Clippers"
teams <- subset(teams, urlThumbnailTeam!= "https://stats.nba.com/media/img/teams/logos/NBA_logo.svg")

url <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=Passing&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
url_1 <- "https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
PO_Pass <- data.frame(json_resp$resultSets$rowSet)
colnames(PO_Pass) <- json_resp[["resultSets"]][["headers"]][[1]]
PO_Pass <- PO_Pass %>%
  select(TEAM_NAME, PASSES_MADE, POTENTIAL_AST)

res <- GET(url = url_1, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
PO_Gen <- data.frame(json_resp$resultSets$rowSet)
colnames(PO_Gen) <- json_resp[["resultSets"]][["headers"]][[1]]
PO_Gen <- PO_Gen %>%
  select(TEAM_NAME, OFF_RATING)

combined <- merge(PO_Pass, PO_Gen, by="TEAM_NAME")
combined <- merge(teams, combined, by="TEAM_NAME")
combined[c("PASSES_MADE", "POTENTIAL_AST", "OFF_RATING")] <- sapply(combined[c("PASSES_MADE", "POTENTIAL_AST", "OFF_RATING")], as.numeric)


ggplot(combined, aes(POTENTIAL_AST, OFF_RATING, image = urlThumbnailTeam)) +
  xlim(30, 50) +
  ylim(95, 130) +
  geom_image(size = 0.1) +
  geom_smooth(method=lm) +
  theme_owen() +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 10),
        plot.subtitle = element_text(size = 8),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) + 
  labs(x = "Potential Assists", 
       y = "Offensive Rating", 
       title = "Does Passing Generate Offense?", 
       subtitle = paste0("Stats per NBA.com\nUpdated ", format(Sys.Date(), "%B %d, %Y")))

# ggplot(combined, aes(PASSES_MADE, OFF_RATING, image = urlThumbnailTeam)) +
#   xlim(225, 310) +
#   ylim(95, 130) +
#   geom_image(size = 0.1) +
#   #geom_smooth(method=lm) +
#   theme_owen() +
#   theme(strip.text.x = element_blank(),
#         panel.spacing.x = unit(1, "lines"),
#         plot.title.position = 'plot',
#         plot.title = element_text(face =  'bold', size = 10),
#         plot.subtitle = element_text(size = 8),
#         plot.margin = unit(c(.5, .5, 1, .5), "lines")) + 
#   labs(x = "Passes Made", 
#        y = "Offensive Rating", 
#        title = "Does Passing Generate Offense?", 
#        subtitle = paste0("Stats per NBA.com\nUpdated ", format(Sys.Date(), "%B %d, %Y")))

ggplot(combined, aes(POTENTIAL_AST, OFF_RATING)) +
  xlim(30, 50) +
  ylim(95, 130) +
  geom_smooth(method=lm) +
  theme_owen() +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 10),
        plot.subtitle = element_text(size = 8),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) + 
  labs(x = "Potential Assists", 
       y = "Offensive Rating", 
       title = "Does Passing Generate Offense?", 
       subtitle = paste0("Stats per NBA.com\nUpdated ", format(Sys.Date(), "%B %d, %Y")))

ggplot(combined, aes(PASSES_MADE, OFF_RATING)) +
  xlim(225, 310) +
  ylim(95, 130) +
  geom_smooth(method=lm) +
  theme_owen() +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 10),
        plot.subtitle = element_text(size = 8),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
  labs(x = "Passes Made",
       y = "Offensive Rating",
       title = "Does Passing Generate Offense?",
       subtitle = paste0("Stats per NBA.com\nUpdated ", format(Sys.Date(), "%B %d, %Y")))
