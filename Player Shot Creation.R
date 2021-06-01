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

url <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Passing&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
url_1 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Passing&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
url_2 <- "https://stats.nba.com/stats/playerdashptshots?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PerMode=PerGame&Period=0&PlayerID=203999&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision="
url_3 <- "https://stats.nba.com/stats/playerdashptshots?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PerMode=PerGame&Period=0&PlayerID=203999&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&TeamID=0&VsConference=&VsDivision="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
passes_R <- data.frame(json_resp$resultSets$rowSet)
colnames(passes_R) <- json_resp[["resultSets"]][["headers"]][[1]]
passes_R[c("FGA")] <- sapply(passes_R[c("POTENTIAL_AST")], as.numeric)
passes_R <- passes_R %>%
  dplyr::filter(PLAYER_NAME == "Nikola Jokic") %>%
  select(PLAYER_ID, POTENTIAL_AST)

res <- GET(url = url_1, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
passes_PO <- data.frame(json_resp$resultSets$rowSet)
colnames(passes_PO) <- json_resp[["resultSets"]][["headers"]][[1]] 
passes_PO[c("FGA")] <- sapply(passes_PO[c("POTENTIAL_AST")], as.numeric)
passes_PO <- passes_PO %>%
  dplyr::filter(PLAYER_NAME == "Nikola Jokic") %>%
  select(PLAYER_ID, POTENTIAL_AST)

res <- GET(url = url_2, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
shots_R <- data.frame(json_resp[["resultSets"]][["rowSet"]][[7]])
colnames(shots_R) <- json_resp[["resultSets"]][["headers"]][[7]] 
shots_R[c("FGA")] <- sapply(shots_R[c("FGA")], as.numeric)
SCS_R <- sum(shots_R$FGA[2:3])

res <- GET(url = url_3, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
shots_PO <- data.frame(json_resp[["resultSets"]][["rowSet"]][[7]])
colnames(shots_PO) <- json_resp[["resultSets"]][["headers"]][[7]] 
shots_PO[c("FGA")] <- sapply(shots_PO[c("FGA")], as.numeric)
SCS_PO <- sum(shots_PO$FGA[2:3])

Season <- c("Regular", "Regular", "Playoffs", "Playoffs")
Type <- c("Passes", "Shots", "Passes", "Shots")
Shots_Created <- c(passes_R$POTENTIAL_AST, SCS_R, passes_PO$POTENTIAL_AST, SCS_PO)
data <- data.frame(Season, Type, Shots_Created)
data[c("Shots_Created")] <- sapply(data[c("Shots_Created")], as.numeric)

ggplot(data, aes(fill=Season, y=Shots_Created, x=Type)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("#0E2240", "#FEC524")) +
  theme_owen() +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 10),
        plot.subtitle = element_text(size = 8),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) + 
  labs(x = "Play Type", 
       y = "Shots Created", 
       title = "Nikola Jokic Shots Created", 
       subtitle = paste0("Passes are potential assists | Shots are 3+ seconds of touch time\nUpdated ", format(Sys.Date(), "%B %d, %Y")))
