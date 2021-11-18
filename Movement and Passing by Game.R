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

url <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=06%2F22%2F2021&DateTo=06%2F24%2F2021&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=SpeedDistance&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
url_1 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=06%2F24%2F2021&DateTo=06%2F26%2F2021&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=SpeedDistance&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
url_2 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=06%2F26%2F2021&DateTo=06%2F28%2F2021&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=SpeedDistance&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
url_3 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=06%2F28%2F2021&DateTo=06%2F30%2F2021&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=SpeedDistance&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
url_4 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=06%2F30%2F2021&DateTo=07%2F02%2F2021&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=SpeedDistance&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
#url_5 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=06%2F29%2F2021&DateTo=06%2F30%2F2021&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=SpeedDistance&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="


res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
G1 <- data.frame(json_resp$resultSets$rowSet)
colnames(G1) <- json_resp[["resultSets"]][["headers"]][[1]]
G1[c("DIST_MILES_OFF")] <- sapply(G1[c("DIST_MILES_OFF")], as.numeric)
G1 <- G1 %>%
  filter(TEAM_ABBREVIATION == "ATL") %>%
  mutate(Game = 1) %>%
  mutate(Diff = DIST_MILES_OFF - 9.24)

res <- GET(url = url_1, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
G2 <- data.frame(json_resp$resultSets$rowSet)
colnames(G2) <- json_resp[["resultSets"]][["headers"]][[1]]
G2[c("DIST_MILES_OFF")] <- sapply(G2[c("DIST_MILES_OFF")], as.numeric)
G2 <- G2 %>%
  filter(TEAM_ABBREVIATION == "ATL") %>%
  mutate(Game = 2) %>%
  mutate(Diff = DIST_MILES_OFF - 9.24)

res <- GET(url = url_2, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
G3 <- data.frame(json_resp$resultSets$rowSet)
colnames(G3) <- json_resp[["resultSets"]][["headers"]][[1]]
G3[c("DIST_MILES_OFF")] <- sapply(G3[c("DIST_MILES_OFF")], as.numeric)
G3 <- G3 %>%
  filter(TEAM_ABBREVIATION == "ATL") %>%
  mutate(Game = 3) %>%
  mutate(Diff = DIST_MILES_OFF - 9.24)

res <- GET(url = url_3, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
G4 <- data.frame(json_resp$resultSets$rowSet)
colnames(G4) <- json_resp[["resultSets"]][["headers"]][[1]]
G4[c("DIST_MILES_OFF")] <- sapply(G4[c("DIST_MILES_OFF")], as.numeric)
G4 <- G4 %>%
  filter(TEAM_ABBREVIATION == "ATL") %>%
  mutate(Game = 4) %>%
  mutate(Diff = DIST_MILES_OFF - 9.24)

res <- GET(url = url_4, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
G5 <- data.frame(json_resp$resultSets$rowSet)
colnames(G5) <- json_resp[["resultSets"]][["headers"]][[1]]
G5[c("DIST_MILES_OFF")] <- sapply(G5[c("DIST_MILES_OFF")], as.numeric)
G5 <- G5 %>%
  filter(TEAM_ABBREVIATION == "ATL") %>%
  mutate(Game = 5) %>%
  mutate(Diff = DIST_MILES_OFF - 9.24)

# res <- GET(url = url_5, add_headers(.headers=headers))
# json_resp <- fromJSON(content(res, "text"))
# G6 <- data.frame(json_resp$resultSets$rowSet)
# colnames(G6) <- json_resp[["resultSets"]][["headers"]][[1]]
# G6[c("DIST_MILES_OFF")] <- sapply(G6[c("DIST_MILES_OFF")], as.numeric)
# G6 <- G6 %>%
#   filter(TEAM_ABBREVIATION == "ATL") %>%
#   mutate(Game = 6) %>%
#   mutate(Diff = DIST_MILES_OFF - 9.24)


data <- rbind(G1, G2, G3, G4, G5)

plot1 <- ggplot(data = data, aes(x = Game, y = Diff, fill=Diff)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low="#00471B", mid="#2C5234", high="#EEE1C6") +
  scale_x_continuous(breaks = seq(1, 5, by = 1)) +
  theme_owen() +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        legend.position = "none",
        plot.title = element_text(face =  'bold', size = 15),
        plot.subtitle = element_text(size = 11),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
  labs(x = "Game #",
       y = "Off. Dist. Traveled +/- Avg.",
       title = "Atlanta Hawks Movement")

url <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=06%2F22%2F2021&DateTo=06%2F24%2F2021&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=Passing&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
url_1 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=06%2F24%2F2021&DateTo=06%2F26%2F2021&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=Passing&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
url_2 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=06%2F26%2F2021&DateTo=06%2F28%2F2021&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=Passing&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
url_3 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=06%2F28%2F2021&DateTo=06%2F30%2F2021&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=Passing&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
url_4 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=06%2F30%2F2021&DateTo=07%2F02%2F2021&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=Passing&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
#url_5 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=06%2F27%2F2021&DateTo=06%2F30%2F2021&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=Passing&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="


res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
G1 <- data.frame(json_resp$resultSets$rowSet)
colnames(G1) <- json_resp[["resultSets"]][["headers"]][[1]]
G1[c("POTENTIAL_AST")] <- sapply(G1[c("POTENTIAL_AST")], as.numeric)
G1 <- G1 %>%
  filter(TEAM_ABBREVIATION == "ATL") %>%
  mutate(Game = 1) %>%
  mutate(Diff = POTENTIAL_AST - 41.3)

res <- GET(url = url_1, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
G2 <- data.frame(json_resp$resultSets$rowSet)
colnames(G2) <- json_resp[["resultSets"]][["headers"]][[1]]
G2[c("POTENTIAL_AST")] <- sapply(G2[c("POTENTIAL_AST")], as.numeric)
G2 <- G2 %>%
  filter(TEAM_ABBREVIATION == "ATL") %>%
  mutate(Game = 2) %>%
  mutate(Diff = POTENTIAL_AST - 41.3)

res <- GET(url = url_2, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
G3 <- data.frame(json_resp$resultSets$rowSet)
colnames(G3) <- json_resp[["resultSets"]][["headers"]][[1]]
G3[c("POTENTIAL_AST")] <- sapply(G3[c("POTENTIAL_AST")], as.numeric)
G3 <- G3 %>%
  filter(TEAM_ABBREVIATION == "ATL") %>%
  mutate(Game = 3) %>%
  mutate(Diff = POTENTIAL_AST - 41.3)

res <- GET(url = url_3, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
G4 <- data.frame(json_resp$resultSets$rowSet)
colnames(G4) <- json_resp[["resultSets"]][["headers"]][[1]]
G4[c("POTENTIAL_AST")] <- sapply(G4[c("POTENTIAL_AST")], as.numeric)
G4 <- G4 %>%
  filter(TEAM_ABBREVIATION == "ATL") %>%
  mutate(Game = 4) %>%
  mutate(Diff = POTENTIAL_AST - 41.3)

res <- GET(url = url_4, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
G5 <- data.frame(json_resp$resultSets$rowSet)
colnames(G5) <- json_resp[["resultSets"]][["headers"]][[1]]
G5[c("POTENTIAL_AST")] <- sapply(G5[c("POTENTIAL_AST")], as.numeric)
G5 <- G5 %>%
  filter(TEAM_ABBREVIATION == "ATL") %>%
  mutate(Game = 5) %>%
  mutate(Diff = POTENTIAL_AST - 41.3)

# res <- GET(url = url_5, add_headers(.headers=headers))
# json_resp <- fromJSON(content(res, "text"))
# G6 <- data.frame(json_resp$resultSets$rowSet)
# colnames(G6) <- json_resp[["resultSets"]][["headers"]][[1]]
# G6[c("PASSES_MADE")] <- sapply(G6[c("PASSES_MADE")], as.numeric)
# G6 <- G6 %>%
#   filter(TEAM_ABBREVIATION == "PHX") %>%
#   mutate(Game = 6) %>%
#   mutate(Diff = PASSES_MADE - 41.3)

data <- rbind(G1, G2, G3, G4, G5)

plot2 <- ggplot(data = data, aes(x = Game, y = Diff, fill=Diff)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low="#00471B", mid="#2C5234", high="#EEE1C6") +
  scale_x_continuous(breaks = seq(1, 5, by = 1)) +
  theme_owen() +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        legend.position = "none",
        plot.title = element_text(face =  'bold', size = 15),
        plot.subtitle = element_text(size = 11),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
  labs(x = "Game #",
       y = "Potential Assists +/- Avg.",
       title = "Atlanta Hawks Passing")


ggarrange(plot1, plot2,
          ncol = 1, nrow = 2)