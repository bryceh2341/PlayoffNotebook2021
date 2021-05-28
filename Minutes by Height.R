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

url <- "https://stats.nba.com/stats/playerindex?College=&Country=&DraftPick=&DraftRound=&DraftYear=&Height=&Historical=1&LeagueID=00&Season=2020-21&SeasonType=Regular%20Season&TeamID=0&Weight="
url_1 <- "https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight="
url_2 <- "https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
players <- data.frame(json_resp$resultSets$rowSet)
colnames(players) <- json_resp[["resultSets"]][["headers"]][[1]]
players <- select(players, "PERSON_ID", "HEIGHT", "WEIGHT")
colnames(players)[which(names(players) == "PERSON_ID")] <- "PLAYER_ID"
players <- players %>% 
  separate(HEIGHT, c('feet', 'inches'), "-", convert = TRUE) %>% 
  mutate(HEIGHT = (12*feet + inches))

res <- GET(url = url_1, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Playoff_Gen <- data.frame(json_resp$resultSets$rowSet)
colnames(Playoff_Gen) <- json_resp[["resultSets"]][["headers"]][[1]]
Playoff_Gen <- select(Playoff_Gen, "PLAYER_ID", "PLAYER_NAME", "GP", "MIN")
Playoff_Gen[c("GP", "MIN")] <- sapply(Playoff_Gen[c("GP", "MIN")], as.numeric)
Playoff_Gen <- Playoff_Gen %>%
  mutate(Total_Min_PO = MIN * GP)

res <- GET(url = url_2, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Reg_Gen <- data.frame(json_resp$resultSets$rowSet)
colnames(Reg_Gen) <- json_resp[["resultSets"]][["headers"]][[1]]
Reg_Gen <- select(Reg_Gen, "PLAYER_ID", "GP", "MIN")
Reg_Gen[c("GP", "MIN")] <- sapply(Reg_Gen[c("GP", "MIN")], as.numeric)
Reg_Gen <- Reg_Gen %>%
  mutate(Total_Min_Reg = MIN * GP)

combined_PO <- merge(players, Playoff_Gen, by="PLAYER_ID")
combined_PO <- combined_PO %>%
  select(HEIGHT, Total_Min_PO) %>%
  group_by(HEIGHT) %>%
  summarise(Total_Min_PO = sum(Total_Min_PO)) %>%
  mutate(MinPer = Total_Min_PO / sum(Total_Min_PO))
combined_Reg <- merge(players, Reg_Gen, by="PLAYER_ID")
combined_Reg <- combined_Reg %>%
  select(HEIGHT, Total_Min_Reg) %>%
  group_by(HEIGHT) %>%
  summarise(Total_Min_Reg = sum(Total_Min_Reg)) %>%
  mutate(MinPer = Total_Min_Reg / sum(Total_Min_Reg))

# barplot(combined_PO$Total_Min_PO, combined_PO$HEIGHT)
# barplot(combined_Reg$Total_Min_Reg, combined_Reg$HEIGHT)

ggplot() +
  geom_line(data = combined_PO, aes(x=HEIGHT, y=MinPer), color = "red", size=3) +
  geom_line(data = combined_Reg, aes(x=HEIGHT, y=MinPer), color = "blue", size=3) +
  theme_owen() +
  theme(legend.position="top") +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 10),
        plot.subtitle = element_text(size = 8),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) + 
  labs(x = "Height (inches)", 
       y = "% of Minutes Played", 
       title = "Minutes Played by Height", 
       subtitle = paste0("2020-21 Season\nRgular Season in Blue | Playoffs in Red\nUpdated ", format(Sys.Date(), "%B %d, %Y")))
  