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

url <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=10%2F20%2F2021&DateTo=10%2F22%2F2021&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Possessions&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
url_1 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=10%2F22%2F2021&DateTo=10%2F24%2F2021&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Possessions&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
url_2 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Possessions&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="


# res <- GET(url = url, add_headers(.headers=headers))
# json_resp <- fromJSON(content(res, "text"))
# game1 <- data.frame(json_resp$resultSets$rowSet)
# colnames(game1) <- json_resp[["resultSets"]][["headers"]][[1]]
# game1[c("TOUCHES", "AVG_SEC_PER_TOUCH", "MIN")] <- sapply(game1[c("TOUCHES", "AVG_SEC_PER_TOUCH", "MIN")], as.numeric)
# game1 <- game1 %>%
#   filter(PLAYER_NAME == "Luka Doncic") %>%
#   mutate(TimePerMin = TOUCHES*AVG_SEC_PER_TOUCH/MIN)
# 
# res <- GET(url = url_1, add_headers(.headers=headers))
# json_resp <- fromJSON(content(res, "text"))
# game2 <- data.frame(json_resp$resultSets$rowSet)
# colnames(game2) <- json_resp[["resultSets"]][["headers"]][[1]]
# game2[c("TOUCHES", "AVG_SEC_PER_TOUCH", "MIN")] <- sapply(game2[c("TOUCHES", "AVG_SEC_PER_TOUCH", "MIN")], as.numeric)
# game2 <- game2 %>%
#   filter(PLAYER_NAME == "Luka Doncic") %>%
#   mutate(TimePerMin = TOUCHES*AVG_SEC_PER_TOUCH/MIN)

res <- GET(url = url_2, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Last <- data.frame(json_resp$resultSets$rowSet)
colnames(Last) <- json_resp[["resultSets"]][["headers"]][[1]]
Last[c("TOUCHES", "AVG_SEC_PER_TOUCH", "MIN")] <- sapply(Last[c("TOUCHES", "AVG_SEC_PER_TOUCH", "MIN")], as.numeric)
Last <- Last %>%
  #filter(PLAYER_NAME == "Luka Doncic") %>%
  mutate(TimePerMin = TOUCHES*AVG_SEC_PER_TOUCH/MIN) %>%
  arrange(TimePerMin)

# df = data.frame(Section = c(1, 2, 3), TTPM = c(15.6, 14.8, 15.8))
# 
# ggplot(data = df, aes(x = factor(Section), y = TTPM, fill=TTPM)) +
#   geom_bar(stat = "identity") +
#   geom_text(aes(label=TTPM), vjust=1.6, color="white", size=5.5)+
#   scale_fill_gradient2(low="#ED174C", mid="#006BB6", high="#002B5C") +
#   theme_owen() +
#   scale_x_discrete(labels=c("Last Season", "Game 1", "Game 2")) +
#   scale_y_continuous(breaks=(seq(12,18,1)), limits = c(12, 18), oob = rescale_none) +
#   theme(legend.position="none") +
#   theme(strip.text.x = element_blank(),
#         panel.spacing.x = unit(1, "lines"),
#         plot.title.position = 'plot',
#         plot.title = element_text(face =  'bold', size = 20),
#         plot.subtitle = element_text(size = 12),
#         plot.margin = unit(c(.5, .5, 1, .5), "lines")) + 
#   labs(x = "", 
#        y = "Touch Time per Minute", 
#        title = "Luka Needs the Ball", 
#        subtitle = "Data per nba.com")
