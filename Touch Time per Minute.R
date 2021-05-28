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

url <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Possessions&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
url_1 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Possessions&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
url_2 <- "https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight="
url_3 <- "https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
playoff_touches <- data.frame(json_resp$resultSets$rowSet)
colnames(playoff_touches) <- json_resp[["resultSets"]][["headers"]][[1]]
playoff_touches <- select(playoff_touches, PLAYER_NAME, TOUCHES, TIME_OF_POSS)

res <- GET(url = url_1, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
reg_touches <- data.frame(json_resp$resultSets$rowSet)
colnames(reg_touches) <- json_resp[["resultSets"]][["headers"]][[1]] 
reg_touches <- select(reg_touches, PLAYER_NAME, TOUCHES, TIME_OF_POSS)

res <- GET(url = url_2, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
playoff_gen <- data.frame(json_resp$resultSets$rowSet)
colnames(playoff_gen) <- json_resp[["resultSets"]][["headers"]][[1]] 
playoff_gen <- select(playoff_gen, PLAYER_NAME, MIN)

res <- GET(url = url_3, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
reg_gen <- data.frame(json_resp$resultSets$rowSet)
colnames(reg_gen) <- json_resp[["resultSets"]][["headers"]][[1]] 
reg_gen <- select(reg_gen, PLAYER_NAME, MIN)

combined <- merge(playoff_touches, reg_touches, by="PLAYER_NAME")
combined <- merge(combined, playoff_gen, by="PLAYER_NAME")
combined <- merge(combined, reg_gen, by="PLAYER_NAME")
combined$TOUCHES.x <- as.numeric(combined$TOUCHES.x)
combined$TIME_OF_POSS.x <- as.numeric(combined$TIME_OF_POSS.x)
combined$TOUCHES.y <- as.numeric(combined$TOUCHES.y)
combined$TIME_OF_POSS.y <- as.numeric(combined$TIME_OF_POSS.y)
combined$MIN.x <- as.numeric(combined$MIN.x)
combined$MIN.y <- as.numeric(combined$MIN.y)
combined <- combined %>%
  filter(rank(desc(as.numeric(TOUCHES.x)))<=31) %>%
  mutate(DIFF = TOUCHES.x - TOUCHES.y) %>%
  mutate(REG_TIME = TOUCHES.y * TIME_OF_POSS.y) %>%
  mutate(POST_TIME = TOUCHES.x * TIME_OF_POSS.x) %>%
  mutate(REG_TPM = REG_TIME / MIN.y) %>%
  mutate(PLAYOFF_TPM = POST_TIME / MIN.x) %>%
  mutate(TPM_DIFF = PLAYOFF_TPM - REG_TPM) %>%
  mutate(pos_neg = case_when(
    TPM_DIFF >= 0 ~ "Better On", 
    TRUE ~ "Worse on",))


plot(combined$TOUCHES.x, combined$TOUCHES.y)

comet_plot <- combined %>%
  ggplot() +
  geom_link(aes(x = REG_TPM, y = fct_reorder(PLAYER_NAME, TPM_DIFF), xend = PLAYOFF_TPM, yend = fct_reorder(PLAYER_NAME, TPM_DIFF), color = pos_neg, size = stat(index))) +
  scale_color_manual(values = c("#00A087FF", "#E64B35FF")) +
  scale_size(range = c(.01, 4)) +
  scale_x_continuous(labels = c("5", "10", "15", "20", "25"), breaks = seq(5, 25, 5)) +
  theme_owen() +
  geom_point(
    data = filter(combined, TPM_DIFF > 0),
    aes(PLAYOFF_TPM, y = fct_reorder(PLAYER_NAME, TPM_DIFF), color = pos_neg),
    shape = 21,
    fill = "white",
    size = 3.5
  )  +
  geom_point(
    data = filter(combined, TPM_DIFF < 0),
    aes(PLAYOFF_TPM, y = fct_reorder(PLAYER_NAME, TPM_DIFF), color = pos_neg),
    shape = 21,
    fill = "white",
    size = 3.5
  ) +
  annotate(geom = 'label', x = 21.5, y = 7.5, label = "Player is getting\nless time", family = "Consolas", color = "#E64B35FF", fontface = 'bold', fill = "floralwhite", label.size = 0, size = 3) +
  annotate(geom = 'label', x = 5.5, y = 22, label = "Player is getting\nmore time", family = "Consolas", color = "#00A087FF", fontface = 'bold', fill = "floralwhite", label.size = 0, size = 3) +
  theme(legend.position = 'none',
        plot.title.position = 'plot',
        axis.text.y = element_text(size = 6),
        plot.title = element_text(face = 'bold', size = 15),
        plot.subtitle = element_text(size = 7),
        plot.margin = margin(10, 10, 20, 10)) +
  labs(x = "Seconds of Possession Per Minute",
       y = "",
       title = "Give Me the Ball!",
       subtitle = "Time Spent w/ the Ball per Minute for Post Season vs. Regular Season | Players Sorted by Differential")
# Save Plot
ggsave("Comet_Plot.png", comet_plot, w = 6, h = 6, dpi = 300, type = "cairo")

  