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

url <- "https://stats.nba.com/stats/teamdashptshots?DateFrom=05%2F21%2F2021&DateTo=05%2F28%2F2021&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&TeamID=1610612756&VsConference=&VsDivision="
url_1 <- "https://stats.nba.com/stats/teamdashptshots?DateFrom=&DateTo=&GameSegment=&LastNGames=1&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&TeamID=1610612756&VsConference=&VsDivision="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Suns3 <- data.frame(json_resp[["resultSets"]][["rowSet"]][[2]])
colnames(Suns3) <- json_resp[["resultSets"]][["headers"]][[2]]
Suns3[c("FGA_FREQUENCY", "EFG_PCT")] <- sapply(Suns3[c("FGA_FREQUENCY", "EFG_PCT")], as.numeric)
Suns3 <- Suns3 %>%
  select(SHOT_CLOCK_RANGE, FGA_FREQUENCY, EFG_PCT)

res <- GET(url = url_1, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Suns1 <- data.frame(json_resp[["resultSets"]][["rowSet"]][[2]])
colnames(Suns1) <- json_resp[["resultSets"]][["headers"]][[2]]
Suns1[c("FGA_FREQUENCY", "EFG_PCT")] <- sapply(Suns1[c("FGA_FREQUENCY", "EFG_PCT")], as.numeric)
Suns1 <- Suns1 %>%
  select(SHOT_CLOCK_RANGE, FGA_FREQUENCY, EFG_PCT)

combined <- merge(Suns3, Suns1, by="SHOT_CLOCK_RANGE")
target <- c("24-22", "22-18 Very Early", "18-15 Early", "15-7 Average", "7-4 Late", "4-0 Very Late")
combined <- combined[match(target, combined$SHOT_CLOCK_RANGE),]

combined %>%
  gt()  %>%
  cols_label(SHOT_CLOCK_RANGE = "Shot Clock Range",
             FGA_FREQUENCY.x = "Games 1-3",
             FGA_FREQUENCY.y = "Game 4",
             EFG_PCT.x = "Games 1-3",
             EFG_PCT.y = "Game 4"
  ) %>%
  tab_header(
    title = md("Suns Offense"),
    subtitle = paste0("Shot Attempts for Games 1-3 vs. Game 4 | Updated ", format(Sys.Date(), format="%B %d, %Y"))
  )  %>%
  tab_spanner(
    label = "Frequency",
    columns = vars(FGA_FREQUENCY.x, FGA_FREQUENCY.y)
  ) %>%
  tab_spanner(
    label = "eFG%",
    columns = vars(EFG_PCT.x, EFG_PCT.y)
  ) %>%
  fmt_percent(
    columns = vars(FGA_FREQUENCY.x, FGA_FREQUENCY.y, EFG_PCT.x, EFG_PCT.y),
    decimals = 1
  )  %>%
  data_color(
    columns = c(FGA_FREQUENCY.x,FGA_FREQUENCY.y),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(EFG_PCT.x, EFG_PCT.y),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::orange_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "right",
    columns = vars(FGA_FREQUENCY.y, EFG_PCT.y)
  ) %>%
  cols_width(vars(FGA_FREQUENCY.x, FGA_FREQUENCY.y, EFG_PCT.x, EFG_PCT.y) ~ px(45),
             vars(FGA_FREQUENCY.x, FGA_FREQUENCY.y, EFG_PCT.x, EFG_PCT.y) ~ px(30)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = SHOT_CLOCK_RANGE == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = SHOT_CLOCK_RANGE == "League Average")
  ) %>%
  tab_options(
    table.background.color = "floralwhite",
    column_labels.font.size = 10.5,
    table.font.size = 10,
    heading.title.font.size  = 24,
    heading.title.font.weight = 'bold',
    heading.subtitle.font.size = 11,
    table.font.names = "Consolas",
    table.font.color = 'black',
    table.border.top.color = "transparent",
    data_row.padding = px(2),
    footnotes.font.size = 8,
    source_notes.font.size = 9,
    footnotes.padding = px(1),
  ) %>%
  gtsave("Suns Time Shots.png")