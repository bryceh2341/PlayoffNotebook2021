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

url <- "https://stats.nba.com/stats/playerdashboardbyshootingsplits?DateFrom=&DateTo=&GameSegment=&LastNGames=1&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerID=1629630&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&ShotClockRange=&VsConference=&VsDivision="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
player_1 <- data.frame(json_resp[["resultSets"]][["rowSet"]][[2]])
colnames(player_1) <- json_resp[["resultSets"]][["headers"]][[1]]
player_1[c("FGM", "FGA", "EFG_PCT", "PCT_UAST_FGM")] <- sapply(player_1[c("FGM", "FGA", "EFG_PCT", "PCT_UAST_FGM")], as.numeric)
player_1 <- player_1 %>%
  mutate(perc = FGA / sum(FGA)) %>%
  select(GROUP_VALUE, perc, EFG_PCT, PCT_UAST_FGM) %>%
  slice(1:6)

url_1 <- "https://stats.nba.com/stats/playerdashboardbyshootingsplits?DateFrom=05%2F22%2F2021&DateTo=05%2F30%2F2021&GameSegment=&LastNGames=4&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerID=1629630&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&ShotClockRange=&VsConference=&VsDivision="

res <- GET(url = url_1, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
player_3 <- data.frame(json_resp[["resultSets"]][["rowSet"]][[2]])
colnames(player_3) <- json_resp[["resultSets"]][["headers"]][[1]]
player_3[c("FGM", "FGA", "EFG_PCT", "PCT_UAST_FGM")] <- sapply(player_3[c("FGM", "FGA", "EFG_PCT", "PCT_UAST_FGM")], as.numeric)
player_3 <- player_3 %>%
  mutate(perc = FGA / sum(FGA)) %>%
  select(GROUP_VALUE, perc, EFG_PCT, PCT_UAST_FGM) %>%
  slice(1:6)

All <- merge(player_3, player_1, by="GROUP_VALUE")
target <- c("Less Than 5 ft.", "5-9 ft.", "10-14 ft.", "15-19 ft.", "20-24 ft.", "25-29 ft.")
All <- All[match(target, All$GROUP_VALUE),]

All %>%
  gt()  %>%
  cols_label(GROUP_VALUE = "Shot Location",
             perc.x = "Games 1-3",
             perc.y = "Game 4",
             EFG_PCT.x = "Games 1-3",
             EFG_PCT.y = "Game 4",
             PCT_UAST_FGM.x = "Games 1-3",
             PCT_UAST_FGM.y = "Game 4"
  ) %>%
  tab_header(
    title = md("Ja Morant Shot Location"),
    subtitle = paste0("2021 Playoffs | Updated ", format(Sys.Date(), format="%B %d, %Y"))
  )  %>%
  tab_spanner(
    label = "% of Shots",
    columns = vars(perc.x, perc.y)
  ) %>%
  tab_spanner(
    label = "eFG%",
    columns = vars(EFG_PCT.x, EFG_PCT.y)
  ) %>%
  tab_spanner(
    label = "% of Unassisted FGM",
    columns = vars(PCT_UAST_FGM.x, PCT_UAST_FGM.y)
  ) %>%
  fmt_percent(
    columns = vars(perc.x, EFG_PCT.x, PCT_UAST_FGM.x, perc.y, EFG_PCT.y, PCT_UAST_FGM.y),
    decimals = 1
  )  %>%
  data_color(
    columns = c(perc.x, perc.y),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::light_blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(EFG_PCT.x, EFG_PCT.y),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(PCT_UAST_FGM.x, PCT_UAST_FGM.y),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::indigo_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "right",
    columns = vars(perc.y, EFG_PCT.y, PCT_UAST_FGM.y)
  ) %>%
  cols_width(vars(perc.y, EFG_PCT.y, PCT_UAST_FGM.y) ~ px(45),
             vars(perc.y, EFG_PCT.y, PCT_UAST_FGM.y) ~ px(30)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = GROUP_VALUE == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = GROUP_VALUE == "League Average")
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
  gtsave("Morant Shots.png")