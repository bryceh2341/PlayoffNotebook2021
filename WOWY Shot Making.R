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

url <- "https://api.pbpstats.com/get-game-stats?Type=LineupOpponent&GameId=0042000401"

json_data <- fromJSON(paste(readLines(url), collapse=""))
lineup <- json_data[["stats"]][["Away"]][["FullGame"]]
lineup[is.na(lineup)] <- 0
lineup <- lineup %>%
  clean_names() %>%
  separate(entity_id, c("Player1", "Player2", "Player3", "Player4", "Player5"), "-") %>%
  filter(Player1 == "201572" | Player2 == "201572" | Player3 == "201572" | Player4 == "201572" | Player5 == "201572") %>%
  mutate(shotm = (efg_pct - shot_quality_avg) * ((fg2a + fg3a) / (sum(fg2a) + sum(fg3a))))

json_data <- fromJSON(paste(readLines(url), collapse=""))
lineup1 <- json_data[["stats"]][["Away"]][["FullGame"]]
lineup1[is.na(lineup1)] <- 0
lineup1 <- lineup1 %>%
  clean_names() %>%
  separate(entity_id, c("Player1", "Player2", "Player3", "Player4", "Player5"), "-") %>%
  filter(Player1 != "201572") %>%
  filter(Player2 != "201572") %>%
  filter(Player3 != "201572") %>%
  filter(Player4 != "201572") %>%
  filter(Player5 != "201572") %>%
  mutate(shotm = (efg_pct - shot_quality_avg) * ((fg2a + fg3a) / (sum(fg2a) + sum(fg3a))))



drtg <- round(sum(lineup$points) / sum(lineup$def_poss) * 100, 2)
shot_making <- sum(lineup$shotm)
exppts <- (sum(lineup$shotm) - .0179) * (sum(lineup$fg2a) + sum(lineup$fg3a))
expdrtg <- round((sum(lineup$points) - exppts) / sum(lineup$def_poss) * 100, 2)
drtg1 <- round(sum(lineup1$points) / sum(lineup1$def_poss) * 100, 2)
shot_making1 <- sum(lineup1$shotm)
exppts1 <- (sum(lineup1$shotm) - .0179) * (sum(lineup1$fg2a) + sum(lineup1$fg3a))
expdrtg1 <- round((sum(lineup1$points) - exppts1) / sum(lineup1$def_poss) * 100, 2)

player <- c("With Lopez", "Without Lopez")
def_rtg <- c(drtg, drtg1)
shotmaking <- c(shot_making, shot_making1)
exp_def_rtg <- c(expdrtg, expdrtg1)

data <- data.frame(player, def_rtg, shotmaking, exp_def_rtg)

data %>%
  gt()  %>%
  cols_label(player = "",
             def_rtg = "DRtg.",
             shotmaking = "Shot Making",
             exp_def_rtg = "Expected DRtg."
  ) %>%
  tab_header(
    title = md("The Bucks and Brook Lopez"),
    subtitle = "NBA Finals Game 1"
  )  %>%
  fmt_percent(
    columns = vars(shotmaking),
    decimals = 1
  )  %>%
  data_color(
    columns = c(def_rtg, exp_def_rtg),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "center",
    columns = vars(def_rtg, shotmaking, exp_def_rtg)
  ) %>%
  cols_width(vars(def_rtg, shotmaking, exp_def_rtg) ~ px(50),
             vars(def_rtg, shotmaking, exp_def_rtg) ~ px(50)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = player == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = player == "League Average")
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
  gtsave("Bucks Lopez.png")