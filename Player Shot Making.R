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

theme_owen <- function () { 
  theme_minimal(base_size=12, base_family="Consolas") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

#get nba playerIds and headshots
players <- nbastatR::nba_players()
players <- players %>% select(idPlayer, urlPlayerHeadshot)
colnames(players)[which(names(players) == "idPlayer")] <- "RowId"

pbp_P_PO <- "https://api.pbpstats.com/get-totals/nba?Season=2020-21&SeasonType=Playoffs&StartType=All&Type=Player"
pbp_P_R <- "https://api.pbpstats.com/get-totals/nba?Season=2020-21&SeasonType=Regular%2BSeason&Type=Player"

json_data <- fromJSON(paste(readLines(pbp_P_PO), collapse=""))
pbp_P_PO <- json_data[["multi_row_table_data"]]
pbp_P_PO <- pbp_P_PO %>%
  mutate(ShotMaking = EfgPct - ShotQualityAvg) %>%
  mutate(FGAG = (FG2A+FG3A) / GamesPlayed) %>%
  select(Name, ShotQualityAvg, EfgPct, ShotMaking, FGAG, RowId)

json_data <- fromJSON(paste(readLines(pbp_P_R), collapse=""))
pbp_P_R <- json_data[["multi_row_table_data"]]
pbp_P_R <- pbp_P_R %>%
  select(Name, ShotQualityAvg, EfgPct, RowId) %>%
  mutate(ShotMakingR = EfgPct - ShotQualityAvg) %>%
  select(RowId, ShotMakingR)

Combined <- merge(pbp_P_PO, pbp_P_R, by="RowId")
Combined <- merge(Combined, players, by="RowId")
Combined <- Combined %>%
  mutate(SMDiff = ShotMaking - ShotMakingR) %>%
  arrange(desc(FGAG)) %>%
  select(urlPlayerHeadshot, Name, ShotQualityAvg, EfgPct, ShotMaking, SMDiff)

Combined %>% 
  slice(1:50) %>%
  arrange(desc(SMDiff)) %>%
  gt()  %>% 
  cols_label(urlPlayerHeadshot = "",
             Name = "Player", 
             ShotQualityAvg = "Shot Quality", 
             EfgPct = "eFG%", 
             ShotMaking = "Shot Making", 
             SMDiff = "+/- Expected") %>% 
  tab_header(
    title = md("Value of Shot Making"), 
    subtitle = paste0("2021 Playoffs | Updated ", format(Sys.Date(), format="%B %d, %Y"))
  )  %>% 
  text_transform(
    locations = cells_body(vars(urlPlayerHeadshot)),
    fn = function(x) {
      web_image(url = x, 
                height = px(22.5)) 
    }
  ) %>%
  tab_spanner(
    label = "Player Shot Making",
    columns = vars(ShotQualityAvg, EfgPct, ShotMaking, SMDiff)
  ) %>% 
  fmt_percent(
    columns = vars(ShotQualityAvg, EfgPct, ShotMaking, SMDiff),
    decimals = 1
  )  %>%
  data_color(
    columns = c(ShotQualityAvg),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(EfgPct),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(ShotMaking),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(SMDiff),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "center",
    columns = vars(ShotQualityAvg, EfgPct, ShotMaking, SMDiff)
  ) %>%
  cols_width(vars(ShotQualityAvg, EfgPct, ShotMaking, SMDiff) ~ px(45),
             vars(ShotQualityAvg, EfgPct, ShotMaking, SMDiff) ~ px(30)) %>% 
  tab_style(
    style = list(
      cell_borders(
        side =  "top", 
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = Name == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = Name == "League Average")
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
  gtsave("Player Shot Making.png")