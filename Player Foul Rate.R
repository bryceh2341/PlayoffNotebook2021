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
pbp_P_PO[is.na(pbp_P_PO)] = 0
pbp_P_PO <- pbp_P_PO %>%
  select(Name, RowId, TwoPtShootingFoulsDrawnPct, ThreePtShootingFoulsDrawnPct, FTA)

json_data <- fromJSON(paste(readLines(pbp_P_R), collapse=""))
pbp_P_R <- json_data[["multi_row_table_data"]]
pbp_P_R[is.na(pbp_P_R)] = 0
pbp_P_R <- pbp_P_R %>%
  select(RowId, TwoPtShootingFoulsDrawnPct, ThreePtShootingFoulsDrawnPct, FTA)

combined <- merge(pbp_P_R, pbp_P_PO, by="RowId")
combined <- merge(players, combined, by="RowId")
combined <- combined %>%
  arrange(desc(FTA.y)) %>%
  select(urlPlayerHeadshot, Name, TwoPtShootingFoulsDrawnPct.x, ThreePtShootingFoulsDrawnPct.x, TwoPtShootingFoulsDrawnPct.y, ThreePtShootingFoulsDrawnPct.y) %>%
  slice(1:15)

combined %>% 
  gt()  %>% 
  cols_label(urlPlayerHeadshot = "",
             Name = "Player", 
             TwoPtShootingFoulsDrawnPct.x = "Regular", 
             TwoPtShootingFoulsDrawnPct.y = "Playoffs", 
             ThreePtShootingFoulsDrawnPct.x = "Regular", 
             ThreePtShootingFoulsDrawnPct.y = "Playoffs") %>% 
  tab_header(
    title = md("Player Shooting Fouls"), 
    subtitle = paste0("2021 Regular Season & Playoffs | Updated ", format(Sys.Date(), format="%B %d, %Y"))
  )  %>% 
  text_transform(
    locations = cells_body(vars(urlPlayerHeadshot)),
    fn = function(x) {
      web_image(url = x, 
                height = px(22.5)) 
    }
  ) %>%
  tab_spanner(
    label = "2pt. Foul%",
    columns = vars(TwoPtShootingFoulsDrawnPct.x, TwoPtShootingFoulsDrawnPct.y)
  ) %>% 
  tab_spanner(
    label = "3pt. Foul%",
    columns = vars(ThreePtShootingFoulsDrawnPct.x, ThreePtShootingFoulsDrawnPct.y)
  ) %>% 
  fmt_percent(
    columns = vars(TwoPtShootingFoulsDrawnPct.x, ThreePtShootingFoulsDrawnPct.x, TwoPtShootingFoulsDrawnPct.y, ThreePtShootingFoulsDrawnPct.y),
    decimals = 1
  )  %>%
  data_color(
    columns = c(TwoPtShootingFoulsDrawnPct.x, TwoPtShootingFoulsDrawnPct.y),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(ThreePtShootingFoulsDrawnPct.x, ThreePtShootingFoulsDrawnPct.y),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "center",
    columns = vars(TwoPtShootingFoulsDrawnPct.x, ThreePtShootingFoulsDrawnPct.x, TwoPtShootingFoulsDrawnPct.y, ThreePtShootingFoulsDrawnPct.y)
  ) %>%
  cols_width(vars(TwoPtShootingFoulsDrawnPct.x, ThreePtShootingFoulsDrawnPct.x, TwoPtShootingFoulsDrawnPct.y, ThreePtShootingFoulsDrawnPct.y) ~ px(45),
             vars(TwoPtShootingFoulsDrawnPct.x, ThreePtShootingFoulsDrawnPct.x, TwoPtShootingFoulsDrawnPct.y, ThreePtShootingFoulsDrawnPct.y) ~ px(30)) %>% 
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
  gtsave("Player Fouls.png")