library(tidyverse)
library(extrafont)
library(jsonlite)
library(httr)
library(hablar)
library(janitor)
library(paletteer)
library(prismatic)
library(scales)
library(nbastatR)
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

All <- read.csv("MavsData.csv")

All <- All %>%
  select(ABB.x, PLAY_TYPE, POSS_PCT_R, POSS_PCT_PO, PPP_R, PPP_PO, PERCENTILE_R, PERCENTILE_PO)

All %>% 
  gt()  %>% 
  cols_label(PLAY_TYPE = "Play Type", 
             POSS_PCT_R = "Regular", 
             POSS_PCT_PO = "Playoffs", 
             PPP_R = "Regular", 
             PPP_PO = "Playoffs", 
             PERCENTILE_R = "Regular", 
             PERCENTILE_PO = "Playoffs") %>% 
  tab_header(
    title = md("Mavericks Offense"), 
    subtitle = paste0("2021 Regular Season vs. Playoffs | Updated ", format(Sys.Date(), format="%B %d, %Y"))
  )  %>% 
  cols_merge(
    columns = vars(PLAY_TYPE, ABB.x)
  ) %>% 
  text_transform(
    locations = cells_body(
      columns = vars(PLAY_TYPE)
    ),
    fn = function(x){
      name <- word(x, 1)
      team <- word(x, -1)
      glue::glue(
        "<div><span style='font-weight:bold;font-variant:small-caps;font-size:10px'>{name}</div>
           <div style='line-height:10px'><span style ='font-weight:bold;color:grey;font-size:7px'>{team}</div>"
      )
    }
  ) %>% 
  tab_spanner(
    label = "% of Possessions",
    columns = vars(POSS_PCT_R, POSS_PCT_PO)
  ) %>% 
  tab_spanner(
    label = "Points Per Possession",
    columns = vars(PPP_R, PPP_PO)
  ) %>% 
  tab_spanner(
    label = "Percentile",
    columns = vars(PERCENTILE_R, PERCENTILE_PO)
  )  %>%
  fmt_percent(
    columns = vars(POSS_PCT_R, POSS_PCT_PO, PERCENTILE_R, PERCENTILE_PO),
    decimals = 1
  )  %>%
  data_color(
    columns = c(POSS_PCT_R, POSS_PCT_PO),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(PPP_R, PPP_PO),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::yellow_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(PERCENTILE_R, PERCENTILE_PO),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "right",
    columns = vars(POSS_PCT_PO, PPP_PO, PERCENTILE_PO)
  ) %>%
  cols_width(vars(POSS_PCT_PO, PPP_PO, PERCENTILE_PO) ~ px(45),
             vars(POSS_PCT_PO, PPP_PO, PERCENTILE_PO) ~ px(30)) %>% 
  tab_style(
    style = list(
      cell_borders(
        side =  "top", 
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = PLAY_TYPE == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = PLAY_TYPE == "League Average")
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
gtsave("Mavericks PO Offense.png")