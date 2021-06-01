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
teams <- nbastatR::nba_teams()
teams <- teams %>% select(slugTeam, urlThumbnailTeam)
colnames(teams)[which(names(teams) == "slugTeam")] <- "team_abbreviation"

pbp_team_PO <- "https://api.pbpstats.com/get-totals/nba?Season=2020-21&SeasonType=Playoffs&StartType=All&Type=Team"
pbp_P_R <- "https://api.pbpstats.com/get-totals/nba?Season=2020-21&SeasonType=Regular%2BSeason&Type=Team"

json_data <- fromJSON(paste(readLines(pbp_team_PO), collapse=""))
pbp_team_PO <- json_data[["multi_row_table_data"]]
pbp_team_PO <- pbp_team_PO %>% 
  clean_names() %>%
  select(team_abbreviation, shot_quality_avg, efg_pct, points, off_poss) %>%
  mutate(shot_making = efg_pct - shot_quality_avg) %>%
  mutate(off_rtg = round(points / off_poss * 100, 1))
pbp_team_PO <- merge(teams, pbp_team_PO, by="team_abbreviation")
pbp_team_PO <- pbp_team_PO %>%
  select(urlThumbnailTeam ,team_abbreviation, shot_quality_avg, shot_making, efg_pct, off_rtg) %>%
  slice(1:15, 17)

json_data <- fromJSON(paste(readLines(pbp_P_R), collapse=""))
pbp_team_R <- json_data[["multi_row_table_data"]]
pbp_team_R <- pbp_team_R %>% 
  clean_names() %>%
  select(team_abbreviation, shot_quality_avg, efg_pct) %>%
  mutate(shot_making_R = efg_pct - shot_quality_avg) %>%
  select(team_abbreviation, shot_making_R)

pbp_team_PO <- merge(pbp_team_PO, pbp_team_R, by="team_abbreviation")
pbp_team_PO <- pbp_team_PO %>%
  mutate(diff = shot_making - shot_making_R) %>%
  select(urlThumbnailTeam ,team_abbreviation, shot_quality_avg, shot_making, efg_pct, diff, off_rtg)

pbp_team_PO %>%
  arrange(desc(diff)) %>%
  gt()  %>%
  cols_label(urlThumbnailTeam = "",
             team_abbreviation = "Team",
             shot_quality_avg = "Shot Quality",
             efg_pct = "eFG%",
             diff = "+/- Expected",
             off_rtg = "Off. Rtg.",
             shot_making = "Shot Making") %>%
  tab_header(
    title = md("Value of Shot Making"),
    subtitle = paste0("2021 Playoffs | Updated ", format(Sys.Date(), format="%B %d, %Y"))
  )  %>%
  text_transform(
    locations = cells_body(vars(urlThumbnailTeam)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  tab_spanner(
    label = "Offensive Shot Making",
    columns = vars(shot_quality_avg, efg_pct, shot_making, diff, off_rtg)
  ) %>%
  fmt_percent(
    columns = vars(shot_quality_avg, shot_making, diff, efg_pct),
    decimals = 1
  )  %>%
  data_color(
    columns = c(shot_quality_avg),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(efg_pct),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(shot_making),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(diff),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(off_rtg),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "center",
    columns = vars(shot_quality_avg, efg_pct, shot_making, diff, off_rtg)
  ) %>%
  cols_width(vars(shot_quality_avg, efg_pct, shot_making, diff, off_rtg) ~ px(45),
             vars(shot_quality_avg, efg_pct, shot_making, diff, off_rtg) ~ px(30)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = team_abbreviation == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = team_abbreviation == "League Average")
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
  gtsave("NBA Shot Making.png")