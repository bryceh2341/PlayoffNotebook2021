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

url <- "https://stats.nba.com/stats/leagueseasonmatchups?DateFrom=&DateTo=&LeagueID=00&OffPlayerID=202710&Outcome=&PORound=0&PerMode=Totals&Season=2020-21&SeasonType=Playoffs"

#get nba playerIds and headshots
players <- nbastatR::nba_players()
players <- players %>% select(idPlayer, urlPlayerHeadshot)

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Butler_Matchups <- data.frame(json_resp$resultSets$rowSet)
colnames(Butler_Matchups) <- json_resp[["resultSets"]][["headers"]][[1]]
Butler_Matchups$PLAYER_PTS <- as.numeric(Butler_Matchups$PLAYER_PTS)
Butler_Matchups[c("PLAYER_PTS", "PARTIAL_POSS", "MATCHUP_FGA", "MATCHUP_FG_PCT", "MATCHUP_FTA", "DEF_PLAYER_ID")] <- sapply(Butler_Matchups[c("PLAYER_PTS", "PARTIAL_POSS", "MATCHUP_FGA", "MATCHUP_FG_PCT", "MATCHUP_FTA", "DEF_PLAYER_ID")], as.numeric)
colnames(Butler_Matchups)[which(names(Butler_Matchups) == "DEF_PLAYER_ID")] <- "idPlayer"

Butler_Matchups <- Butler_Matchups %>%
  mutate(TS = PLAYER_PTS / (2*(MATCHUP_FGA + 0.44*MATCHUP_FTA))) %>%
  mutate(PPP = 100/PARTIAL_POSS * PLAYER_PTS) %>%
  mutate(TimePer = PARTIAL_POSS / sum(PARTIAL_POSS)) %>%
  mutate(ABB = "MIL")

Butler_Matchups$TS[is.nan(Butler_Matchups$TS)]<-0

Butler_Matchups <- merge(Butler_Matchups, players, by="idPlayer")

Butler_Matchups <- Butler_Matchups %>% select(urlPlayerHeadshot, ABB, DEF_PLAYER_NAME, MATCHUP_MIN, TimePer, PLAYER_PTS, MATCHUP_FG_PCT, PPP, TS)


Butler_Matchups %>% 
  arrange(desc(TimePer)) %>% 
  gt()  %>% 
  cols_label(urlPlayerHeadshot = "",
             DEF_PLAYER_NAME = "", 
             MATCHUP_MIN = "Time", 
             TimePer = "%", 
             PLAYER_PTS = "Pts.", 
             MATCHUP_FG_PCT = "FG%", 
             PPP = "Pts./100", 
             TS = "TS%") %>% 
  tab_header(
    title = md("Jimmy Butler Matchups"), 
    subtitle = paste0("2021 Playoffs | Updated ", format(Sys.Date(), format="%B %d, %Y"))
  )  %>% 
  text_transform(
    locations = cells_body(vars(urlPlayerHeadshot)),
    fn = function(x) {
      web_image(url = x, 
                height = px(22.5)) 
    }
  ) %>%
  cols_merge(
    columns = vars(DEF_PLAYER_NAME, ABB)
  ) %>% 
  text_transform(
    locations = cells_body(
      columns = vars(DEF_PLAYER_NAME)
    ),
    fn = function(x){
      name <- word(x, 1, 2)
      team <- word(x, -1)
      glue::glue(
        "<div><span style='font-weight:bold;font-variant:small-caps;font-size:10px'>{name}</div>
           <div style='line-height:10px'><span style ='font-weight:bold;color:grey;font-size:7px'>{team}</div>"
      )
    }
  ) %>% 
  tab_spanner(
    label = "Matchup",
    columns = vars(MATCHUP_MIN, TimePer)
  ) %>% 
  tab_spanner(
    label = "Scoring",
    columns = vars(PLAYER_PTS, MATCHUP_FG_PCT)
  ) %>% 
  tab_spanner(
    label = "Advanced",
    columns = vars(PPP, TS)
  ) %>%
  fmt_percent(
    columns = vars(TimePer, MATCHUP_FG_PCT, TS),
    decimals = 1
  )  %>%
  data_color(
    columns = vars(TimePer, MATCHUP_FG_PCT, TS),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn",
        direction  = 1
      ) %>% as.character(),
      domain = c(0, 1), 
      na.color = "#00441BFF"
    )
  ) %>%
  cols_align(
    align = "right",
    columns = vars(TimePer, MATCHUP_FG_PCT, TS)
  ) %>%
  cols_width(vars(TimePer, MATCHUP_FG_PCT, TS) ~ px(45),
             vars(TimePer, MATCHUP_FG_PCT, TS) ~ px(30)) %>% 
  tab_style(
    style = list(
      cell_borders(
        side =  "top", 
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = DEF_PLAYER_NAME == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = DEF_PLAYER_NAME == "League Average")
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
  # tab_footnote(
  #   footnote = "FGAs in last 5 minutes of close games via stats.nba.com",
  #   locations = cells_column_spanners(spanners = "<span style='font-weight:bold;font-size:12px'>NBA<br></span><span style='font-size:10px'>Last 5 Minutes</span>")
  # ) %>%
  # tab_footnote(
  #   footnote = "FGAs in High Leverage situations via pbpstats.com",
  #   locations = cells_column_spanners(spanners = "<span style='font-weight:bold;font-size:12px'>PBP<br></span><span style='font-size:10px'>High Leverage</span>")
  # ) %>%
  # tab_footnote(
  #   footnote = "FGAs in Very High Leverage situations via pbpstats.com",
  #   locations = cells_column_spanners(spanners = "<span style='font-weight:bold;font-size:12px'>PBP<br></span><span style='font-size:10px'>Very High Leverage</span>")
  # ) %>% 
  gtsave("Butler Matchups.png")