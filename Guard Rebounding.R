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

#get nba playerIds and headshots
id <- nbastatR::nba_players()
id <- id %>% select(idPlayer, urlPlayerHeadshot)
colnames(id)[which(names(id) == "idPlayer")] <- "PLAYER_ID"

url <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=Totals&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Rebounding&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
url_1 <- "https://stats.nba.com/stats/playerindex?College=&Country=&DraftPick=&DraftRound=&DraftYear=&Height=&Historical=1&LeagueID=00&Season=2020-21&SeasonType=Regular%20Season&TeamID=0&Weight="


res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
player_reb <- data.frame(json_resp$resultSets$rowSet)
colnames(player_reb) <- json_resp[["resultSets"]][["headers"]][[1]]
player_reb[c("REB_CONTEST", "MIN")] <- sapply(player_reb[c("REB_CONTEST", "MIN")], as.numeric)
player_reb <- player_reb %>%
  mutate(CRBPM = round(REB_CONTEST/MIN * 36, 2)) %>%
  select(PLAYER_NAME, PLAYER_ID, CRBPM, MIN)

res <- GET(url = url_1, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
players <- data.frame(json_resp$resultSets$rowSet)
colnames(players) <- json_resp[["resultSets"]][["headers"]][[1]]
players <- select(players, "PERSON_ID", "HEIGHT", "POSITION")
colnames(players)[which(names(players) == "PERSON_ID")] <- "PLAYER_ID"
players <- players %>% 
  separate(HEIGHT, c('feet', 'inches'), "-", convert = TRUE) %>% 
  mutate(HEIGHT = (12*feet + inches)) %>%
  select(PLAYER_ID, HEIGHT, POSITION)

combined <- merge(player_reb, players, by="PLAYER_ID")
combined <- merge(id, combined, by="PLAYER_ID")
combined <- combined %>%
  #filter(HEIGHT <= 76) %>%
  filter(POSITION == "G") %>%
  arrange(desc(CRBPM)) %>%
  filter (MIN >= 1250) %>%
  slice(1:6) %>%
  mutate(Rank = c(1:6)) %>%
  select (urlPlayerHeadshot, PLAYER_NAME, CRBPM, Rank)

combined %>%
  arrange(desc(CRBPM)) %>%
  gt()  %>%
  cols_label(urlPlayerHeadshot = "",
             PLAYER_NAME = "Player",
             CRBPM = "Contested Reb./36",
             Rank = "Rank") %>%
  tab_header(
    title = md("Guard Rebounding"),
    subtitle = paste0("2021 Season | Updated ", format(Sys.Date(), format="%B %d, %Y"))
  )  %>%
  text_transform(
    locations = cells_body(vars(urlPlayerHeadshot)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  tab_spanner(
    label = "Rebounds",
    columns = vars(CRBPM, Rank)
  ) %>%
  data_color(
    columns = c(CRBPM),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "right",
    columns = vars(Rank)
  ) %>%
  cols_width(vars(CRBPM) ~ px(60),
             vars(Rank) ~ px(30)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = PLAYER_NAME == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = PLAYER_NAME == "League Average")
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
  gtsave("Guard Rebounding.png")