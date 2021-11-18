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

players <- nbastatR::nba_players()
players <- players %>% select(urlPlayerHeadshot, namePlayer)
colnames(players)[which(names(players) == "namePlayer")] <- "name"


url <- "https://api.pbpstats.com/get-game-stats?Type=Player&GameId=0042000301"
url_1 <- "https://api.pbpstats.com/get-game-stats?Type=Player&GameId=0042000302"
url_2 <- "https://api.pbpstats.com/get-game-stats?Type=Player&GameId=0042000303"
url_3 <- "https://api.pbpstats.com/get-game-stats?Type=Player&GameId=0042000304"
url_4 <- "https://api.pbpstats.com/get-game-stats?Type=Player&GameId=0042000305"
#url_5 <- "https://api.pbpstats.com/get-game-stats?Type=Player&GameId=0042000306"

json_data <- fromJSON(paste(readLines(url), collapse=""))
GAME1 <- json_data[["stats"]][["Away"]][["FullGame"]]
GAME1 <- GAME1 %>%
  clean_names() %>%
  separate(minutes, c('Minutes', 'Seconds'), ":", convert = TRUE) %>%
  mutate(Time1 = round(Minutes + (Seconds/60), 1)) %>%
  select(name, Time1) %>%
  filter(name != "Team")

json_data <- fromJSON(paste(readLines(url_1), collapse=""))
GAME2 <- json_data[["stats"]][["Away"]][["FullGame"]]
GAME2 <- GAME2 %>%
  clean_names() %>%
  separate(minutes, c('Minutes', 'Seconds'), ":", convert = TRUE) %>%
  mutate(Time2 = round(Minutes + (Seconds/60), 1)) %>%
  select(name, Time2) %>%
  filter(name != "Team")

json_data <- fromJSON(paste(readLines(url_2), collapse=""))
GAME3 <- json_data[["stats"]][["Home"]][["FullGame"]]
GAME3 <- GAME3 %>%
  clean_names() %>%
  separate(minutes, c('Minutes', 'Seconds'), ":", convert = TRUE) %>%
  mutate(Time3 = round(Minutes + (Seconds/60), 1)) %>%
  select(name, Time3) %>%
  filter(name != "Team")
  
json_data <- fromJSON(paste(readLines(url_3), collapse=""))
GAME4 <- json_data[["stats"]][["Home"]][["FullGame"]]
GAME4 <- GAME4 %>%
  clean_names() %>%
  separate(minutes, c('Minutes', 'Seconds'), ":", convert = TRUE) %>%
  mutate(Time4 = round(Minutes + (Seconds/60), 1)) %>%
  select(name, Time4) %>%
  filter(name != "Team")

json_data <- fromJSON(paste(readLines(url_4), collapse=""))
GAME5 <- json_data[["stats"]][["Away"]][["FullGame"]]
GAME5 <- GAME5 %>%
  clean_names() %>%
  separate(minutes, c('Minutes', 'Seconds'), ":", convert = TRUE) %>%
  mutate(Time5 = round(Minutes + (Seconds/60), 1)) %>%
  select(name, Time5) %>%
  filter(name != "Team")

# json_data <- fromJSON(paste(readLines(url_5), collapse=""))
# GAME6 <- json_data[["stats"]][["Away"]][["FullGame"]]
# GAME6 <- GAME6 %>%
#   clean_names() %>%
#   separate(minutes, c('Minutes', 'Seconds'), ":", convert = TRUE) %>%
#   mutate(Time6 = round(Minutes + (Seconds/60), 1)) %>%
#   select(name, Time6) %>%
#   filter(name != "Team")


data <- merge(GAME1, GAME2, all = TRUE, by = "name")
data <- merge(data, GAME3, all = TRUE, by = "name")
data <- merge(data, GAME4, all = TRUE, by = "name")
data <- merge(data, GAME5, all = TRUE, by = "name")
#data <- merge(data, GAME6, all = TRUE, by = "name")
data <- merge(players, data, by = "name")
data[is.na(data)] <- 0

data <- data %>%
  mutate(AVG = round((Time1 + Time2 + Time3 + Time4 + Time5)/5, 1)) %>%
  filter(AVG >= 5) %>%
  select(urlPlayerHeadshot, name, Time1, Time2, Time3, Time4, Time5, AVG)

data %>%
  arrange(desc(AVG)) %>%
  gt() %>%
  cols_label(name = "Player",
             Time1 = "Game 1",
             Time2 = "Game 2",
             Time3 = "Game 3",
             Time4 = "Game 4",
             Time5 = "Game 5",
             #Time6 = "Game 6",
             AVG = "Average",
             urlPlayerHeadshot = "") %>%
  tab_header(
    title = md("Hawks Rotation"),
    subtitle = "Minutes for Each Players by Game"
  )  %>%
  text_transform(
    locations = cells_body(vars(urlPlayerHeadshot)),
    fn = function(x) {
      web_image(url = x, 
                height = px(22.5)) 
    }
  ) %>%
  data_color(
    columns = c(Time1, Time2, Time3, Time4, Time5, AVG),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "center",
    columns = vars(Time1, Time2, Time3, Time4, Time5, AVG)
  ) %>%
  cols_width(vars(Time1, Time2, Time3, Time4, Time5, AVG) ~ px(45),
             vars(Time1, Time2, Time3, Time4, Time5, AVG) ~ px(30)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = name == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = name == "League Average")
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
  gtsave("Hawks Rotation.png")
