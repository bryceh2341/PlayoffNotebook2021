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
library(prob)
library(ggbeeswarm)

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

url <- "https://api.pbpstats.com/get-totals/nba?Season=2020-21&SeasonType=Playoffs&Type=Lineup"
url_1 <- "https://api.pbpstats.com/get-totals/nba?Season=2020-21&SeasonType=Regular%2BSeason&Type=Player"

json_data <- fromJSON(paste(readLines(url), collapse=""))
lineups <- json_data[["multi_row_table_data"]]

json_data <- fromJSON(paste(readLines(url_1), collapse=""))
players <- json_data[["multi_row_table_data"]]

players <- players %>%
  clean_names() %>%
  mutate(threepp = fg3a / off_poss * 100) %>%
  #filter(fg3pct > .325 & threepp > 5) %>%
  select(entity_id, name, off_poss, fg3a, fg3pct) %>%
  mutate(threepp = fg3a / off_poss * 100) %>%
  arrange(desc(fg3pct)) %>%
  dplyr::filter(fg3pct >= .333) %>%
  #slice(1:301) %>%
  arrange(desc(threepp)) %>%
  dplyr::filter(threepp >= 5) #%>%
  #slice(1:260)

lineups <- lineups %>%
  clean_names() %>%
  select(entity_id, short_name, off_poss, points, pts_unassisted2s, pts_unassisted3s, team_id, shot_quality_avg, efg_pct) %>%
  separate(entity_id, c("Player 1", "Player 2", "Player 3", "Player 4", "Player 5"), "-") %>%
  mutate(OffRtg = points / off_poss * 100) %>%
  dplyr::filter(off_poss >= 10) %>%
  mutate(count = 0)

for (i in 1:260){
if((lineups$`Player 1`[i] %in% players$entity_id) == TRUE) {
  lineups$count[i] <- lineups$count[i] + 1
}
if((lineups$`Player 2`[i] %in% players$entity_id) == TRUE) {
  lineups$count[i] <- lineups$count[i] + 1
}
if((lineups$`Player 3`[i] %in% players$entity_id) == TRUE) {
  lineups$count[i] <- lineups$count[i] + 1
}
if((lineups$`Player 4`[i] %in% players$entity_id) == TRUE) {
  lineups$count[i] <- lineups$count[i] + 1
}
if((lineups$`Player 5`[i] %in% players$entity_id) == TRUE) {
  lineups$count[i] <- lineups$count[i] + 1
}
}

lineups[c("count")] <- sapply(lineups[c("count")], as.character)


ggplot(lineups, aes(x = count, y = OffRtg, fill = count)) +
  geom_violin(trim = FALSE) +
  theme_owen() +
  theme(legend.position="none") +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 10),
        plot.subtitle = element_text(size = 8),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) + 
  labs(x = "# of Shooters", 
       y = "Offensive Rating", 
       title = "Offensive Rating by Number of Capable Shooters", 
       subtitle = paste0("Capable shooters defined as players who shoot 33.3% and 5 attempts per 100 poss.\nUpdated ", format(Sys.Date(), "%B %d, %Y")))

ggplot(lineups, aes(x = count, y = shot_quality_avg, fill = count)) +
  geom_violin(trim = FALSE) +
  theme_owen() +
  theme(legend.position="none") +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 10),
        plot.subtitle = element_text(size = 8),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) + 
  labs(x = "# of Shooters", 
       y = "Shot Quality", 
       title = "Shot Quality by Number of Capable Shooters", 
       subtitle = paste0("Capable shooters defined as players who shoot 33.3% and 5 attempts per 100 poss.\nUpdated ", format(Sys.Date(), "%B %d, %Y")))
