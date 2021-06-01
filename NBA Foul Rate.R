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
pbp_team_R <- "https://api.pbpstats.com/get-totals/nba?Season=2020-21&SeasonType=Regular%2BSeason&Type=Team"

json_data <- fromJSON(paste(readLines(pbp_team_PO), collapse=""))
pbp_team_PO <- json_data[["multi_row_table_data"]]
pbp_team_PO[is.na(pbp_team_PO)] = 0
pbp_team_PO <- pbp_team_PO %>% 
  clean_names() %>%
  select(two_pt_shooting_fouls_drawn, three_pt_shooting_fouls_drawn, fg2a, fg3a)
PO2F <- sum(pbp_team_PO$two_pt_shooting_fouls_drawn) / sum(pbp_team_PO$fg2a) * 100
PO3F <- sum(pbp_team_PO$three_pt_shooting_fouls_drawn) / sum(pbp_team_PO$fg3a) * 100

json_data <- fromJSON(paste(readLines(pbp_team_R), collapse=""))
pbp_team_R <- json_data[["multi_row_table_data"]]
pbp_team_R <- pbp_team_R %>% 
  clean_names() %>%
  select(two_pt_shooting_fouls_drawn, three_pt_shooting_fouls_drawn, fg2a, fg3a)
R2F <- sum(pbp_team_R$two_pt_shooting_fouls_drawn) / sum(pbp_team_R$fg2a) * 100
R3F <- sum(pbp_team_R$three_pt_shooting_fouls_drawn) / sum(pbp_team_R$fg3a) * 100

Season <- c("Regular", "Regular", "Playoffs", "Playoffs")
Type <- c("2-Pointers", "3-Pointers", "2-Pointers", "3-Pointers")
Fouls <- c(R2F, R3F, PO2F, PO3F)
data <- data.frame(Season, Type, Fouls)

ggplot(data, aes(fill=Season, y=Fouls, x=Type)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("#17408B", "#C9082A")) +
  theme_owen() +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 10),
        plot.subtitle = element_text(size = 8),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) + 
  labs(x = "Shot Type", 
       y = "Foul %", 
       title = "NBA Shooting Foul %", 
       subtitle = paste0("Regular Season vs. Playoffs\nUpdated ", format(Sys.Date(), "%B %d, %Y")))
