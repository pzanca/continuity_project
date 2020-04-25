library(tidyverse)
library(rvest)
library(RSelenium)
library(ballr)

#### Collect Team Schedule/Results data ####

#Create a list of team abbreviations (based on the organization's root menu on Basketball Reference)
teams <- c("ATL", "BOS", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW", "HOU",
           "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOH", "NYK", "NJN",
           "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")

#Create an empty dataframe to capture all teams' schedules
schedules_all <- data.frame()

#Loop through each team
for (team in teams) {
  
  #Loop through each season from 1994-95 to 2019-20
  for (season in 1995:2020) {
    
    #Deal with teams that have changed names, cities, etc. during this period
    if (team == "NJN" & season > 2012) {
      team_abbrev = "BRK"
    } else if (team == "CHA" & season > 2014) {
      team_abbrev = "CHO"
    } else if (team == "CHA" & season %in% 2003:2004) {
      next
    } else if (team == "CHA" & season < 2003) {
      team_abbrev = "CHH"
    } else if (team == "MEM" & season < 1996) {
      next
    } else if (team == "MEM" & season < 2002) {
      team_abbrev = "VAN"
    } else if (team == "NOH" & season > 2013) {
      team_abbrev = "NOP"
    } else if (team == "NOH" & season %in% 2006:2007) {
      team_abbrev = "NOK"
    } else if (team == "NOH" & season < 2003) {
      next
    } else if (team == "OKC" & season < 2009) {
      team_abbrev = "SEA"
    } else if (team == "TOR" & season < 1996) {
      next
    } else if (team == "WAS" & season < 1998) {
      team_abbrev = "WSB"
    } else {
      team_abbrev = team
    }
    
    #Set up the URL for the team's schedule page
    url <- paste0("https://www.basketball-reference.com/teams/", team_abbrev, "/", season, "_games.html")
    
    #Extract and read the HTML
    html <- read_html(url)
    
    #Extract the table, clean it up, and add the team and season info
    schedule <- html %>%
      html_table() %>%
      .[[1]] %>%
      janitor::remove_empty("cols") %>%
      janitor::clean_names() %>%
      filter(g != "G") %>%
      select(g, date, opponent, outcome = x2, w, l, pts = tm, opts = opp) %>%
      mutate(team_master = team,
             team_abbrev = team_abbrev,
             season = season)
    
    #Bind the current team/season schedule to the overall dataframe
    schedules_all <- rbind(schedules_all,
                           schedule)
    
  }
  
}

#Write results to a CSV
write_csv(schedules_all, "data\\schedules_all.csv")

#### Collect overall Minutes Played data (not game level) ####
# This provides us with the list of players for each season, as well as an additional way to look at minutes played.

#Create empty dataframe in which to capture all players' per game stats
per_game_all <- data.frame()

#Loop through each season from 1994-95 to 2019-20
for (season in 1995:2020) {
  
  #Use the ballr package to extract all players' per game stats and append the season
  per_game <- NBAPerGameStatistics(season = season) %>%
    mutate(season = season)
  
  #Bind the current season's dataframe to the overall dataframe
  per_game_all <- rbind(per_game_all,
                        per_game)
  
}

#Calculate MP for the players by multiplying their MPG by G
mp_all <- per_game_all %>%
  mutate(mp = g * mp) %>%
  select(season, player, link, tm, mp) %>%
  #Filter out the Total rows for players who played for multiple teams in one season
  filter(tm != "TOT")

#Write results to a CSV
write_csv(mp_all, "data\\mp_all.csv")


#### Get Game Logs from Bball Ref ####

#Read in dataframe from previous section
mp_all <- read_csv("data\\mp_all.csv")

#The player link/URL is missing for Hall of Fame players
#So we export this list, complete it in Excel and reimport it
missing_links <- mp_all %>% 
  distinct(player, link) %>% 
  filter(is.na(link)) %>%
  write_csv("data\\missing_links.csv")

#Reimport the missing links
missing_links <- read_csv("data\\missing_links_updated.csv")

#Identify all of the player seasons in the data set
player_seasons <- mp_all %>%
  left_join(missing_links %>%
              select(player, link2 = link),
            by = "player") %>%
  mutate(link = ifelse(is.na(link), link2, link)) %>%
  distinct(season, player, link)

#Set up an empty dataframe
gamelogs_all <- data.frame()

#Loop through each player season
### NOTE: THIS LOOP TAKES 10-12 HOURS TO RUN!!!
for (player_season in 1:nrow(player_seasons)) {
  
  #Identify the player link for that player season
  player_link <- str_sub(player_seasons$link[player_season], end = -6)
  
  #Create the URL for that player-season combination
  url <- paste0("https://www.basketball-reference.com", player_link, "/gamelog/", player_seasons$season[player_season])
  
  #Read the HTML
  html <- read_html(url)
  
  #Extract the game log table and add the player link and season as values
  gamelogs <- html %>%
    html_table(fill = TRUE) %>%
    .[[8]] %>%
    janitor::clean_names() %>%
    mutate(link = player_seasons$link[player_season],
           season = player_seasons$season[player_season])
  
  #If there are 32 columns, remove that column (x_3)
  #(At some point in the last 25 years, plus-minus got added to the game logs.)
  if (ncol(gamelogs) == 32) {
    gamelogs <- gamelogs %>%
      select(-x_3)
  }
  
  #Bind the current data frame to the overarching data frame
  gamelogs_all <- rbind(gamelogs_all,
                        gamelogs)
  
}

#Write the data to a CSV
write_csv(gamelogs_all, "data\\gamelogs_all.csv")

#Import the data from a CSV
gamelogs_all <- read_csv("data\\gamelogs_all.csv")

#Clean up the gamelog data
gamelogs_clean <- gamelogs_all %>%
  filter(rk != "Rk") %>%
  mutate(date = as.Date(date),
         min = as.numeric(str_sub(mp, end = -4)) * 60,
         sec = as.numeric(str_sub(mp, start = -2)),
         mp = (min + sec) / 60) %>%
  select(date, tm, mp, season, link)
