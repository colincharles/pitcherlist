require(baseballr)
require(dplyr)
require(googledrive)
require(openxlsx)


teamAbb = data.frame(Opp. = c("Toronto Blue Jays", "Texas Rangers", "New York Yankees",
                              "Baltimore Orioles", "Cleveland Indians", "Boston Red Sox",
                              "Milwaukee Brewers", "Minnesota Twins", "Chicago White Sox",
                              "Houston Astros", "Colorado Rockies", "Arizona Diamondbacks",
                              "San Diego Padres", "Tampa Bay Rays", "San Francisco Giants",
                              "Oakland Athletics", "Washington Nationals", "Cincinnati Reds",
                              "Philadelphia Phillies", "Chicago Cubs", "Miami Marlins",
                              "Los Angeles Dodgers", "Detroit Tigers", "Seattle Mariners",
                              "Atlanta Braves", "New York Mets", "Los Angeles Angels",
                              "Pittsburgh Pirates", "Kansas City Royals","St. Louis Cardinals"),
                     Abb = c("TOR","TEX","NYY","BAL","CLE","BOS","MIL","MIN","CHW","HOU","COL",
                             "ARI","SD","TB","SF","OAK","WSH","CIN","PHI","CHC","MIA","LAD","DET","SEA",
                             "ATL","NYM","LAA","PIT","KC","STL"),
                     Name = c("Blue Jays", "Rangers", "Yankees","Orioles", "Indians", "Red Sox",
                              "Brewers", "Twins", "White Sox","Astros", "Rockies", "Diamondbacks",
                              "Padres", "Rays", "Giants","Athletics", "Nationals", "Reds",
                              "Phillies", "Cubs", "Marlins","Dodgers", "Tigers", "Mariners",
                              "Braves", "Mets", "Angels","Pirates", "Royals","Cardinals"), stringsAsFactors = F)

gameData = NULL
StartData = NULL
Opponent = NULL
dates = as.character(seq.Date(Sys.Date()-5, Sys.Date() + 40, by = "day"))
for(i in dates){
  game_pks = get_game_pks_mlb(i)
  
  gameData1 = data.frame('Game Time' = as.POSIXct(as.character(game_pks$gameDate), format = "%Y-%m-%dT%H:%M:%SZ") - 6*60*60,
                         game_pk = game_pks$game_pk,
                         'Away Team' = game_pks$teams.away.team.name,
                         'Home Team' = game_pks$teams.home.team.name,
                         Venue = game_pks$venue.name,
                         check.names = F)
  
  gameData = rbind(gameData, gameData1)
  
  for(j in game_pks$game_pk){
    
    start1 = get_probables_mlb(j) %>% 
      dplyr::select(game_pk, fullName, team) %>% 
      tidyr::spread(team, fullName)
    
    home_team = game_pks %>% 
      dplyr::filter(game_pk == j) %>% 
      dplyr::select(teams.home.team.name)
    
    away_team = game_pks %>% 
      dplyr::filter(game_pk == j) %>% 
      dplyr::select(teams.away.team.name)
    
    opponent = get_probables_mlb(j) %>% 
      dplyr::select(game_pk, fullName, id,  team) %>% 
      mutate(Opp. = rev(team)) %>% 
      # dplyr::select(-team) %>% 
      mutate(Date = i) %>% 
      dplyr::rename(Pitcher = fullName) %>% 
      mutate(Type = ifelse(Opp. == home_team$teams.home.team.name, "Home", "Away"))
    
    names(start1)[names(start1) == unlist(home_team)] = "Home"
    names(start1)[names(start1) == unlist(away_team)] = "Away"
    
    StartData = rbind(StartData, start1)
    Opponent = rbind(Opponent, opponent)
  }
}
head(StartData)
tail(StartData)




x1 = gameData %>% 
  mutate(Date = format(`Game Time`, format = "%b-%d")) %>% 
  # head(n = 50) %>%
  dplyr::rename(Team1 = `Home Team`,
                Team2 = `Away Team`) %>% 
  dplyr::select(Date, Team1, Team2) %>% 
  reshape2::melt(., id.vars = "Date") %>% 
  dplyr::select(Date, Team = value) %>% 
  group_by(Date, Team) %>% 
  tally() %>% 
  ungroup() %>% 
  tidyr::spread(key = Date, n, convert = F, fill = 0, drop = F) 


StarterData = left_join(gameData, StartData) %>% 
  mutate(Away = ifelse(is.na(Away), "TBD", Away),
         Home = ifelse(is.na(Home), "TBD", Home)) %>% 
  mutate('Game Date' = format(`Game Time`, format = "%d-%b-%Y"),
         'Game Time' = format(`Game Time`, format = "%I:%M %p")) %>% 
  dplyr::select('Game Date', 'Game Time', everything(), -game_pk) %>% 
  dplyr::rename('Away Pitcher' = Away,
                'Home Pitcher' = Home)



pitchData = NULL
for(i in 1:nrow(x1)){
  Teamx = x1$Team[i]
  Team1 = StarterData %>% 
    dplyr::filter(`Away Team` == Teamx | `Home Team` == Teamx) %>% 
    mutate(Date = as.POSIXct(paste(`Game Date`, `Game Time`), format = "%d-%b-%Y %H:%M %p")) %>% 
    arrange(Date) %>% 
    mutate(Team = ifelse(`Away Team` == Teamx, as.character(`Away Team`), as.character(`Home Team`))) %>% 
    mutate(Pitcher = ifelse(`Away Team` == Teamx, as.character(`Away Pitcher`), as.character(`Home Pitcher`))) %>% 
    dplyr::filter(Date > as.POSIXct("2019/08/27")) %>% 
    mutate(N = 1:n())
  
  Pitcher1 = Team1 %>% 
    dplyr::filter(N %in% seq(1, max(Team1$N), 5)) %>% 
    dplyr::select(`Game Date`, `Game Time`, `Away Team`, `Home Team`, Pitcher) %>% 
    mutate(Pitcher = Pitcher[1])
  
  Pitcher2 = Team1 %>% 
    dplyr::filter(N %in% seq(2, max(Team1$N), 5)) %>% 
    dplyr::select(`Game Date`, `Game Time`, `Away Team`, `Home Team`, Pitcher) %>% 
    mutate(Pitcher = Pitcher[1])
  
  Pitcher3 = Team1 %>% 
    dplyr::filter(N %in% seq(3, max(Team1$N), 5)) %>% 
    dplyr::select(`Game Date`, `Game Time`, `Away Team`, `Home Team`, Pitcher) %>% 
    mutate(Pitcher = Pitcher[1])
  
  Pitcher4 = Team1 %>% 
    dplyr::filter(N %in% seq(4, max(Team1$N), 5)) %>% 
    dplyr::select(`Game Date`, `Game Time`, `Away Team`, `Home Team`, Pitcher) %>% 
    mutate(Pitcher = Pitcher[1])
  
  Pitcher5 = Team1 %>% 
    dplyr::filter(N %in% seq(5, max(Team1$N), 5)) %>% 
    dplyr::select(`Game Date`, `Game Time`, `Away Team`, `Home Team`, Pitcher) %>% 
    mutate(Pitcher = Pitcher[1])
  

  
  df = rbind(Pitcher1, Pitcher2, Pitcher3, Pitcher4, Pitcher5) %>% 
    # dplyr::filter(as.POSIXct(paste(`Game Date`, `Game Time`), format = "%d-%b-%Y %H:%M %p") >= as.POSIXct("2019/08/27")) %>% 
    mutate(Opp. = ifelse(`Away Team` == Teamx, as.character(`Home Team`), as.character(`Away Team`))) %>% 
    left_join(teamAbb, by = "Opp.") %>% 
    mutate(Abb = ifelse(`Home Team` == Teamx, Abb, paste("@", Abb))) %>% 
    dplyr::filter(Pitcher != "TBD")
  
  for(j in unique(df$Pitcher)){
    df1 = df %>% 
      dplyr::filter(Pitcher == j) %>% 
      mutate(Date = as.POSIXct(paste(`Game Date`, `Game Time`), format = "%d-%b-%Y %H:%M %p")) %>% 
      mutate(Lag = Date - lag(Date))
    
    if(any(na.omit(df1$Lag) < 0)){
      df1 = df1[1:(which(df1$Lag < 0)-1),]
    }
    
    df1 = df1 %>%
      mutate(Date = substr(`Game Date`, 1, 6)) %>% 
      mutate(Abb = paste0(Abb, " (", Date, ")")) %>% 
      dplyr::select(Pitcher, Abb) %>% 
      mutate(Start = paste("Start", 1:n())) %>% 
      tidyr::spread(Start, Abb) 
    
     if(!is.null(nrow(pitchData))){
       if(ncol(df1) < ncol(pitchData)){
         df1$`Start 7` = NA
       }
       # } else if(ncol(df1) > ncol(pitchData)) {
       #   df1 = df1 %>% 
       #     dplyr::select(Pitcher, paste("Start", 1:(ncol(pitchData)-1)))
       # }
     } 
    
    pitchData = rbind(pitchData, df1)
  }
  
  # for(k in 2:ncol(x1)){
  #   x1[i,k] = x1[i,(k-1)] + x1[i,k]
  #   names(x1)[k]
  # }
  
}

pitchData %>% 
  arrange(Pitcher) %>% 
  write.csv(., "~/Projected Schedule ROS 2019.csv", row.names = F)

