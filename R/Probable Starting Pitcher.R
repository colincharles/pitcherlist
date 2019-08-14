
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
                              "Padres", "Bay Rays", "Giants","Athletics", "Nationals", "Reds",
                              "Phillies", "Cubs", "Marlins","Dodgers", "Tigers", "Mariners",
                              "Braves", "Mets", "Angels","Pirates", "Royals","Cardinals"))
gameData = NULL
StartData = NULL
Opponent = NULL
dates = as.character(seq.Date(Sys.Date(), Sys.Date() + 6, by = "day"))
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
      dplyr::select(game_pk, fullName, team) %>% 
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


StarterData = left_join(gameData, StartData) %>% 
  mutate(Away = ifelse(is.na(Away), "TBD", Away),
         Home = ifelse(is.na(Home), "TBD", Home)) %>% 
  mutate('Game Date' = format(`Game Time`, format = "%d-%b-%Y"),
         'Game Time' = format(`Game Time`, format = "%I:%M %p")) %>% 
  dplyr::select('Game Date', 'Game Time', everything(), -game_pk) %>% 
  dplyr::rename('Away Pitcher' = Away,
                'Home Pitcher' = Home)



Opponent1 = Opponent %>% 
  dplyr::filter(Date <= Sys.Date() + 1) %>% 
  dplyr::select(Date, Pitcher, Opp., Type, team) %>% 
  left_join(., teamAbb[,c("Opp.","Abb")]) %>% 
  left_join(., teamAbb[,c("Opp.","Name")], by = c("team" = "Opp.")) %>% 
  dplyr::select(-team) %>% 
  mutate(Pitcher = sub('(.*)\\,\\s+(.*)','\\2 \\1', Pitcher),
         Pitcher = ifelse(is.na(Pitcher), paste("TBD", Name, "SP"), Pitcher)) %>% 
  mutate(Opp. = ifelse(Type == "Home", paste("@", Abb), paste("vs.", Abb))) %>% 
  dplyr::select(Date, Pitcher, Opp.)

dates = format(seq.Date(Sys.Date(), Sys.Date() + 6, by = "day"), format = "%d-%b-%Y")

wb <- createWorkbook()

negStyle <- createStyle(bgFill = "#FFC7CE", halign = "center", fontSize = 14)
posStyle <- createStyle(bgFill = "#daedf4", halign = "center", fontSize = 14)

for(i in 1:(length(dates) + 1)){
  ## Create a new workbook
  
  ## Add a worksheet
  if(i == 1){
    addWorksheet(wb, "Opponent") 
    
    bodyStyle = createStyle(halign = "center", fontSize = 14)
    addStyle(wb, sheet = i, bodyStyle, rows = 1:70, cols = 1:3, gridExpand = TRUE)
    
    ## set row heights and col widths
    setColWidths(wb, sheet = i, cols = 1:3, widths = "auto")
    setRowHeights(wb, sheet = i, rows = 1:70, heights = 20)
    
    writeData(wb, sheet = i, x = Opponent1)
    
    conditionalFormatting(wb, sheet = i, cols = 1, rows=2:70, rule = "A2==$A$2", style = posStyle)
    conditionalFormatting(wb, sheet = i, cols = 2, rows=2:70, rule = "A2==$A$2", style = posStyle)
    conditionalFormatting(wb, sheet = i, cols = 3, rows=2:70, rule = "A2==$A$2", style = posStyle)

    conditionalFormatting(wb, sheet = i, cols = 2, rows=1:70, type = "contains", rule="TBD", style = negStyle)

    
    next
    
  } else if(i == 2){
    addWorksheet(wb, "Today's Games") 
  } else if(i == 3){
    addWorksheet(wb, "Tomorrow's Games") 
  } else {
    addWorksheet(wb, paste(format(as.Date(dates[i-1], format = "%d-%b-%Y"), format = "%A"), dates[i-1]))
  }
  
  bodyStyle = createStyle(halign = "center", fontSize = 14)
  addStyle(wb, sheet = i, bodyStyle, rows = 1:70, cols = 1:20, gridExpand = TRUE)
  
  
  ## set row heights and col widths
  setColWidths(wb, sheet = i, cols = 1:8, widths = "auto")
  setRowHeights(wb, sheet = i, rows = 1:70, heights = 20)
  
  dat = StarterData %>% 
    dplyr::filter(`Game Date` == dates[i-1])
  # write to workbook
  writeData(wb, sheet = i, x = dat)
}

## Save workbook
saveWorkbook(wb, "C:/Users/charlesc/Desktop/Starter Data.xlsx", overwrite = TRUE)

drive_update(media = "C:/Users/charlesc/Desktop/Starter Data.xlsx",
             file = as_id("1tVKLlMtZ5xU79snRcx-AwJPNVKUUIdJ3x02bJj8cIDQ")) %>% 
  drive_share(role = "reader", type = "anyone")
