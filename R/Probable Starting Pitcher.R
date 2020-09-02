
# devtools::install_github("BillPetti/baseballr", ref = "development_branch")
# devtools::install_github("BillPetti/baseballr")
require(baseballr)
require(tidyverse)
require(googledrive)
require(openxlsx)
require(parallel)

rm(list = ls())

setwd("~/PitcherList")

# drive_download(as_id("1tVKLlMtZ5xU79snRcx-AwJPNVKUUIdJ3x02bJj8cIDQ"),
#                overwrite = T)
# 
# test = read.xlsx("MLB Probable Starters.xlsx", sheet = 1) %>% 
#   distinct()
# 
# names(test) = test[1,] 
# 
# test = test[-1,]



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
                              "Braves", "Mets", "Angels","Pirates", "Royals","Cardinals"))


statcast_pitcher_data = function(id, season = 2020){

  if(is.na(id)){
    data = data.frame(GS = NA, IP = NA, W = NA,
                      L = NA, ERA = NA, SO = NA, BB = NA,
                      WHIP = NA, `K %` = NA, `BB %` = NA,
                      `Hard Hit %` = NA, `Batted Balls` = NA, Barrels = NA,
                      `Barrel %` = NA, XBA = NA, XSLG = NA,
                      WOBA = NA, XWOBA = NA, XWOBACON = NA,
                      id = NA, csw = NA, check.names = FALSE)
    
    return(data)
    next
  }
  
  # this automatically changes the "james-paxton" part based on the player id (luckily)
  url = paste0("https://baseballsavant.mlb.com/savant-player/james-paxton-",id,"?stats=career-r-pitching-mlb")

  data1 = xml2::read_html(url)

  standard = data1 %>%
    rvest::html_nodes(xpath = '//*[@id="pitchingStandard"]') %>%
    rvest::html_nodes("table") %>%
    # .[[1]] %>% 
    rvest::html_table() %>%
    as.data.frame()
  
  
  if(nrow(standard) == 0){
    data = data.frame(GS = NA, IP = NA, W = NA,
                      L = NA, ERA = NA, SO = NA, BB = NA,
                      WHIP = NA, `K %` = NA, `BB %` = NA,
                      `Hard Hit %` = NA, `Batted Balls` = NA, Barrels = NA,
                      `Barrel %` = NA, XBA = NA, XSLG = NA,
                      WOBA = NA, XWOBA = NA, XWOBACON = NA,
                      id = id, check.names = FALSE)
    return(data)
    next
  }
  
  standard  = standard %>%
    dplyr::select(-1) %>%
    dplyr::filter(Season == season)

  if(nrow(standard) > 1){
    standard = standard %>%
      dplyr::filter(BF == max(BF))
  } else if(nrow(standard) == 0){
    standard = data.frame(GS = NA, IP = NA, W = NA,
                      L = NA, ERA = NA, SO = NA, BB = NA,
                      WHIP = NA, Season = season, check.names = FALSE)
    
  }

  standard = standard %>%
    dplyr::select(GS, IP, W, L, ERA, SO, BB, WHIP, Season)

  statcast = data1 %>%
    rvest::html_nodes(xpath = '//*[@id="statcast_pitching"]') %>%
    rvest::html_nodes("table") %>%
    rvest::html_table() %>%
    as.data.frame() 
  
  if(!season %in% statcast$Season){
    statcast = data.frame(`K %` = NA, `BB %` = NA,
                          `Hard Hit %` = NA, `Batted Balls` = NA, Barrels = NA,
                          `Barrel %` = NA, XBA = NA, XSLG = NA, `Sweet.Spot..` = NA,
                          WOBA = NA, XWOBA = NA, XWOBACON = NA, ERA = NA, xERA = NA,
                          Season = 2020, check.names = FALSE)
  } else if(nrow(statcast) > 0){
    statcast = statcast %>% 
      dplyr::filter(Season == season) %>%
      dplyr::select(-c('Exit.Velocity', 'Launch.Angle', Pitches)) %>%
      dplyr::rename("Barrel %" = Barrel..,
                    "Batted Balls" = Batted.Balls,
                    "Hard Hit %" = Hard.Hit..,
                    "K %" = K..,
                    "BB %" = BB..) %>%
      dplyr::select('K %', 'BB %', 'Hard Hit %', everything())
  } else {
    statcast = data.frame(`K %` = NA, `BB %` = NA,
                      `Hard Hit %` = NA, `Batted Balls` = NA, Barrels = NA,
                      `Barrel %` = NA, XBA = NA, XSLG = NA,
                      WOBA = NA, XWOBA = NA, XWOBACON = NA, Season = season, check.names = FALSE)
  }
  
  statcast = statcast %>% 
    rename(`Sweet Spot %` = Sweet.Spot..) %>% 
    dplyr::select(-ERA) %>% 
    mutate(Season = as.character(Season))
  
  standard = standard %>% 
    mutate(Season = as.character(Season))
  
  
  data = left_join(standard, statcast) %>%
    dplyr::select(-Season) %>%
    mutate(id = as.character(id))
  

  
  dat = scrape_statcast_savant(start_date = "2020-07-01",
                               end_date = Sys.Date() - 1,
                               playerid = id, player_type = "pitcher") %>% 
     mutate(CSW = ifelse(stringr::str_detect(description, "swing|tip|strike"), 1, 0)) %>%
     group_by(player_name, pitcher) %>%
     summarize(pitch_percent = sum(CSW)/n(),
              total_pitches = n()) %>%
     arrange(desc(pitch_percent)) %>% 
    ungroup() %>% 
    dplyr::select(player_name, pitcher, total_pitches, pitch_percent) %>% 
    mutate(pitch_percent = round(100 * pitch_percent, 1)) %>% 
    rename(csw = pitch_percent, id = pitcher) %>% 
    dplyr::select(id, csw) %>%
    mutate(id = as.character(id))
  
  
  data = left_join(data, dat)
  
  
  return(data)
}


url = "https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=8&season=2020&month=0&season1=2020&ind=0&team=0,ts&rost=&age=&filter=&players=0"

teamData = xml2::read_html(url) %>%
  rvest::html_nodes('.rgMasterTable') %>%
  # Extract the raw text as a table
  rvest::html_table() %>%
  as.data.frame() %>%
  dplyr::slice(c(-1,-3))

colnames(teamData) = teamData[1,]

teamData = teamData %>%
  slice(-1) %>%
  dplyr::select(Team, wOBA, 'wRC+')


start = Sys.Date() - 30
end = Sys.Date()

url = paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=8&season=2020&month=1000&season1=2020&ind=0&team=0%2Cts&rost=&age=&filter=&players=0&startdate=", start,"&enddate=",end)

last30 = xml2::read_html(url) %>%
  rvest::html_nodes('.rgMasterTable') %>%
  # Extract the raw text as a table
  rvest::html_table() %>%
  as.data.frame() %>%
  dplyr::slice(c(-1,-3))

colnames(last30) = last30[1,]

last30 = last30 %>%
  slice(-1) %>%
  dplyr::select(Team, wOBA, 'wRC+')


gameData = NULL
StartData = NULL
Opponent = NULL
dates = as.character(seq.Date(Sys.Date(), Sys.Date() + 6, by = "day"))
for(i in dates){
  if(i > as.Date("2020-09-30")) next
  
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
  # dplyr::filter(Date <= as.Date("2019-09-29")) %>% 
  dplyr::filter(Date <= Sys.Date() + 1) %>%
  dplyr::select(Date, Pitcher, id,  Opp., Type, team) %>%
  left_join(., teamAbb[,c("Opp.","Abb")]) %>%
  left_join(., teamAbb[,c("Opp.","Name")], by = c("team" = "Opp.")) %>%
  dplyr::rename('Team Name' = Name) %>% 
  left_join(., teamAbb[,c("Opp.","Name")], by = c("Opp." = "Opp.")) %>%
  left_join(., teamData, by = c("Name" = "Team")) %>%
  dplyr::rename(wOBA_full = wOBA,
                'wRC+_full' = 'wRC+') %>%
  left_join(., last30, by = c("Name" = "Team")) %>%
  dplyr::select(-team) %>%
  mutate(Pitcher = sub('(.*)\\,\\s+(.*)','\\2 \\1', Pitcher),
         Pitcher = ifelse(is.na(Pitcher), paste("TBD", `Team Name`, "SP"), Pitcher)) %>%
  mutate(Opp. = ifelse(Type == "Home", paste("@", Abb), paste("vs.", Abb))) %>%
  dplyr::select(Date, Pitcher, Opp., wOBA_full, 'wRC+_full', wOBA, 'wRC+', id) 



cl <- makeCluster(3)
clusterEvalQ(cl, { c(library(xml2), library(rvest), library(dplyr), library(baseballr)) })  # you need to export packages as well
clusterExport(cl, "statcast_pitcher_data")  # each worker is a new environment, you will need to export variables/functions to
DATA <- parallel::parLapply(cl, Opponent1$id, fun = function(i) statcast_pitcher_data(i))   
stopCluster(cl)

# for(i in Opponent1$id){
#   cat(i)
#   statcast_pitcher_data(i)
# }

# DATA = bind_rows(DATA)

DATA = DATA %>%
  data.table::rbindlist(., fill = TRUE)

DATA$Pitcher = Opponent1$Pitcher


Opponent1 = base::merge(Opponent1, DATA, all.x = T, sort = F)



Opponent1 = Opponent1 %>% 
  dplyr::select(-id) %>% 
  dplyr::select(Date, Pitcher, Opp., wOBA_full, 'wRC+_full', wOBA, 'wRC+', 
                csw, everything()) %>% 
  rename(CSW = csw) %>% 
  distinct()

names(Opponent1)[4] = "wOBA"
names(Opponent1)[5] = "wRC+"



dates = format(seq.Date(Sys.Date(), Sys.Date() + 6, by = "day"), format = "%d-%b-%Y")

wb <- createWorkbook()

negStyle <- createStyle(bgFill = "#FFC7CE", halign = "center", fontSize = 14)
posStyle <- createStyle(bgFill = "#daedf4", halign = "center", fontSize = 14)

for(i in 1:(length(dates) + 1)){

  if(i == 1){
    addWorksheet(wb, "Opponent")
    
    bodyStyle = createStyle(halign = "center", fontSize = 14)
    addStyle(wb, sheet = i, bodyStyle, rows = 1:70, cols = 1:29, gridExpand = TRUE)

    mergeCells(wb, "Opponent", cols = 4:5, rows = 1)
    mergeCells(wb, "Opponent", cols = 6:7, rows = 1)
    mergeCells(wb, "Opponent", cols = 8:29, rows = 1)
    
    writeData(wb, sheet = "Opponent", x = "Hitting Team Full Season", colNames = FALSE, rowNames = FALSE,
              startCol = 4, startRow = 1)
    
    writeData(wb, sheet = "Opponent", x = "Hitting Team Last 30 Days", colNames = FALSE, rowNames = FALSE,
              startCol = 6, startRow = 1)
    
    writeData(wb, sheet = "Opponent", x = "Pitcher Statistics", colNames = FALSE, rowNames = FALSE,
              startCol = 8, startRow = 1)

    writeData(wb, sheet = "Opponent", x = "wOBA", colNames = FALSE, rowNames = FALSE,
              startCol = 4, startRow = 2)
    
    writeData(wb, sheet = "Opponent", x = "wRC+", colNames = FALSE, rowNames = FALSE,
              startCol = 5, startRow = 2)

    
    for(ii in seq_along(Opponent1)){
      writeData(wb, sheet = "Opponent", names(Opponent1)[ii], startCol = ii, startRow = 2)
      icol <- Opponent1[[ii]]
      if(ii >= 4) icol = as.numeric(icol)
      for(j in seq_along(icol)){
        x <- icol[[j]]
        writeData(wb, sheet = "Opponent", x, startCol = ii, startRow = j + 2)
      }
    }

    conditionalFormatting(wb, sheet = "Opponent", cols = 2, rows = 1:70, type = "contains",
                          rule = "TBD", style = negStyle)
    
    conditionalFormatting(wb, sheet = "Opponent", cols = 1, rows=3:70, rule = "A3==$A$3", style = posStyle)
    conditionalFormatting(wb, sheet = "Opponent", cols = 2, rows=3:70, rule = "A3==$A$3", style = posStyle)
    conditionalFormatting(wb, sheet = "Opponent", cols = 3, rows=3:70, rule = "A3==$A$3", style = posStyle)
    

    conditionalFormatting(wb, "Opponent", cols = 4, rows = 3:(nrow(Opponent1[Opponent1$Date == Sys.Date(),]) + 2),
                          style = c("lightgreen", "#FFFFCC", "tomato"), rule = NULL,
                          type = "colourScale")

    
    conditionalFormatting(wb, "Opponent", cols = 5, rows = 3:(nrow(Opponent1[Opponent1$Date == Sys.Date(),]) + 2),
                          style = c("lightgreen", "#FFFFCC", "tomato"), rule = NULL,
                          type = "colourScale")

    conditionalFormatting(wb, "Opponent", cols = 6, rows = 3:(nrow(Opponent1[Opponent1$Date == Sys.Date(),]) + 2),
                          style = c("lightgreen", "#FFFFCC", "tomato"), rule = NULL,
                          type = "colourScale")

    conditionalFormatting(wb, "Opponent", cols = 7, rows = 3:(nrow(Opponent1[Opponent1$Date == Sys.Date(),]) + 2),
                          style = c("lightgreen", "#FFFFCC", "tomato"), rule = NULL,
                          type = "colourScale")
    
    
    conditionalFormatting(wb, "Opponent", cols = 8, rows = 3:(nrow(Opponent1[Opponent1$Date == Sys.Date(),]) + 2),
                          style = c("tomato", "#FFFFCC", "lightgreen"), rule = NULL,
                          type = "colourScale") 


    ## set row heights and col widths
    setColWidths(wb, sheet = "Opponent", cols = 1:29, widths = "auto")
    setRowHeights(wb, sheet = "Opponent", rows = 1:70, heights = 20)
    
    setColWidths(wb, sheet = "Opponent", cols = 4, widths = 13)
    setColWidths(wb, sheet = "Opponent", cols = 5, widths = 13)
    setColWidths(wb, sheet = "Opponent", cols = 6, widths = 13)
    setColWidths(wb, sheet = "Opponent", cols = 7, widths = 13)
    setColWidths(wb, sheet = "Opponent", cols = 8, widths = 9)
    setColWidths(wb, sheet = "Opponent", cols = 9, widths = 8)
    setColWidths(wb, sheet = "Opponent", cols = 27, widths = 12)
    setColWidths(wb, sheet = "Opponent", cols = 28, widths = 15)
    # setColWidths(wb, sheet = "Opponent", cols = 5, widths = 6)
    # setColWidths(wb, sheet = "Opponent", cols = 7, widths = 6)

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
saveWorkbook(wb, "~/PitcherList/Starter Data.xlsx", overwrite = TRUE)



Starter_sheet <- drive_update(media = "~/PitcherList/Starter Data.xlsx",
             file = as_id("1tVKLlMtZ5xU79snRcx-AwJPNVKUUIdJ3x02bJj8cIDQ")) %>%
  drive_share(role = "reader", type = "anyone")

