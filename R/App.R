


#add in code to calculate KDE for where pitches crossed the plate

ksFUN_plate = function(data){
  
  H = Hpi(data[,c("plate_x","plate_z")], binned = TRUE) * 1
  
  fhata = kde(data[,c("plate_x","plate_z")], H = H, compute.cont = TRUE,
              xmin = c(minX, minY), xmax = c(maxX, maxY))
  
  res95 = data.frame(HR = contourSizes(fhata, cont = 95, approx = TRUE))
  res50 = data.frame(HR = contourSizes(fhata, cont = 50, approx = TRUE))
  
  dimnames(fhata[['estimate']]) = list(fhata[["eval.points"]][[1]],
                                       fhata[["eval.points"]][[2]])
  dat = reshape2::melt(fhata[['estimate']])
  dat$breaks50 = fhata[["cont"]]["50%"]
  dat$breaks95 = fhata[["cont"]]["5%"]
  dat$HR95 = round(res95$HR, 2)
  dat$CA50 = round(res50$HR, 2)
  
  return(dat)
}


minX = min(pitch1$plate_x, na.rm = T); maxX = max(pitch1$plate_x, na.rm = T)
minY = min(pitch1$plate_z, na.rm = T); maxY = max(pitch1$plate_z, na.rm = T)

xx = pitch1 %>%
  dplyr::filter(pitch_name != "") %>% 
  group_by(player_name, pitch_name) %>%
  do(as.data.frame(ksFUN(.)))


breaks = xx %>%
  group_by(player_name, pitch_name) %>%
  summarize(breaks95 = mean(breaks95),
            breaks50 = mean(breaks50))

pl <- ggplot(data = xx, aes(x = Var1, y = Var2, fill = pitch_name)) + facet_wrap(~player_name, scales = "free") +
  geom_point(data = pitch1[pitch1$pitch_name != "",],
             aes(x = plate_x, y = plate_z, col = pitch_name), size = rel(0.3)) + 
  # xlab("Horizontal Release Point") + ylab("Vertical Release Point") + 
  theme(strip.background = element_rect(fill = "lightblue"),
        strip.text = element_text(face = "bold", size = rel(1.3)))
groups <- unique(xx$pitch_name)
player_name <- unique(xx$player_name)

# loop and add for each group
for(i in groups){
  for(j in player_name){
    pl <- pl + stat_contour(data = xx[xx$pitch_name == i & xx$player_name == j,], aes(z = value, col = pitch_name),
                            breaks = breaks[breaks$pitch_name == i & breaks$player_name == j,]$breaks95, 
                            alpha = 0.3, geom = "polygon") + theme_bw() + theme(panel.grid = element_blank())
  }
}
pl + facet_wrap(~pitch_name) +
  geom_path(data = SZ, aes(x = x, y = z), col = 1, size = 1.05, inherit.aes = FALSE) 


require(baseballr)
require(dplyr)
require(ggplot2)
require(msm)
require(shiny)
require(shinydashboard)
require(ks)
# require(highcharter)


# Map of 2018 MLB Player Names and IDs
master = read.csv("http://crunchtimebaseball.com/master.csv")

master1 = master %>%
  dplyr::filter(mlb_pos == "P") %>%
  dplyr::select(mlb_name, mlb_id, fg_id, mlb_team) %>%
  dplyr::mutate_all(as.character) %>%
  dplyr::mutate(mlb_id = as.numeric(mlb_id))


player_list = master1 %>%
  collect() %>%
  base::split(.$mlb_name) %>%
  purrr::map(~.$mlb_id)


ui = dashboardPage(
  #Header
  dashboardHeader(title = "Statcast Analysis"),
  dashboardSidebar(
    selectInput("Player",
                label = "Player:", 
                choices = player_list, 
                selectize = TRUE,
                selected = 453286),
    sidebarMenu(
      menuItem("First Pitch %", tabName = "first_pitch"),
      menuItem("Total Pitches", tabName = "total_pitches"),
      menuItem("Prob of Next", tabName = "pnext"),
      menuItem("Statcast", tabName = "Statcast"),
      menuItem("Heat Map", tabName = "heat_map"),
      menuItem("Release Points", tabName = "Release_Point"),
      menuItem("Pitch Locations", tabName = "Pitch_Locations"),
      menuItem("Pitch Velocity", tabName = "PitchVelocity")
    )),
  # Body
  dashboardBody(tabItems(
    tabItem(tabName = "first_pitch",
            fluidRow(
              column(12,
                     dataTableOutput('first_pitch')
              )
            )),
    tabItem(tabName = "total_pitches",
            fluidRow(
              column(12,
                     dataTableOutput('total_pitches')
              )
            )),
    tabItem(tabName = "Statcast",
            fluidRow(
              column(12,
                     dataTableOutput('statcast_summary')
              )
            )),
    tabItem(tabName = "pnext",
            fluidRow(
              column(12,
                     dataTableOutput('pnext')
              )
            )),
    # Second tab content
    tabItem(tabName = "Release_Point",
            fluidRow(
              plotOutput("release_point")
            )),
    tabItem(tabName = "Pitch_Locations",
            fluidRow(
              plotOutput("Pitch_Locations")
            )),
    tabItem(tabName = "heat_map",
            fluidRow(
              plotOutput("heat_map")
            )),
    tabItem(tabName = "PitchVelocity",
            fluidRow(
              plotOutput("pitch_velo")
            )))
  )
)



server <- function(input, output, session) { 
  
  datasetInput <- reactive({
    scrape_statcast_savant_pitcher(start_date = "2018-03-31", end_date = "2018-10-01", pitcherid = input$Player) %>%
      dplyr::arrange(game_pk, inning, at_bat_number, pitch_number) %>%
      dplyr::mutate(Type = ifelse(game_date < as.Date("2018-07-14"),"First Half", "Second Half")) %>%
      dplyr::mutate(ID = paste0(game_pk, inning, at_bat_number)) %>%
      dplyr::filter(pitch_name != "") %>%
      dplyr::mutate(Pitch = pitch_name) 
  })
  
  output$first_pitch = renderDataTable({
    
    dataset = datasetInput()
    dataset %>%
      # dplyr::rename(Pitch = pitch_name) %>%
      group_by(Pitch) %>%
      dplyr::filter(pitch_number == 1) %>%
      dplyr::count(Pitch) %>%
      ungroup() %>%
      dplyr::rename(Proportion = n) %>%
      dplyr::mutate(`First Pitch %` = paste(round(100 * Proportion/sum(Proportion), 1), "%")) %>%
      dplyr::select(-Proportion)

  })
  
  output$statcast_summary = renderDataTable({
  
    dataset = datasetInput()
    dataset %>%
    # dplyr::rename(Pitch = pitch_name) %>%
    group_by(Pitch) %>%
    mutate(Swing = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked",
                                             "foul", "foul_tip", "hit_into_play",
                                             "hit_into_play_no_out", "hit_into_play_score"), 1, 0),
           Whiff = ifelse(description %in% c("swinging_strike",
                                             "swinging_strike_blocked", "foul_tip"), 1, 0),
           B = ifelse(type == "B", 1, 0),
           CS = ifelse(description %in% c("called_strike"), 1, 0),
           Watch = ifelse(description %in% c("called_strike"), 1, 0),
           wOBA = sum(woba_value, na.rm = T)/sum(woba_denom, na.rm = T),
           xwOBA = estimated_woba_using_speedangle,
           events = ifelse(events %in% c("hit_by_pitch","walk","sac_fly","sac_bunt"), 1, 0),
           barrel2 = ifelse(launch_angle <= 50 & launch_speed >= 
                              98 & launch_speed * 1.5 - launch_angle >= 117 & launch_speed + 
                              launch_angle >= 124, 1, 0)) %>%
    summarize(N = n(),
              `% RHB` = round(100 * (length(stand[stand == "R"])/N), 1),
              `% LHB` = round(100 * (length(stand[stand == "L"])/N), 1),
              MPH = round(mean(release_speed), 1), 
              `Whiff %` = round(100 * (sum(Whiff)/sum(Swing)), 1),
              `Swing %` = round(100 * (sum(Swing)/N), 1), 
              `Called Strike %` = round(100 * (sum(Watch)/N), 1),
              `Barrel %` = round(100 * (sum(barrel2, na.rm = T)/sum(type =="X")), 1),
              wOBA = round(mean(wOBA, na.rm = T), 3),
              EV = round(mean(launch_speed[type =="X"], na.rm = T), 1)) %>%
    arrange(desc(N))
  
  })

  output$total_pitches <- renderDataTable({
    
    dataset = datasetInput()
    
    pitcher.table = table(dataset$pitch_name)
    prop.table(pitcher.table) %>%
      as.data.frame() %>%
      dplyr::rename(prop = Freq) %>%
      mutate(Proportion = paste(round(100*prop, 1), "%")) %>%
      dplyr::rename(`Pitch Name` = Var1) %>%
      dplyr::select(-prop) 

  })
  
  output$pnext <- renderDataTable({
    
    dataset = datasetInput()
    
    pitcher.matrix = statetable.msm(pitch_name, ID, data = dataset)
    
    transition.matrix = round(t(t(pitcher.matrix) / rep(rowSums(pitcher.matrix), each = ncol(pitcher.matrix))),3)
    x = data.frame(matrix(as.numeric(100*transition.matrix), byrow = F, nrow = ncol(pitcher.matrix)),
                   row.names = dimnames(transition.matrix)[[1]])
    names(x) = row.names(x)
    x

  })
  
  output$heat_map <- renderPlot({
    
    
    zones = rbind(data.frame(x = c(1, 2, 2, 1, 1),
                             z = c(4, 4, 3, 3, 4),
                             zone = 1),
                  data.frame(x = c(2, 3, 3, 2, 2),
                             z = c(4, 4, 3, 3, 4),
                             zone = 2),
                  data.frame(x = c(3, 4, 4, 3, 3),
                             z = c(4, 4, 3, 3, 4),
                             zone = 3),
                  data.frame(x = c(1, 2, 2, 1, 1),
                             z = c(3, 3, 2, 2, 3),
                             zone = 4),
                  data.frame(x = c(2, 3, 3, 2, 2),
                             z = c(3, 3, 2, 2, 3),
                             zone = 5),
                  data.frame(x = c(3, 4, 4, 3, 3),
                             z = c(3, 3, 2, 2, 3),
                             zone = 6),
                  data.frame(x = c(1, 2, 2, 1, 1),
                             z = c(2, 2, 1, 1, 2),
                             zone = 7),
                  data.frame(x = c(2, 3, 3, 2, 2),
                             z = c(2, 2, 1, 1, 2),
                             zone = 8),
                  data.frame(x = c(3, 4, 4, 3, 3),
                             z = c(2, 2, 1, 1, 2),
                             zone = 9),
                  data.frame(x = c(0, 2.5, 2.5, 1, 1, 0, 0),
                             z = c(5, 5, 4, 4, 2.5, 2.5, 5),
                             zone = 11),
                  data.frame(x = c(2.5, 5, 5, 4, 4, 2.5, 2.5),
                             z = c(5, 5, 2.5, 2.5, 4, 4, 5),
                             zone = 12),
                  data.frame(x = c(0, 1, 1, 2.5, 2.5, 0, 0),
                             z = c(2.5, 2.5, 1, 1, 0, 0, 2.5),
                             zone = 13),
                  data.frame(x = c(2.5, 2.5, 4, 4, 5, 5, 2.5),
                             z = c(0, 1, 1, 2.5, 2.5, 0, 0),
                             zone = 14))
    zones$zone = as.integer(zones$zone)
    zone2 = data.frame(zone = c(1:9, 11:14),
                       plotx = c(1.5, 2.5, 3.5,
                                 1.5, 2.5, 3.5,
                                 1.5, 2.5, 3.5,
                                 0.5, 4.5, 0.5, 4.5),
                       plotz = c(3.5, 3.5, 3.5,
                                 2.5, 2.5, 2.5,
                                 1.5, 1.5, 1.5,
                                 4.5, 4.5, 0.5, 0.5))
    zone2$zone = as.integer(zone2$zone)
    
    dataset = datasetInput()
    dataset %>%
      group_by(zone) %>%
      tally() %>%
      right_join(., zones) %>%
      group_by(zone) %>%
      distinct(zone, n) %>%
      left_join(., zones) %>%
      left_join(., zone2) %>%
      ggplot(data = .) + 
      geom_polygon(aes(x, z, group = zone, fill = n), col = 1) +
      scale_fill_gradient(low = "blue", high = "red", guide = F) + 
      stat_unique(aes(x = plotx, y = plotz,label = n), geom = "text",
                  fontface = "bold", size = rel(5), col = "white") + 
      theme_void() + theme(panel.grid = element_blank())
  })
  
  
  output$Pitch_Locations <- renderPlot({
    
    
    SZ = data.frame(x = c(-.95,.95,.95,-.95,-.95), z = c(1.6,1.6,3.5,3.5,1.6))
    plate = data.frame(x = c(-0.95, -0.95, 0.95, 0.95, 0, -0.95),
                       z = c(-0.3, 0.1, 0.1, -0.3, -0.6, -0.3))
    LHB = data.frame(x = c(2.5, 2.5, 3, 3, 2.5), z = c(1, 4, 4, 1, 1))
    RHB = data.frame(x = c(-2.5, -2.5, -3, -3, -2.5), z = c(1, 4, 4, 1, 1))
    batter = data.frame(x = c(-2.5, 2.5), y = c(3))
    
    dataset = datasetInput()
    dataset %>%
      # dplyr::rename(Pitch = pitch_name) %>%
      group_by(Pitch) %>%
      ggplot(., aes(plate_x, plate_z, col = Pitch)) +
      theme_bw() + facet_wrap(~Pitch) +
      xlab("") + ylab("") +
      theme_void() +
      theme(strip.text = element_text(colour = 1, face = "bold", size = rel(1.2)),
            panel.grid = element_blank(),
            legend.position = "none") + 
      geom_polygon(data = plate, aes(x = x, y = z), col = 1, size = 1.05, fill = "grey") +
      geom_polygon(data = LHB, aes(x = x, y = z), col = "grey", fill = "grey", alpha = 0.5) +
      geom_polygon(data = RHB, aes(x = x, y = z), col = "grey", fill = "grey", alpha = 0.5) +
      geom_point(alpha = 0.3) +
      geom_path(data = SZ, aes(x = x, y = z), col = 1, size = 1.05) 
    
  })
  
  output$release_point <- renderPlot({
    
    ksFUN = function(data){
      
      H = Hpi(data[,c("release_pos_x","release_pos_z")], binned = TRUE) * 1
      
      fhata = kde(data[,c("release_pos_x","release_pos_z")], H = H, compute.cont = TRUE,
                  xmin = c(minX, minY), xmax = c(maxX, maxY))
      
      res95 = data.frame(HR = contourSizes(fhata, cont = 95, approx = TRUE))
      res50 = data.frame(HR = contourSizes(fhata, cont = 50, approx = TRUE))
      
      dimnames(fhata[['estimate']]) = list(fhata[["eval.points"]][[1]],
                                           fhata[["eval.points"]][[2]])
      dat = reshape2::melt(fhata[['estimate']])
      dat$breaks50 = fhata[["cont"]]["50%"]
      dat$breaks95 = fhata[["cont"]]["5%"]
      dat$HR95 = round(res95$HR, 2)
      dat$CA50 = round(res50$HR, 2)
      
      return(dat)
    }

    dataset = datasetInput()
    
    minX = min(dataset$release_pos_x, na.rm = T); maxX = max(dataset$release_pos_x, na.rm = T)
    minY = min(dataset$release_pos_z, na.rm = T); maxY = max(dataset$release_pos_z, na.rm = T)
    
    xx = dataset %>%
      group_by(player_name, Pitch) %>%
      do(as.data.frame(ksFUN(.)))
    
    breaks = xx %>%
      group_by(player_name, Pitch) %>%
      summarize(breaks95 = mean(breaks95),
                breaks50 = mean(breaks50))
    
    
    pl <- ggplot(data = xx, aes(x = Var1, y = Var2, fill = Pitch)) + facet_wrap(~player_name, scales = "free") +
      geom_point(data = dataset[dataset$Pitch != "",],
                 aes(x = release_pos_x, y = release_pos_z, col = Pitch), size = rel(0.3)) + 
      xlab("Horizontal Release Point") + ylab("Vertical Release Point") + 
      theme(strip.background = element_rect(fill = "lightblue"),
            strip.text = element_text(face = "bold", size = rel(1.3)))
    groups <- unique(xx$Pitch)
    player_name <- unique(xx$player_name)
    
    # loop and add for each group
    for(i in groups){
      for(j in player_name){
        pl <- pl + stat_contour(data = xx[xx$Pitch == i & xx$player_name == j,], aes(z = value, col = Pitch),
                                breaks = breaks[breaks$Pitch == i & breaks$player_name == j,]$breaks95, 
                                alpha = 0.3, geom = "polygon") + theme_bw() + theme(panel.grid = element_blank())
      }
    }
    pl
    
  })
  
  output$pitch_velo <- renderPlot({

    dataset = datasetInput()
    dataset %>%
      dplyr::mutate(Name = paste(player_name, "Pitches", game_year)) %>%
      dplyr::rename(Pitch = pitch_name) %>%
      group_by(Pitch) %>%
      arrange(game_date, at_bat_number, pitch_number) %>%
      dplyr::mutate(counter = row_number(Pitch)) %>%
      ggplot(., aes(counter, release_speed, col = Pitch)) +
      geom_path(alpha = 0.5, size = rel(1.1)) +
      xlab("Number of Pitches Thrown") + ylab("Release Speed (mph)") +
      stat_smooth(method = "loess", se = F, alpha = 1) +
      theme_bw() + facet_wrap(~Name) +
      theme(strip.background = element_rect(fill = "lightblue"),
            strip.text = element_text(colour = 1, face = "bold", size = rel(1.2)),
            legend.position = "top",
            panel.grid = element_blank(),
            legend.title = element_blank())

  })
}


# Run the application 
shinyApp(ui = ui, server = server)
