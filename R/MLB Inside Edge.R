
rm(list = ls())

require(dplyr)
require(httr)

api_key = scan("~/PitcherList/MLB Inside Edge/api_key.txt", character(), quote = "")

Inside_Edge = function(GHUID, api_key = api_key){
  
  url = paste0("http://gamedata.inside-edge.com/api/ie/game/", GHUID,
               "/?apikey=", api_key)
  
  x = jsonlite::fromJSON(url, simplifyVector=F)
  xx = lapply(seq_along(x), FUN = function(i) names(x[[i]]))
  pp = unlist(which(xx == "PitchPush"))
  dat1 = lapply(pp, FUN = function(i) as.data.frame(do.call("cbind", x[[i]]$PitchPush)))
  
  df = purrr::map_df(dat1, dplyr::bind_rows)
  return(df)
}

IDs = read.table("~/PitcherList/MLB Inside Edge/Inside Edge Game IDs.txt", stringsAsFactors = FALSE, header = T)
# IDs = head(IDs, n=15)


# future::plan("multiprocess")
# tictoc::tic()
# dat = IDs$GHUID %>% 
#   furrr::future_map(~Inside_Edge(.x, api_key = api_key))
# tictoc::toc()


require(parallel)
cl <- makeCluster(3)
clusterEvalQ(cl, { c(library(xml2), library(jsonlite), library(dplyr)) })  # you need to export packages as well
clusterExport(cl, "Inside_Edge")  # each worker is a new environment, you will need to export variables/functions to
clusterExport(cl, "api_key")  # each worker is a new environment, you will need to export variables/functions to
# tictoc::tic()
DATA <- parallel::parLapply(cl, IDs$GHUID,  fun = function(i) Inside_Edge(i, api_key = api_key))   
stopCluster(cl)
# tictoc::toc()


DATA = bind_rows(DATA)

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

DATA %>% 
  mutate(Velo = as.numeric(Velo)) %>% 
  group_by(PitcherID, PitcherName, PitchType) %>% 
  summarize(Velo = mean(Velo, na.rm = TRUE),
            N = n()) %>% 
  group_by(PitcherID, PitcherName) %>% 
  mutate(Usage = N/sum(N)) %>% 
  mutate(Usage_perc = percent(Usage)) %>% 
  write.csv(., "~/PitcherList/MLB Inside Edge/July 2019 MLB Inside Edge Example.csv", row.names = F)


######
#
### End here
#
#####
    
                            
                            
library(xml2)
require(dplyr)
require(ggplot2)
require(purrr)
require(baseballr)

# Need to go to this link and copy/paste to get the xml example data
# http://gamedata.inside-edge.com/api/demopostgame/game/2019/07/05/anamlb-houmlb-1/

test = read_xml("C:/Users/charlesc/Desktop/Test.xml")

nodes = test %>% xml_find_all("//PitchPush") 

dat = map_df(nodes, function(x) {
  kids = xml_children(x)
  setNames(as.list(type.convert(xml_text(kids))), xml_name(kids))
}) %>% 
  as.data.frame


IDs = dat %>% 
  select(HitterName, HitterMlbamId, HitterID) %>% 
  distinct()

Pitchers = dat %>% 
  select(PitcherName, PitcherMlbamId) %>% 
  distinct()

head(dat)

unique(dat$PitchResult)

# Calculate CSW
dat %>% 
  group_by(PitcherName, PitcherMlbamId) %>% 
  dplyr::filter(n() >= 60) %>% 
  mutate(CSW = ifelse(PitchResult %in% c("Called Strike", "Swing Miss", "Foul Tip"), 1, 0)) %>% 
  summarize(CSW = sum(CSW),
            N = n()) %>% 
  mutate('CSW %' = 100 * (sum(CSW)/N))



nodes = test %>% xml_find_all("//ActionPush") 

bat = map_df(nodes, function(x) {
  kids = xml_children(x)
  setNames(as.list(type.convert(xml_text(kids))), xml_name(kids))
}) %>% 
  as.data.frame

bat %>% 
  left_join(., IDs) %>% 
  group_by(HitterMlbamId, HitterName) %>% 
  summarize(RBI = sum(as.numeric(RBI)))


bat %>% 
  dplyr::filter(HitterMlbamId == 488726)

dat %>% 
  dplyr::filter(HitterMlbamId == 488726)

EventCodes = data.frame(IECode = c("A","Appeal","B.Int","Balk","BB",
                                   "BHA","BSA", "C.PKO", "CI", "CS",
                                   "D","DI","E","FC","FO", "FODP", "GIDP",
                                   "GO", "HBP", "HR", "HRA", "IBB", 
                                   "K", "KC", "KS", "LO", "LODP",
                                   "OB", "OP", "PB", "PCS", "PI", "Pit.Out",
                                   "PKO", "PkSB", "PO", "RH", "ROE", "S",
                                   "SB", "SBA", "SF", "SH" ,"T", "TP",
                                   "WP", "X"))

boxscore = bat %>% 
  # head %>% 
  mutate(Date = substr(GHUID, 1, 10),
         Date = as.Date(Date, format = "%Y/%m/%d"),
         Date = format(Date, format = "%d-%b-%Y")) %>% 
  # dplyr::filter(HitterMlbamId == 493329) %>% 
  left_join(., IDs) %>% 
  group_by(HitterMlbamId, HitterName) %>% 
  summarize(PA = max(PA_ThisGame),
            X1B = sum(PrimaryEvent == "S"),
            X2B = sum(PrimaryEvent == "D"),
            X3B = sum(PrimaryEvent == "T"),
            HR = sum(PrimaryEvent == "HR"),
            RBI = sum(as.numeric(RBI)),
            H = X1B + X2B + X3B + HR,
            uBB =  sum(PrimaryEvent == "BB"),
            IBB = sum(PrimaryEvent == "IBB"),
            HBP = sum(PrimaryEvent == "HBP"),
            SF = sum(PrimaryEvent %in% c("SF","SH")),
            SO = sum(PrimaryEvent %in% c("K","KC","KS"))) %>% 
  # dplyr::select(-Single) %>% 
  dplyr::select(HitterMlbamId, HitterName, everything())



nodes = test %>% xml_find_all("//RunsPush") 

runs = map_df(nodes, function(x) {
  kids = xml_children(x)
  setNames(as.list(type.convert(xml_text(kids))), xml_name(kids))
}) %>% 
  as.data.frame

bat %>% 
  dplyr::filter(ROB1 == "10354"|ROB2 == "10354"|ROB3 == "10354")

boxscore = runs %>% 
  group_by(RunnerId) %>% 
  tally(name = "R") %>% 
  left_join(., IDs, by = c("RunnerId" = "HitterID")) %>% 
  left_join(boxscore, .) %>%
  mutate(R = ifelse(is.na(R), 0 , R)) %>% 
  dplyr::select(-RunnerId)
  # dplyr::select(HitterMlbamId, HitterName, PA, H, Double, Triple, HR,
  #               R, RBI, BB, IBB, HBP, SF) 


runs %>% 
  group_by(PitcherMlbamId) %>% 
  summarize(Runs = n(),
            ER = length(UnearnedRun[UnearnedRun == "false"])) %>% 
  left_join(Pitchers, .) %>% 
  mutate(Runs = ifelse(is.na(Runs), 0 , Runs),
         ER = ifelse(is.na(ER), 0 , ER)) 
  

nodes = test %>% xml_find_all("//BaseRunnersPush") 

baserunning = map_df(nodes, function(x) {
  kids = xml_children(x)
  setNames(as.list(type.convert(xml_text(kids))), xml_name(kids))
}) %>% 
  as.data.frame


CS = baserunning %>% 
  dplyr::filter(CSIDMlbamId != "") %>% 
  group_by(CSIDMlbamId) %>% 
  tally %>% 
  dplyr::rename(CS = n,
                HitterMlbamId = CSIDMlbamId)

SB = baserunning %>% 
  dplyr::filter(SBIDMlbamId != "") %>% 
  group_by(SBIDMlbamId) %>% 
  tally %>% 
  dplyr::rename(SB = n,
                HitterMlbamId = SBIDMlbamId)

boxscore1 = boxscore %>% 
  left_join(., SB) %>% 
  left_join(., CS) %>% 
  mutate(SB = ifelse(is.na(SB), 0 , SB),
         CS = ifelse(is.na(CS), 0 , CS),
         PA = as.integer(PA),
         AB = (PA - (uBB + IBB + HBP + SF)),
         Avg = H/AB,
         OBP = (H+uBB+HBP)/(AB+uBB+HBP+SF)) %>% 
  mutate(season = "2019") 
  
baseballr::woba_plus(boxscore1) %>% 
  mutate(Avg = format(Avg, nsmall = 3, digits = 3),
         OBP = format(OBP, digits = 3, nsmall = 3),
         wOBA = format(wOBA, nsmall = 3, digits = 3),
         'K%' = format(100*(SO/PA), nsmall = 1, digits = 1),
         'BB%' = format(100*(uBB/PA), nsmall = 1, digits = 1))
