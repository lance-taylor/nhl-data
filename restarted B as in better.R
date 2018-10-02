library(jsonlite)
library(tidyr)
library(lubridate)
library(dplyr)
library(stringr)
library(rvest)

season<-20172018


X<-fromJSON(paste0("http://live.nhl.com/GameData/SeasonSchedule-", season, ".json"))
X1<-X%>%filter(str_sub(id, 5, 6)=="02") #regular season
X2<-X%>%filter(str_sub(id, 5, 6)=="03") #playoffs

gamedata<-list()
system.time(
for(gameidnumber in 501:525){
  
Y<-fromJSON(
  paste("http://statsapi.web.nhl.com/api/v1/game/", X1$id[gameidnumber], "/feed/live", 
        sep=""))

if(length(Y$liveData$plays$allPlays$coordinates)>0){
Y1<-bind_cols(Y$liveData$plays$allPlays$coordinates,
Y$liveData$plays$allPlays$team,
Y$liveData$plays$allPlays$about%>%select(-goals),
Y$liveData$plays$allPlays$about$goals,
Y$liveData$plays$allPlays$result%>%select(-strength))

cow<-list()
for(i in 1:nrow(Y1)){
turd<-length(Y$liveData$plays$allPlays$players[[i]]$player$fullName)
turd<-ifelse(turd==0, 4, turd)
cow1<-data.frame(player.name=Y$liveData$plays$allPlays$players[[i]]$player$fullName)%>%t
cow2<-data.frame(player.name=Y$liveData$plays$allPlays$players[[i]]$playerType)%>%t
if(nrow(cow1)==0){
  cow1<-data.frame(rep(NA, turd))%>%t
  cow2<-data.frame(rep(NA, turd))%>%t
}
colnames(cow1)<-paste0("player.name", 1:turd)
rownames(cow1)<-NULL
colnames(cow2)<-paste0("player.type", 1:turd)
rownames(cow2)<-NULL

cow[[i]]<-cbind(cow1, cow2)%>%as.data.frame
}
cow<-bind_rows(cow)

Y2<-bind_cols(Y1, cow)%>%
  mutate(home.team=Y$gameData$teams$home$triCode,
         away.team=Y$gameData$teams$away$triCode,
         event.team.home=ifelse(home.team==triCode, 1, 0),
         event.team.away=ifelse(away.team==triCode, 1, 0),
         event.team.home=ifelse(eventTypeId=="BLOCKED_SHOT", 
                                1-event.team.home, event.team.home),
         event.team.away=ifelse(eventTypeId=="BLOCKED_SHOT", 
                                1-event.team.away, event.team.away))
Y2$emptyNet<-NULL
 cow<-read_html(paste0("http://www.nhl.com/scores/htmlreports/", season, "/PL", 
                       str_sub(X1$id[gameidnumber], 5, 10), ".HTM"))%>%
    html_nodes(".bborder")%>%html_text()
  Z<-matrix(cow, nrow=length(cow)/8, ncol=8, byrow=TRUE)%>%as.data.frame
  
  colnames(Z)<-c("eventIdx", "period", "strength", "time.elapsed", "event", "desc.", 
               "away.players", "home.players")
Z<-Z%>%mutate(event=str_replace_all(event, " ", ""))%>%
  filter(eventIdx%in%as.character(0:999), 
         !event%in%c("PGSTR", "PGEND", "ANTHEM", "PEND", "PSTR", "GEND", "GSTR"))%>%
  mutate(eventIdx=as.numeric(as.character(eventIdx)), eventIdx=eventIdx+2*(as.integer(period)-1))

Y2<-Y2%>%filter(!eventTypeId%in%c("PERIOD_END", "GAME_END", "PERIOD_OFFICIAL", "PERIOD_START",
                                  "GAME_SCHEDULED", "PERIOD_READY"))%>%
  mutate(eventIdx=eventIdx+2)%>%
  left_join(Z, by="eventIdx")%>%
  select(-name, -link, -eventIdx, -eventId, -period.y, -periodType, -ordinalNum,
         -dateTime, -event.x, -eventCode, -eventTypeId, -description, -gameWinningGoal,
         -period.y, -time.elapsed, -desc.)%>%
  filter(event.y!="STOP", period.x!=5)%>%
  mutate(netycoord=0, homenetxcoord=-89*199/200, awaynetxcoord=89*199/200,
         gamexcoord=2*x*(-1*(period.x==2))+x*(period.x==1|3),
         gameycoord=y*(period.x==1|3)+(period.x==2)*-2*y,
         xhomedistfromawaynet=(gamexcoord-awaynetxcoord)*event.team.home,
         xawaydistfromhomenet=(gamexcoord-homenetxcoord)*event.team.away,
         xdistfromoppnet=abs(xawaydistfromhomenet+xhomedistfromawaynet),
         ydistfromoppnet=gameycoord-netycoord,
         distfromoppnet=sqrt(xdistfromoppnet^2+ydistfromoppnet^2),
         oa=ydistfromoppnet/xdistfromoppnet,
         angle=abs(atan(oa)*180/pi), gameID=X1$id[gameidnumber])%>%
  select(-netycoord, -homenetxcoord, -awaynetxcoord, -gamexcoord,
         -gameycoord, -xdistfromoppnet, -xawaydistfromhomenet, -xhomedistfromawaynet,
         -ydistfromoppnet, -oa, -home.team, -away.team, -x, -y, -id, -periodTimeRemaining,
         -penaltySeverity)%>%
  mutate(away.players=str_replace_all(away.players, "[^((0-9)|G)]", "cow")%>%
           str_replace_all("[cow]+", " "),
         home.players=str_replace_all(home.players, "[^((0-9)|G)]", "cow")%>%
           str_replace_all("[cow]+", " "),
         away.on.ice=str_count(away.players, "[0-9]+ ")-str_count(away.players, "G"),
         home.on.ice=str_count(home.players, "[0-9]+ ")-str_count(home.players, "G"),
         away.goalie.in.net=str_count(away.players, "G"),
         home.goalie.in.net=str_count(home.players, "G"))%>%
  rename(team=triCode, period=period.x, period.time=periodTime, away.goals=away, home.goals=home,
         details=secondaryType, pims=penaltyMinutes, event=event.y, 
         dist=distfromoppnet)

gamedata[[gameidnumber]]<-Y2[,c("gameID", "away.goals", "home.goals", "period", "period.time", "team",
                                "event.team.away", "event.team.home", "event", "dist", "angle", 
                                "strength", "details", "pims", "player.name1", "player.type1", 
                                "player.name2", "player.type2", "player.name3", "player.type3", 
                                "player.name4", "player.type4", "away.on.ice", "home.on.ice", 
                                "away.goalie.in.net", "home.goalie.in.net", "away.players", 
                                "home.players")]
}
}
)

datacowA<-bind_rows(gamedata)

redocow<-datacowA%>%group_by(gameID)%>%
  filter(event%in%c("SHOT", "BLOCK", "GOAL", "MISS"))%>%
  summarise(distance=mean(dist, na.rm=TRUE))%>%
  filter(distance>89)

redocow<-redocow$gameID

datacowB<-datacowA%>%filter(!gameID%in%redocow)

gamedata<-list()
system.time(
  for(gameidnumber in 1:length(redocow)){
    
    Y<-fromJSON(
      paste("http://statsapi.web.nhl.com/api/v1/game/", redocow[gameidnumber], "/feed/live", 
            sep=""))
    
    if(length(Y$liveData$plays$allPlays$coordinates)>0){
      Y1<-bind_cols(Y$liveData$plays$allPlays$coordinates,
                    Y$liveData$plays$allPlays$team,
                    Y$liveData$plays$allPlays$about%>%select(-goals),
                    Y$liveData$plays$allPlays$about$goals,
                    Y$liveData$plays$allPlays$result%>%select(-strength))
      
      cow<-list()
      for(i in 1:nrow(Y1)){
        turd<-length(Y$liveData$plays$allPlays$players[[i]]$player$fullName)
        turd<-ifelse(turd==0, 4, turd)
        cow1<-data.frame(player.name=Y$liveData$plays$allPlays$players[[i]]$player$fullName)%>%t
        cow2<-data.frame(player.name=Y$liveData$plays$allPlays$players[[i]]$playerType)%>%t
        if(nrow(cow1)==0){
          cow1<-data.frame(rep(NA, turd))%>%t
          cow2<-data.frame(rep(NA, turd))%>%t
        }
        colnames(cow1)<-paste0("player.name", 1:turd)
        rownames(cow1)<-NULL
        colnames(cow2)<-paste0("player.type", 1:turd)
        rownames(cow2)<-NULL
        
        cow[[i]]<-cbind(cow1, cow2)%>%as.data.frame
      }
      cow<-bind_rows(cow)
      
      Y2<-bind_cols(Y1, cow)%>%
        mutate(home.team=Y$gameData$teams$home$triCode,
               away.team=Y$gameData$teams$away$triCode,
               event.team.home=ifelse(home.team==triCode, 1, 0),
               event.team.away=ifelse(away.team==triCode, 1, 0),
               event.team.home=ifelse(eventTypeId=="BLOCKED_SHOT", 
                                      1-event.team.home, event.team.home),
               event.team.away=ifelse(eventTypeId=="BLOCKED_SHOT", 
                                      1-event.team.away, event.team.away))
      Y2$emptyNet<-NULL
      cow<-read_html(paste0("http://www.nhl.com/scores/htmlreports/", season, "/PL", 
                            str_sub(redocow[gameidnumber], 5, 10), ".HTM"))%>%
        html_nodes(".bborder")%>%html_text()
      Z<-matrix(cow, nrow=length(cow)/8, ncol=8, byrow=TRUE)%>%as.data.frame
      
      colnames(Z)<-c("eventIdx", "period", "strength", "time.elapsed", "event", "desc.", 
                     "away.players", "home.players")
      Z<-Z%>%mutate(event=str_replace_all(event, " ", ""))%>%
        filter(eventIdx%in%as.character(0:999), 
               !event%in%c("PGSTR", "PGEND", "ANTHEM", "PEND", "PSTR", "GEND", "GSTR"))%>%
        mutate(eventIdx=as.numeric(as.character(eventIdx)), eventIdx=eventIdx+2*(as.integer(period)-1))
      
      Y2<-Y2%>%filter(!eventTypeId%in%c("PERIOD_END", "GAME_END", "PERIOD_OFFICIAL", "PERIOD_START",
                                        "GAME_SCHEDULED", "PERIOD_READY"))%>%
        mutate(eventIdx=eventIdx+2)%>%
        left_join(Z, by="eventIdx")%>%
        select(-name, -link, -eventIdx, -eventId, -period.y, -periodType, -ordinalNum,
               -dateTime, -event.x, -eventCode, -eventTypeId, -description, -gameWinningGoal,
               -period.y, -time.elapsed, -desc.)%>%
        filter(event.y!="STOP", period.x!=5)%>%
        mutate(netycoord=0, homenetxcoord=89*199/200, awaynetxcoord=-89*199/200,
               gamexcoord=2*x*(-1*(period.x==2))+x*(period.x==1|3),
               gameycoord=y*(period.x==1|3)+(period.x==2)*-2*y,
               xhomedistfromawaynet=(gamexcoord-awaynetxcoord)*event.team.home,
               xawaydistfromhomenet=(gamexcoord-homenetxcoord)*event.team.away,
               xdistfromoppnet=abs(xawaydistfromhomenet+xhomedistfromawaynet),
               ydistfromoppnet=gameycoord-netycoord,
               distfromoppnet=sqrt(xdistfromoppnet^2+ydistfromoppnet^2),
               oa=ydistfromoppnet/xdistfromoppnet,
               angle=abs(atan(oa)*180/pi), gameID=redocow[gameidnumber])%>%
        select(-netycoord, -homenetxcoord, -awaynetxcoord, -gamexcoord,
               -gameycoord, -xdistfromoppnet, -xawaydistfromhomenet, -xhomedistfromawaynet,
               -ydistfromoppnet, -oa, -home.team, -away.team, -x, -y, -id, -periodTimeRemaining,
               -penaltySeverity)%>%
        mutate(away.players=str_replace_all(away.players, "[^((0-9)|G)]", "cow")%>%
                 str_replace_all("[cow]+", " "),
               home.players=str_replace_all(home.players, "[^((0-9)|G)]", "cow")%>%
                 str_replace_all("[cow]+", " "),
               away.on.ice=str_count(away.players, "[0-9]+ ")-str_count(away.players, "G"),
               home.on.ice=str_count(home.players, "[0-9]+ ")-str_count(home.players, "G"),
               away.goalie.in.net=str_count(away.players, "G"),
               home.goalie.in.net=str_count(home.players, "G"))%>%
        rename(team=triCode, period=period.x, period.time=periodTime, away.goals=away, home.goals=home,
               details=secondaryType, pims=penaltyMinutes, event=event.y, 
               dist=distfromoppnet)
      
      gamedata[[gameidnumber]]<-Y2[,c("gameID", "away.goals", "home.goals", "period", "period.time", "team",
                                      "event.team.away", "event.team.home", "event", "dist", "angle", 
                                      "strength", "details", "pims", "player.name1", "player.type1", 
                                      "player.name2", "player.type2", "player.name3", "player.type3", 
                                      "player.name4", "player.type4", "away.on.ice", "home.on.ice", 
                                      "away.goalie.in.net", "home.goalie.in.net", "away.players", 
                                      "home.players")]
    }
  }
)

datacowC<-bind_rows(gamedata)

datacow<-bind_rows(datacowB, datacowC)

datacow%>%group_by(gameID)%>%
  filter(event%in%c("SHOT", "GOAL", "MISS"))%>%
  summarise(distance=mean(dist, na.rm=TRUE))%>%View

#data.table::fwrite(datacow, "C:\\Users\\Lance\\Dropbox\\hockey\\me\\shots.csv")

##also get list of all player IDs, and the numbers they wore for each game
#and game stats to create database
#Y$gameData$players$ID8474709
#Y$liveData$boxscore$teams$away$players$ID8476941$stats
##as well as a list of scratched players by game to rate coaches based off given up expected points
#Y$liveData$boxscore$teams$away$scratches






