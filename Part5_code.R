library(dplyr)
library(tidyverse)
library(ggplot2)
library(wordcloud)
library(scales)
library(RColorBrewer)
olympic <- read_csv("C:/Temp/athlete_events.csv")
as_tibble(olympic)

head(olympic)
olympic$Season%>%
  summary

## 1. 하계올림픽, 동계올림픽 규모 비교

# a) 연도에 따른 메달 수 하계/동계 비교

olympic%>%
  filter(!is.na(Medal))%>%
  group_by(Year,Season)%>%
  summarise(medal_count = n())%>%
  ggplot(aes(x=Year,y=medal_count,fill=Season))+geom_histogram(binwidth=0.5,stat='identity')+xlab("연도")+ylab("메달수")


# b) 연도에 따른 종목 수 하계/동계 비교
olympic%>%
  group_by(Year,Season)%>%
  summarise(Sport=n_distinct(Sport))%>%
  ggplot(aes(x=Year,y=Sport))+
  geom_point(aes(color=Season))+geom_line(aes(color=Season))


olympic%>%
  group_by(Year,Season)%>%
  summarise(Event=n_distinct(Event))%>%
  ggplot(aes(x=Year,y=Event))+
  geom_point(aes(color=Season))+geom_line(aes(color=Season))

## 2. 하계올림픽/동계올림픽 메달수와 나라의 기후간의 연관성


olympicTot<-olympic%>%
  filter(Medal!=0)%>%
  group_by(NOC)%>%
  summarise(medal_tot=n(),na.rm=TRUE)%>%
  select(-na.rm)%>%
  arrange(desc(medal_tot))%>%
  filter(rank(desc(medal_tot))<=10)

olympicMedal<-left_join(olympicTot,olympic,by="NOC")%>%
  group_by(NOC,medal_tot,Medal)%>%
  summarise(medal_sum=n())%>%
  spread(key=Medal,value=medal_sum)%>%
  gather('Bronze':'Silver',key="Medal",value="medal_sum")%>%
  arrange(NOC,Medal)
ggplot(olympicMedal,aes(x=reorder(NOC,-medal_tot,sum),medal_tot,fill=ordered(Medal,levels=c("Bronze","Silver","Gold"))))+geom_bar(stat="identity")+xlab("NOC")+ylab("Total Medal")+labs(fill="Medal")+scale_fill_manual(values=c("#ad8a56","#d7d7d7","#fee101"))

# 하계올림픽 메달 획득 수 상위 10개국

summer <- olympic %>%
  filter(Season=='Summer',!is.na(Medal))
summer %>%
  group_by(NOC)%>%
  summarise(medal_count=n())%>%
  filter(rank(desc(medal_count))<=10)%>%
  arrange(desc(medal_count))%>%
  ggplot(aes(x=reorder(NOC,-medal_count,sum),y=medal_count))+geom_bar(stat="identity",fill="#F8766D",show.legend=FALSE)+xlab("나라(NOC)")+ylab("메달수")

# 동계올림픽 메달 획득 수 상위 10개국
winter <- olympic %>%
  filter(Season=='Winter',!is.na(Medal))
winter %>%
  group_by(NOC)%>%
  summarise(medal_count=n())%>%
  filter(rank(desc(medal_count))<=10)%>%
  arrange(desc(medal_count))%>%
  ggplot(aes(x=reorder(NOC,-medal_count),y=medal_count))+geom_bar(stat="identity",fill="#00BFC4")+xlab("나라(NOC)")+ylab("메달수")

# 하계올림픽/동계올림픽 국가별 메달 수 워드클라우드

summer_country_medals <- summer%>%
  group_by(NOC) %>%
  summarise(medals = n())
summer_country_medals <- na.omit(summer_country_medals)
wordcloud(summer_country_medals$NOC,summer_country_medals$medals,colors=brewer.pal(6,"Reds"),random.order=FALSE)
winter_country_medals <- winter%>%
  group_by(NOC) %>%
  summarise(medals = n())
winter_country_medals <- na.omit(winter_country_medals)
wordcloud(winter_country_medals$NOC,winter_country_medals$medals,colors=brewer.pal(6,"Blues"),random.order=FALSE)

# World Maps (set up)
library(plyr)
library(rworldmap)
library(repr)
options(repr.plot.width=6, repr.plot.height=6)
world <- map_data(map="world")
world <- world[world$region != "Antarctica",] 
country_code <- read_csv('c:/Users/KimMinyoung/Documents/country_codes.txt')
country_code <- country_code %>% select(Country, IOC, ISO)
olympic2 <- olympic %>% left_join(country_code, by = c('NOC' = 'IOC'))
ISO_Medal<-olympic2%>%
  filter(!is.na(Medal))%>%
  group_by(ISO)%>%
  dplyr::summarise(medal_count=n())

ISO_Medal_winter<-olympic2%>%
  filter(Season=="Winter")%>%
  filter(!is.na(Medal))%>%
  group_by(ISO)%>%
  dplyr::summarise(medal_winter_count = n())

ISO_Medal_summer<-olympic2%>%
  filter(Season=="Summer")%>%
  filter(!is.na(Medal))%>%
  group_by(ISO)%>%
  dplyr::summarise(medal_summer_count=n())

winter_prop2 <- ISO_Medal%>%left_join(ISO_Medal_winter,by="ISO")
winter_prop2[is.na(winter_prop2)]<-0

winter_prop2<-winter_prop2%>%
  mutate(w_prop=medal_winter_count/medal_count*100)

summer_prop2 <- ISO_Medal%>%left_join(ISO_Medal_summer,by="ISO")
summer_prop2[is.na(summer_prop2)]<-0

summer_prop2<-summer_prop2%>%
  mutate(s_prop=medal_summer_count/medal_count*100)

# 하계올림픽 나라별 메달 수 지도

options(repr.plot.width=25, repr.plot.height=25)

world <- world[world$region != "Antarctica",] 
colourPalette <- RColorBrewer::brewer.pal(6,'Reds')
sPDF1 <- joinCountryData2Map(summer_prop2,joinCode = "ISO3",nameJoinColumn = "ISO")

mapCountryData(sPDF1,nameColumnToPlot='medal_summer_count',colourPalette=colourPalette, catMethod ='fixedWidth',numCats = length(table(sPDF1$medal_summer_count)))

# 동계올림픽 국가별 메달 수 지도
options(repr.plot.width=25, repr.plot.height=25)

world <- world[world$region != "Antarctica",] 
colourPalette <- RColorBrewer::brewer.pal(6,'Blues')
sPDF2 <- joinCountryData2Map(winter_prop2,joinCode = "ISO3",nameJoinColumn = "ISO")
mapCountryData(sPDF2,nameColumnToPlot='medal_winter_count',colourPalette=colourPalette, catMethod ='fixedWidth',numCats = length(table(sPDF2$medal_winter_count)))

# 나라별 하계올림픽, 동계올림픽 메달 비율
options(repr.plot.width=25, repr.plot.height=25)

world <- world[world$region != "Antarctica",] 
colourPalette <- RColorBrewer::brewer.pal(6,'RdYlBu')
sPDF3 <- joinCountryData2Map(winter_prop2,joinCode = "ISO3",nameJoinColumn = "ISO")
mapCountryData(sPDF3,nameColumnToPlot='w_prop',colourPalette=colourPalette, catMethod ='fixedWidth',numCats = length(table(sPDF3$w_prop)))


# 동계올림픽 나라별 메달수 자세히 살펴보기

# 스키 종목(설상 종목)
winter_ski<-olympic%>%
  filter(!is.na(Medal),Season=="Winter",Sport==c("Alpine Skiing","Cross Country Skiing","Freestyle Skiing"))%>%
  group_by(NOC)%>%
  dplyr::summarise(medal_ski_count = n())
winter_ski%>%
  filter(rank(desc(medal_ski_count))<=10)%>%
  ggplot(aes(x=reorder(NOC,-medal_ski_count),y=medal_ski_count))+geom_bar(stat="identity",fill="#00BFC4")+xlab("나라(NOC)")+ylab("메달수")

colourPalette <- RColorBrewer::brewer.pal(6,'Blues')
sPDF4 <- joinCountryData2Map(winter_ski,joinCode = "ISO3",nameJoinColumn = "NOC")
mapCountryData(sPDF4,nameColumnToPlot='medal_ski_count',colourPalette=colourPalette, catMethod ='fixedWidth',numCats = length(table(sPDF4$medal_ski_count)))

# 쇼트트랙 종목(실내 종목)

winter_st<-olympic%>%
  filter(!is.na(Medal),Season=="Winter",Sport==c("Short Track Speed Skating"))%>%
  group_by(NOC)%>%
  dplyr::summarise(medal_st_count = n())
winter_st%>%
  filter(rank(desc(medal_st_count))<=10)%>%
  ggplot(aes(x=reorder(NOC,-medal_st_count),y=medal_st_count))+geom_bar(stat="identity",fill="#00BA38")+xlab("나라(NOC)")+ylab("메달수")

colourPalette <- RColorBrewer::brewer.pal(6,'Greens')
sPDF5<- joinCountryData2Map(winter_st,joinCode = "ISO3",nameJoinColumn = "NOC")
mapCountryData(sPDF5,nameColumnToPlot='medal_st_count',mapTitle="Short Track",colourPalette=colourPalette, catMethod ='fixedWidth',numCats = length(table(sPDF5$medal_st_count)))