##Setup the Bench


library("dplyr")
library("data.table")
library("ggplot2")
library("choroplethr")

##Read the data

reRead <- 1
##read data and save it as RData to save time nect time:
if(reRead==1){
  colsToKeep <- c("PINCP", "SCHL", "ESR", "ST")
  popDataA <- fread("ss13pusa.csv", select=colsToKeep )  
  popDataB <- fread("ss13pusb.csv", select=colsToKeep )
  populData <- rbind(popDataA, popDataB)
  rm(popDataA, popDataB)
  save(populData, file="populData.RData")
}else{
  load("populData.RData")
} 

##Data Manipulation

populData <- tbl_df(populData) 
ds <-  populData %>%  
  na.omit() %>%
  filter(SCHL %in%  c(21,22,24)) %>%
  group_by(SCHL) 
#rm(populData)

degreeCode = "SCHL,DegLevel
21,Bachelor 
22,Masters
24,Doctorate"
degreCodes <- fread(degreeCode)  

##How many are they?

#Visualize   them here
degreeHolders <-  summarise(ds, count=n())
degreeHolders <- left_join(degreeHolders , degreCodes, by.x=c("SCHL"))
Degrees <- factor(degreeHolders$DegLevel, levels = unique(degreeHolders$DegLevel))

ggplot(degreeHolders, aes(x= Degrees , y=degreeHolders$count, fill= Degrees)) +                        
  geom_bar(stat="identity") + scale_fill_hue(l=40) +
  ylab("No of People") + 
  xlab("Degree") + ggtitle("Comparing Degrees Holders in the US") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.background = element_rect(fill = 'white' ))



#Filter unemployed, group them by degree and calculate rates:
jobLess <- ds %>%
  filter(ESR==3)%>% #3 indicates  Unemployed
  group_by(SCHL) %>% 
  summarise(count=n())%>%
  mutate(Percet = count/degreeHolders$count*100)


ggplot(jobLess, aes(x= Degrees , y=jobLess$Percet, fill= Degrees)) +                        
  geom_bar(stat="identity") + scale_fill_hue(l=80) +
  ylab("Percent %") + 
  xlab("Degree") + ggtitle("Percentages of Unemployed Degree  Holders")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.background = element_rect(fill = 'white' ))

stateTotalBSc  <- ds%>%
  filter(SCHL==21)%>%
  group_by(ST)%>%
  summarise(count = n())

jobLessBSc  <- ds%>%
  filter(SCHL==21, ESR==3)%>%
  group_by(ST)%>%
  summarise(count = n())


stateCodeCSV = "ST,region
001,alabama
002,alaska
004,arizona
005,arkansas
006,california
008,colorado
009,connecticut
010,delaware
011,district of columbia
012,florida
013,georgia
015,hawaii
016,idaho
017,illinois
018,indiana
019,iowa
020,kansas
021,kentucky
022,louisiana
023,maine
024,maryland
025,massachusetts
026,michigan
027,minnesota
028,mississippi
029,missouri
030,montana
031,nebraska
032,nevada
033,new hampshire
034,new jersey
035,new mexico
036,new york
037,north carolina
038,north dakota
039,ohio
040,oklahoma
041,oregon
042,pennsylvania
044,rhode island
045,south carolina
046,south dakota
047,tennessee
048,texas
049,utah
050,vermont
051,virginia
053,washington
054,west virginia
055,wisconsin
056,wyoming"
stateCodes <- fread(stateCodeCSV)

jobLessBSc <- right_join(jobLessBSc , stateCodes, by.x=c("ST"))
jobLessBSc[is.na(jobLessBSc)] <- 0
jobLessBSc <- mutate(jobLessBSc, value = jobLessBSc$count/stateTotalBSc$count*100)

state_choropleth(jobLessBSc, title = "Percentage of Unemployed BSc Holders", num_colors=9)
