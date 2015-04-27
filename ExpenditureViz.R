#Author: Brent Cohn
#Date: 4/27/15
#Data source: Centers for Medicare & Medicaid Services (2011). 
#Health Expenditures by State of Residence. Retrieved (date accessed) at 
#http://www.cms.gov/NationalHealthExpendData/downloads/resident- state-estimates.zip
#note theme_border requires this code to run:
#https://github.com/sipemu/R-Code/blob/master/useful_tools/ggplot2/theme_border.R

if(length(ls())>0){rm(list=ls())}
packages=c("ggplot2","ggthemes","dplyr","scales",'tidyr',"gridExtra")
lapply(packages,require,character.only=TRUE)

Medicare = read.csv("~/Downloads/resident-state-estimates/MEDICARE_PER_ENROLLEE09.CSV")
Medicaid = read.csv("~/Downloads/resident-state-estimates/MEDICAID_PER_ENROLLEE09.CSV")

Medicare=Medicare[Medicare$State_Name!="",]

Medicare=Medicare[Medicare$State_Name!="",]
Medicare=Medicare[Medicare$Item!="Medicare/Personal Health Care ($)",]


MedicareLong=Medicare%>%gather(Year,Expenditure,7:25)

MedicareTotal=MedicareLong%>% 
  group_by(State_Name,Year)%>%
  summarize(MedicareTotal=sum(Expenditure))
  

Medicaid=Medicaid[Medicaid$Item!="Medicaid/Personal Health Care ($)",]

MedicaidLong=Medicaid%>%gather(Year,Expenditure,7:25)

MedicaidTotal=MedicaidLong%>% 
  group_by(State_Name,Year)%>%
  summarize(MedicaidTotal=sum(Expenditure))

Mergicare<-merge(MedicareTotal,MedicaidTotal,by=c("State_Name","Year"))
Mergicare$Year<-gsub("Y","",Mergicare$Year)
Mergicare$Year<-as.numeric(as.character(Mergicare$Year))

Mergicare$State='Other States'
Mergicare[Mergicare$State_Name=="Hawaii",]$State="Hawai'i"
Mergicare[Mergicare$State_Name=="Massachusetts",]$State="Massachusetts"
Mergicare[Mergicare$State_Name=="Texas",]$State="Texas"
Mergicare[Mergicare$State_Name=="Florida",]$State="Florida"
Mergicare[Mergicare$State_Name=="Arizona",]$State="Arizona"
Mergicare[Mergicare$State_Name=="Alaska",]$State="Alaska"

Mergicare$fiveo='Other States'
Mergicare[Mergicare$State_Name=="Hawaii",]$fiveo="Hawai'i"


Mergicare$State<-ordered(Mergicare$State,levels=c("Hawai'i","Arizona","Alaska","Massachusetts","Texas","Florida",'Other States'))
Mergicare<-Mergicare[order(Mergicare$State,Mergicare$State_Name,Mergicare$Year),]
Mergicare<-Mergicare[order(Mergicare$fiveo),]


p1<-ggplot(data=Mergicare,aes(x=MedicareTotal,y=MedicaidTotal,group=factor(State_Name),color=factor(fiveo)))+geom_point(aes(size=Year))+geom_path(aes(size=Year))+
  theme_few()+scale_color_manual(values =c("#FFD700E6","#BEBEBE17"))+xlab("Medicare Spending Per Capita")+ylab("Medicaid Spending Per Capita")+theme(panel.border = theme_border("none"))+
  scale_x_continuous(breaks=c(2000,8000,14000),labels=c("$2,000","$8,000","$14,000"),limits=c(1900,16000))+
  scale_y_continuous(breaks=c(2000,8000,14000),labels=c("$2,000","$8,000","$14,000"),limits=c(1900,16000))+
  guides(colour = guide_legend(title="State"))+ggtitle("State Level Medicare and Medicaid Spending Per Capita 1995-2009: \n Hawai'i")


p2<-ggplot(data=Mergicare[Mergicare$State!="Other States",],aes(x=MedicareTotal,y=MedicaidTotal,group=factor(State_Name),color=State))+geom_point(aes(size=Year),alpha=.9)+geom_path(aes(size=Year),alpha=.1)+
  scale_color_manual(values=c("#d73027","#fc8d59","#e6f598","#fee08b","#91bfdb","#4575b4"))+theme_few()+xlab("Medicare Spending Per Capita")+ylab("Medicaid Spending Per Capita")+theme(panel.border = theme_border("none"))+
  scale_x_continuous(breaks=c(2000,8000,14000),labels=c("$2,000","$8,000","$14,000"),limits=c(1900,16000))+
  scale_y_continuous(breaks=c(2000,8000,14000),labels=c("$2,000","$8,000","$14,000"),limits=c(1900,16000))+
  guides(colour = guide_legend(title="State"))+ggtitle("State Level Medicare and Medicaid Spending Per Capita 1995-2009: \n Selected States")

