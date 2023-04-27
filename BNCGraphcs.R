setwd("C:/Users/Fleur/Documents/Thesis Files/Freeze Study")
BNCTemps=read.csv("BNCtemps.csv", sep = ",")
library(dplyr)
TempReads<- BNCTemps %>% select('DateNTime','ChamberPoint1', 'ChamberPoint2', 'ChamberSetPoint',
                       'TrialTEMP2', 'Averagetemp')
#Verification Step
head(TempReads)
library(hrbrthemes)
library(GGally)
library(viridis)
library(tidyr)
#ggplot(TempReads)+geom_line(aes(DateNTime,Averagetemp))+ylim(-20,30)
#convertDateNTime to DateClass below
t<-TempReads$DateNTime
head(t)
unclass(t)
class(t)
p<-as.POSIXct(t)
print((strptime(t,"%mm/%dd-%YYYY %H:%M"))) 

      #ButtonAverage Below
for (var in unique(TempReads$TrialTEMP2)) {
  dev.new()
  x<-ggplot(TempReads[TempReads$TrialTEMP2==var,],aes(DateNTime,Averagetemp,group=1))+geom_line()+
    scale_color_viridis(discrete=FALSE) +
    theme_ipsum()+
    theme(
      legend.position="right",
      plot.title = element_text(size=15)
    )+
    ggtitle(var)
  x$labels$colour<-"Temperature (Celsius)"
print(x)  }

#Chamber observed points
for (var in unique(TempReads$TrialTEMP2)) {
  dev.new()
  x<-ggplot(TempReads[TempReads$TrialTEMP2==var,],aes(DateNTime,ChamberPoint1,group=1))+geom_line()+ylim(-20,30)+
    scale_color_viridis(discrete=TRUE) +
    theme_ipsum()+
    theme(
      legend.position="right",
      plot.title = element_text(size=15)
    )+
    ggtitle(var)
  x$labels$colour<-"Temperature (Celsius)"
  print(x)  } 

#Chamber set points
for (var in unique(TempReads$TrialTEMP2)) {
  dev.new()
  x<-ggplot(TempReads[TempReads$TrialTEMP2==var,],aes(DateNTime,ChamberSetPoint,group=1))+geom_line()+ylim(-20,30)+
    scale_color_viridis(discrete=TRUE) +
    theme_ipsum()+
    theme(
      legend.position="right",
      plot.title = element_text(size=15)
    )+
    ggtitle(var)
  x$labels$colour<-"Temperature (Celsius)"
  print(x)  } 

##
###
##All Height graphs below
##this is probably not the right spot for this
setwd("C:/Users/Fleur/Documents/Thesis Files/Freeze Study")
HeightData=read.csv("MasterFreezeData111422.csv", sep = ",")
#download.packages(Rmisc)
#install.packages('ggplot2')
library(ggplot2)
library(hrbrthemes)
library(GGally)
library(viridis)
library(dplyr)
HeightData %>% group_by(Sample_Number,Time, treat) %>% 
  summarise_at(vars("Total_HeightRegBrady"),mean, na.rm=T) -> df


for (var in unique(df$Sample_Number)) {
  dev.new()
  x<-  ggplot(df[df$Sample_Number==var,],aes(x=Time, y=Height (cm)))+ geom_line(aes(Time,Total_HeightRegBrady,group=as.factor(treat),colour=as.factor(treat)))+
    #    geom_smooth(method=lm, se=FALSE, col="red", 
    #                size=.5,aes(Time,Total_HeightRegBrady))+
    #     scale_color_manual(values="red",name="Trend Line")+
    #    guides(colour=guide_legend(override.aes = list(linetype=0)))+
    scale_color_viridis(discrete=TRUE)+
    theme_ipsum()+
    theme(
      legend.title = element_text(face="bold"),
      axis.text = element_text(face="bold"),
      axis.text.x = element_text(face="bold"),
      axis.text.y = element_text(face="bold"),
      axis.title.y = element_text(hjust=0.5, face="bold"),
      axis.title.x = element_text(hjust=0.5, face="bold"),
      legend.position="right",
      plot.title = element_text(size=16)
    )+
    ylim(0,200)+
    labs(y="Height (cm)",colour="Temperature Treatment (C)")+
    ggtitle(var)
  print(x)} 
