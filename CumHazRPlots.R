setwd("C:/Users/Fleur/Documents/Thesis Files/Freeze Study/SurvivalSAS/")
CUMHAZData=read.csv("FinalsurvcumhazNOoutliers.csv", sep = ",")
library(ggplot2)
library(dplyr)
library(scales)
#install.packages('ggrepel')
library(ggrepel)
# reformat column titles
 colnames(CUMHAZData)[1]<- "Sample_Number"
#
CUMHAZData <- CUMHAZData%>% filter(treat!=0)
CumHazOnly3 <-CUMHAZData%>% filter(Time!=2)
CumHazOnly3 <-CumHazOnly3%>% filter(Time!=1)
###To make time exclusive to 3

###to make time exclusive to 1
CUMHAZData <- CUMHAZData%>% filter(treat!=0)
CumHazOnly1 <-CUMHAZData%>% filter(Time!=2)
CumHazOnly1 <-CumHazOnly1%>% filter(Time!=3)

##Remove number 28 and 46 cause they were bad
CumHazOnly3filt <-CumHazOnly3%>% filter(Sample_Number!=46)
CumHazOnly3filt <-CumHazOnly3filt%>% filter(Sample_Number!=28)
#duplicate for time 1
CumHazOnly1filt <-CumHazOnly1%>% filter(Sample_Number!=46)
CumHazOnly1filt <-CumHazOnly1filt%>% filter(Sample_Number!=28)
#or if they have already been removed.
CumHazOnly3filt <- CumHazOnly3


#### SEPARATED BY SAMPLE NOT THE CORRECT VISUALIZATION BELOW!!!
for (var in unique(CUMHAZData$Sample_Number)) {
  dev.new()
c<- ggplot(CUMHAZData[CUMHAZData$Sample_Number==var,], aes(x=Time))+
  geom_line(aes(y=CumHaz,group=as.factor(treat),color=as.factor(treat)))+
  geom_errorbar(aes(ymin=LowerCumHaz, ymax=UpperCumHaz,group=as.factor(treat),color=as.factor(treat)), width=0.05)+
  scale_y_continuous(name = "Cum Haz", breaks = seq(0,0.3,by=.15),
                     limits= c(0,0.3),
                    expand = c(0,0))+
  #coord_cartesian(ylim=c(0,0.3))+
 # coord_cartesian(ylim = c(2,2.3), y = "secondary")+
  geom_point(aes(y=StdErrCumHaz, group=as.factor(treat), color="Treat STDError"))+
  theme(legend.position = "bottom")+
  labs(title = "Treatment effects of Cumulative Hazard across Time", x = "Time", y= "Cumulative Hazard")+
  ggtitle(var)
print(c)}

## Set X axis as temperature
#### NOT CORRECT SCALE DO NOT USE THE FOLLOWING GRAPH CODE ###################
  c<-ggplot(CumHazOnly3,aes(x=as.factor(treat)))+
      geom_line(aes(x=as.factor(treat),y=CumHaz,group=as.factor(Sample_Number),color=as.factor(Sample_Number)))+
      # geom_errorbar(aes(ymin=LowerCumHaz, ymax=UpperCumHaz,x=as.factor(treat), group=as.factor(Sample_Number),color=as.factor(Sample_Number)), width=0.05)+
      # scale_y_continuous(name = "Cum Haz", breaks = seq(0,0.3,by=.15),
      #                     limits= c(0,0.3),
      #                    expand = c(0,0))+
        #coord_cartesian(ylim=c(0,0.3))+
        # coord_cartesian(ylim = c(2,2.3), y = "secondary")+
        #geom_point(aes(y=StdErrCumHaz, group=as.factor(Sample_Number), color="STDError"))+
      theme(legend.position = "bottom")+
      labs(title = "Treatment effects of Cumulative Hazard across Treatment", x = "treat", y= "Cumulative Hazard")
      print(c)
##Remove number 28 and 46 cause they were bad
CumHazOnly3filt <-CumHazOnly3%>% filter(Sample_Number!=46)
CumHazOnly3filt <-CumHazOnly3filt%>% filter(Sample_Number!=28)
CumHazOnly3filt <-CumHazOnly3filt%>% filter(Sample_Number!=20)
CumHazOnly3filt <-CumHazOnly3filt%>% filter(Sample_Number!=26)
#duplicate for time 1
CumHazOnly1filt <-CumHazOnly1%>% filter(Sample_Number!=46)
CumHazOnly1filt <-CumHazOnly1filt%>% filter(Sample_Number!=28)

##THE FOLLOWING CODE LABELS ALL LINES BUT IS SEVERELY DENSE. ###################
     c<-ggplot(CumHazOnly3filt,aes(x=as.factor(treat)))+
       geom_line(aes(x=as.factor(treat),y=CumHaz,group=as.factor(Sample_Number),color=as.factor(Sample_Number)))+
       # geom_errorbar(aes(ymin=LowerCumHaz, ymax=UpperCumHaz,x=as.factor(treat), group=as.factor(Sample_Number),color=as.factor(Sample_Number)), width=0.05)+
        scale_y_continuous(name = "Cum Haz", breaks = seq(0,2.0,by=0.5),
                            limits= c(0,2.5),expand = c(0,0), oob=squish)+
      #25 and 32 are off the scale
      #  scale_y_continuous(waiver(), sec.axis = dup_axis(~.+2))+
       geom_text_repel(data=CumHazOnly3filt_treat15, aes(y=CumHaz,label=Sample_Number, color=as.factor(Sample_Number)),
                       #point.size=NA,segment.colour=NA,
                       segment.alpha = 0, ## This will 'hide' the link
                       segment.curvature = 0.8,
                       segment.square = TRUE,
                       segment.shape=0.5,
                       segment.size=0.2,
                       segment.inflect=TRUE,
                       #segment.color = 'grey',
                       box.padding = 0.5,
                       point.padding = 1,
                       nudge_x = 0.15,
                       nudge_y = 0.9,
                       force = 0.5,
                       hjust = "left",
                       direction="y",
                       max.overlaps = 44, size= 1,xlim = c(15,16.5),
                                              ylim = c(0,2.5))+
    #below is for the numbers above is for the lines...i think.
          geom_text_repel(data=CumHazOnly3filt_treat15,aes(y=CumHaz, label=Sample_Number),
                       segment.curvature = 0.7,
                       segment.square = TRUE,
                       segment.color = 'grey',
                       box.padding = 0.7,
                       point.padding = 0,
                       nudge_x = 1.15,
                      nudge_y = 0.7,
                       force = 0.5,
                       hjust = 0,
                       direction="y",
                       na.rm = TRUE, 
                       ylim = c(0,2.5))+
              #coord_cartesian(ylim=c(0,2.1))+ 
       #coord_cartesian(ylim = c(4.1,5.3), y = "secondary")+
       #geom_point(aes(y=StdErrCumHaz, group=as.factor(Sample_Number), color="STDError"))+
       theme(legend.position = "right",
             panel.grid.minor.y = element_blank(),
             panel.grid.major.x = element_blank())+
       labs(title = "Treatment effects of Cumulative Hazard across Treatment", 
            x = "", y="Cumulative Hazard")
     print(c)     
      
#### THE FOLLOWING CODE IS TO Graphically SHOWCASE THE OUTLIER SAMPLES ###################
     
     c<-ggplot(CumHazOnly3filt,aes(x=as.factor(treat)))+
       geom_line(aes(x=as.factor(treat),y=CumHaz,group=as.factor(Sample_Number),color=as.factor(Sample_Number)))+
       # geom_errorbar(aes(ymin=LowerCumHaz, ymax=UpperCumHaz,x=as.factor(treat), group=as.factor(Sample_Number),color=as.factor(Sample_Number)), width=0.05)+
       scale_y_continuous(name = "Cumulative Hazard", breaks = seq(0,2.0,by=0.5),
                          limits= c(0,3.5),expand = c(0,0), oob=squish)+
       #add the trans line in place of the above code in s_y_c() for log scale.
       #trans = "log10
       #25 and 32 are off the scale
       #  scale_y_continuous(waiver(), sec.axis = dup_axis(~.+2))+
       geom_text_repel(data=filter(CumHazOnly3filt,treat==15 & (CumHaz > 1|Sample_Number==45|CumHaz<0.09)),
                       aes(y=CumHaz, label=Sample_Number),
                       #point.size=NA,segment.colour=NA,
                       segment.alpha = 0, ## This will 'hide' the link
                       segment.curvature = 0.8,
                       segment.square = TRUE,
                       segment.shape=-1,
                       segment.size=0.2,
                       segment.inflect=TRUE,
                       #segment.color = 'grey',
                       box.padding = 0.5,
                       point.padding = 1,
                       nudge_x = 0.15,
                       nudge_y = 0.9,
                       force = 0.5,
                       hjust = "left",
                       direction="y",
                       max.overlaps = 44, size= 1,xlim = c(15,16.5),
                       ylim = c(0,3.5))+
       #below is for the numbers above is for the lines...i think.
       geom_text_repel(data=filter(CumHazOnly3filt,treat==15 & (CumHaz > 1|Sample_Number==45|CumHaz< 0.09)),
                       aes(y=CumHaz, label=Sample_Number),
                       segment.curvature = 0.7,
                       segment.square = TRUE,
                       segment.inflect= TRUE,
                       segment.color = 'grey',
                       box.padding = 0.3,
                       point.padding = 0.1,
                       nudge_x = 1.7,
                       nudge_y = 0.7,
                       force = 0.5,
                       hjust = 0.5,
                       direction="y",
                       na.rm = TRUE, 
                       ylim = c(0,NA))+
       #coord_cartesian(ylim=c(0,2.1))+ 
       #coord_cartesian(ylim = c(4.1,5.3), y = "secondary")+
       #geom_point(aes(y=StdErrCumHaz, group=as.factor(Sample_Number), color="STDError"))+
       theme(legend.position = "none",
             #legend.title = element_text(), 
             #axis.title.y = element_text(),
             panel.grid.minor.y = element_blank(),
             panel.grid.major.x = element_blank())+
       labs(x = "Treatment Temperature (*-1°C)")
     print(c)   
       #labs(title = "Treatment effects of Cumulative Hazard across Treatment", 
           # x = "", y="Cumulative Hazard")
     
     #ggsave(paste0("/Users/Fleur/Documents/Thesis Files/Freeze Study/Figures/Survival/CumulativeHazard",".png"))
     
     
     
##### END OUTLIER CODE     #########################################
     
######### OUTLIER TIME 1 ###############3
     
     d<-ggplot(CumHazOnly1filt,aes(x=as.factor(treat)))+
       geom_line(aes(x=as.factor(treat),y=CumHaz,group=as.factor(Sample_Number),color=as.factor(Sample_Number)))+
       # geom_errorbar(aes(ymin=LowerCumHaz, ymax=UpperCumHaz,x=as.factor(treat), group=as.factor(Sample_Number),color=as.factor(Sample_Number)), width=0.05)+
       scale_y_continuous(name = "Cum Haz", breaks = seq(0,2.0,by=0.5),
                          limits= c(0,3.5),expand = c(0,0), oob=squish)+
       #25 and 32 are off the scale
       #  scale_y_continuous(waiver(), sec.axis = dup_axis(~.+2))+
       geom_text_repel(data=filter(CumHazOnly1filt,treat==15 & (CumHaz > 1|CumHaz<0.09)),
                       aes(y=CumHaz, label=Sample_Number),
                       #point.size=NA,segment.colour=NA,
                       segment.alpha = 0, ## This will 'hide' the link
                       segment.curvature = 0.8,
                       segment.square = TRUE,
                       segment.shape=-1,
                       segment.size=0.2,
                       segment.inflect=TRUE,
                       #segment.color = 'grey',
                       box.padding = 0.5,
                       point.padding = 1,
                       nudge_x = 0.15,
                       nudge_y = 0.9,
                       force = 0.5,
                       hjust = "left",
                       direction="y",
                       max.overlaps = 44, size= 1,xlim = c(15,16.5),
                       ylim = c(0,3.5))+
       #below is for the numbers above is for the lines...i think.
       geom_text_repel(data=filter(CumHazOnly1filt,treat==15 & (CumHaz > 1|CumHaz< 0.09)),
                       aes(y=CumHaz, label=Sample_Number),
                       segment.curvature = 0.7,
                       segment.square = TRUE,
                       segment.inflect= TRUE,
                       segment.color = 'grey',
                       box.padding = 0.3,
                       point.padding = 0.1,
                       nudge_x = 1.7,
                       nudge_y = 0.7,
                       force = 0.5,
                       hjust = 0.5,
                       direction="y",
                       na.rm = TRUE, 
                       ylim = c(0,NA))+
       #coord_cartesian(ylim=c(0,2.1))+ 
       #coord_cartesian(ylim = c(4.1,5.3), y = "secondary")+
       #geom_point(aes(y=StdErrCumHaz, group=as.factor(Sample_Number), color="STDError"))+
       theme(legend.position = "right",
             panel.grid.minor.y = element_blank(),
             panel.grid.major.x = element_blank())+
       labs(title = "Treatment effects of Cumulative Hazard across Treatment", 
            x = "", y="Cumulative Hazard")
     print(d)   
     #######################################
     
     
     #######Start Surv. graph.
     s<-ggplot(CumHazOnly3filt, aes(x = as.factor(treat))) +
       geom_line(aes(y = Survival,
                     group = as.factor(Sample_Number), 
                     color = as.factor(Sample_Number))) +
       scale_y_continuous(name = "Survival Probability", breaks = seq(0,1.0,by=0.25),
                          limits = c(0,1), expand = c(0,0), oob = squish) +
       geom_text_repel(data=filter(CumHazOnly3filt,treat==15 & (Survival > .990|Sample_Number==45|Survival<0.09)),
                       aes(y=Survival, label=Sample_Number),
                       #point.size=NA,segment.colour=NA,
                       segment.alpha = 0, ## This will 'hide' the link
                       segment.curvature = 0.8,
                       segment.square = TRUE,  segment.shape=-1,
                       segment.size=0.2,            segment.inflect=TRUE,
                       #segment.color = 'grey',
                       box.padding = 0.5,  point.padding = 1,
                       nudge_x = 0.15,     nudge_y = 0.9,
                       force = 0.5,        hjust = "left",
                       direction="y",  max.overlaps = 44, size= 1,xlim = c(15,16.5),
                       ylim = c(0,NA))+
       #below is for the numbers above is for the lines...i think.
       geom_text_repel(data=filter(CumHazOnly3filt,treat==15 & (Survival > .990|Sample_Number==45|Survival< 0.09)),
                       aes(y=Survival, label=Sample_Number),
                       segment.curvature = 0.7,  segment.square = TRUE,
                       segment.inflect= TRUE, segment.color = 'grey',
                       box.padding = 0.3,  point.padding = 0.1,
                       nudge_x = 1.7,    nudge_y = 0.7,
                       force = 0.5,   hjust = 0.5,     direction="y",
                       na.rm = TRUE,  ylim = c(0,NA))+
       #geom_point(aes(y=StdErrCumHaz, group=as.factor(Sample_Number), color="STDError"))+
       theme(legend.position = "right",
             panel.grid.minor.y = element_blank(),
             panel.grid.major.x = element_blank())+
       labs(x = "Temperature (*-1°C)", y = "Survival probability", color = "Accession") +
       ggtitle(" ")
     print(s)
     
     
     
     
     ### START OF FOREST PLOT TRIAL AND ERROR ###
setwd("C:/Users/Fleur/Documents/Thesis Files/Freeze Study/SurvivalSAS/Final Draft")
#install.packages("readxl")
library("readxl")
setwd("C:/Users/Fleur/Documents/Thesis Files/Freeze Study/SurvivalSAS/")
Cox<-read_excel("Cox.xls", sheet = "Cox")
transposed_Cox<-t(Cox)
Coxlist<-as.list(transposed_Cox)

no_headers=subset(Cox, select = -c(1:ncol(transposed_Cox)))
no_headers[1,1]
colnames(transposed_Cox)=NULL
head(transposed_Cox)

setwd("C:/Users/Fleur/Documents/Thesis Files/Freeze Study/SurvivalSAS/Final Draft")
HAZRAT<-read_excel("SurvivalCoxSummaryCUMHAZref45Final.xlsx", sheet = "ComboHR")
install.packages("survminer")
library(survminer)
ggforest()










# Plot the survival curve
ggplot(CumHazOnly3filt, x =as.factor(treat))+
  geom_line(aes(x =as.factor(treat), y =Survival,
                group=as.factor(Sample_Number), color=as.factor(Sample_Number) )) +
  scale_y_continuous(name = "Survival", breaks = seq(0,1.0,by=0.25),
                     limits= c(0,1),expand = c(0,0), oob=squish)+
  labs(x = "Temperature (°C)", y = "Survival probability", color = "Accession") +
  #ggtitle("Survival curve for all accession")

treat<- CumHazOnly3filt$treat


s<-ggplot(CumHazOnly3filt, aes(x = as.factor(treat))) +
  geom_line(aes(y = Survival,
                group = as.factor(Sample_Number), 
                color = as.factor(Sample_Number))) +
  scale_y_continuous(name = "Survival Probability", breaks = seq(0,1.0,by=0.25),
                     limits = c(0,1), expand = c(0,0), oob = squish) +
  geom_text_repel(data=filter(CumHazOnly3filt,treat==15 & (Survival > .990|Sample_Number==45|Survival<0.09)),
                  aes(y=Survival, label=Sample_Number),
                  #point.size=NA,segment.colour=NA,
                  segment.alpha = 0, ## This will 'hide' the link
                  segment.curvature = 0.8,
                  segment.square = TRUE,  segment.shape=-1,
                  segment.size=0.2,            segment.inflect=TRUE,
                  #segment.color = 'grey',
                  box.padding = 0.5,  point.padding = 1,
                  nudge_x = 0.15,     nudge_y = 0.9,
                  force = 0.5,        hjust = "left",
                  direction="y",  max.overlaps = 44, size= 1,xlim = c(15,16.5),
                  ylim = c(0,NA))+
  #below is for the numbers above is for the lines...i think.
  geom_text_repel(data=filter(CumHazOnly3filt,treat==15 & (Survival > .990|Sample_Number==45|Survival< 0.09)),
                  aes(y=Survival, label=Sample_Number),
                  segment.curvature = 0.7,  segment.square = TRUE,
                  segment.inflect= TRUE, segment.color = 'grey',
                  box.padding = 0.3,  point.padding = 0.1,
                  nudge_x = 1.7,    nudge_y = 0.7,
                  force = 0.5,   hjust = 0.5,     direction="y",
                  na.rm = TRUE,  ylim = c(0,NA))+
  #geom_point(aes(y=StdErrCumHaz, group=as.factor(Sample_Number), color="STDError"))+
  theme(legend.position = "right",
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank())+
  labs(x = "Temperature (*-1°C)", y = "Survival probability", color = "Accession") +
  ggtitle(" ")
  print(s)
  
  #############
  #CHAZ CODE!!
  #################################
  c<-ggplot(CumHazOnly3filt,aes(x=as.factor(treat)))+
    geom_line(aes(x=as.factor(treat),y=CumHaz,group=as.factor(Sample_Number),color=as.factor(Sample_Number)))+
    # geom_errorbar(aes(ymin=LowerCumHaz, ymax=UpperCumHaz,x=as.factor(treat), group=as.factor(Sample_Number),color=as.factor(Sample_Number)), width=0.05)+
    scale_y_continuous(name = "Cumulative Hazard", trans = "log10")+
    #add the trans line in place of the above code in s_y_c() for log scale.
    #trans = "log10"
    #25 and 32 are off the scale
    #  scale_y_continuous(waiver(), sec.axis = dup_axis(~.+2))+
    geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
    geom_text(y = 1,data=filter(CumHazOnly3filt,treat==3), label = "Cumulative Hazard = 1", hjust = "left", color = "red") +
    geom_text_repel(data=filter(CumHazOnly3filt,treat==15 & (CumHaz > 1|Sample_Number==45)),
                    aes(y=CumHaz, label=Sample_Number),
                    #point.size=NA,segment.colour=NA,
                    segment.alpha = 0, ## This will 'hide' the link
                    segment.curvature = 0.8,
                    segment.square = TRUE,
                    segment.shape=-1,
                    segment.size=0.2,
                    segment.inflect=TRUE,
                    #segment.color = 'grey',
                    box.padding = 0.5,
                    point.padding = 1,
                    nudge_x = 0.15,
                    nudge_y = 4,
                    force = 0.5,
                    direction="y",
                    max.overlaps = 44, size= 1,xlim = c(15,16.5),
                    ylim = c(NA,NA))+
    #below is for the numbers above is for the lines...i think.
    geom_text_repel(data=filter(CumHazOnly3filt,treat==15 & (CumHaz > 1|Sample_Number==45)),
                    aes(y=CumHaz, label=Sample_Number),
                    segment.curvature = 0.7,
                    segment.square = TRUE,
                    segment.inflect= TRUE,
                    segment.color = 'grey',
                    box.padding = 0.3,
                    point.padding = 0.1,
                    nudge_x = 1.7,
                    nudge_y =4,
                    force = 0.5,
                    direction="y",
                    na.rm = TRUE, 
                    ylim = c(NA,NA))+
    geom_text_repel(data=filter(CumHazOnly3filt,treat==15 & (CumHaz<0.09)),
                    aes(y=CumHaz, label=Sample_Number),
                    #point.size=NA,segment.colour=NA,
                    segment.alpha = 0, ## This will 'hide' the link
                    segment.curvature = 0.8,
                    segment.square = TRUE,
                    segment.shape=-1,
                    segment.size=0.2,
                    segment.inflect=TRUE,
                    #segment.color = 'grey',
                    box.padding = 0.5,
                    point.padding = 1,
                    nudge_x = 0.15,
                    nudge_y = 4,
                    force = 2,
                    direction="both",
                    max.overlaps = 4, size= 1,xlim = c(15,16.5),
                    ylim = c(NA,0.1))+
    #below is for the numbers above is for the lines...i think.
    #############################################################
  geom_text_repel(data=filter(CumHazOnly3filt,treat==15 & (CumHaz< 0.09)),
                  aes(y=CumHaz, label=Sample_Number),
                  segment.curvature = 0.7,
                  segment.square = TRUE,
                  segment.inflect= TRUE,
                  segment.color = 'grey',
                  box.padding = 0.3,
                  point.padding = 0.1,
                  nudge_x = 1.7,
                  nudge_y =4,
                  force = 2,
                  direction="both",
                  na.rm = TRUE, 
                  ylim = c(NA,0.1))+
    #coord_cartesian(ylim=c(0,2.1))+ 
    #coord_cartesian(ylim = c(4.1,5.3), y = "secondary")+
    #geom_point(aes(y=StdErrCumHaz, group=as.factor(Sample_Number), color="STDError"))+
    theme(legend.position = "none",
          #legend.title = element_text(), 
          #axis.title.y = element_text(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank())+
    labs(x = "Treatment Temperature (*-1°C)")
  #print(c)   
  #labs(title = "Treatment effects of Cumulative Hazard across Treatment", 
  # x = "", y="Cumulative Hazard")
  
  ggsave(paste0("/Users/Fleur/Documents/Thesis Files/Freeze Study/Figures/Survival/LPCH",".png"))
  
  
