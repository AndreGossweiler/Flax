setwd("C:/Users/Fleur/Documents/Thesis Files/Freeze Study")
HeightData=read.csv("MasterFreezeData111422.csv", sep = ",")
HeightData2=read.csv("MFDandEffects.csv", sep = ",")
#download.packages(Rmisc)
#install.packages('hrbrthemes')
#install.packages('GGally')
#install.packages('viridis')
#install.packages('ggplot2')

##
##
library(ggplot2)
library(hrbrthemes)
library(GGally)
#library(viridis)
library(dplyr)
#library(ggpubfigs)
#library(scales)
HeightData %>% group_by(Sample_Number,Time, treat) %>% 
  summarise_at(vars("Total_HeightRegBrady"),mean, na.rm=T) -> df

HeightData2 %>% group_by(Sample_Number,Time, Time.Effect, Treatment.Effect, Treatment.Time.Effect,treat) %>% 
  summarise_at(vars("Total_HeightRegBrady"),mean, na.rm=T) -> df2 
#temp try above^
#pvalues=read.csv("FixedEffectPvalues.csv", sep = ",")
legendlabels <-paste0("p = ", df2$Treatment.Effect)
legend(x = "topright", legend = legendlabels, bty = "n")

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

for (var in unique(df$Sample_Number)) {
  dev.new()
   x<-  ggplot(df[df$Sample_Number==var,],aes(x=Time, y=Height (cm)))+ geom_line(aes(Time,Total_HeightRegBrady,group=as.factor(treat),colour=as.factor(treat)))+
 #    geom_smooth(method=lm, se=FALSE, col="red", 
 #                size=.5,aes(Time,Total_HeightRegBrady))+
#     scale_color_manual(values="red",name="Trend Line")+
 #    guides(colour=guide_legend(override.aes = list(linetype=0)))+
     scale_color_manual(values = cbbPalette)+
    theme_ipsum()+
    theme(
      legend.title = element_text(face="bold"),
      axis.text = element_text(face="bold"),
      axis.text.x = element_text(face="bold"),
      axis.text.y = element_text(face="bold"),
      axis.title.y = element_text(hjust=0.5, face="bold"),
      axis.title.x = element_text(hjust=0.5, face="bold"),
      legend.position="right",
      plot.title = element_text(size=16),
      #legend(x = "topright", legend = legendlabels, bty = "n")
      )+
           ylim(0,200)+
     labs(x="Time (week)",y="Height (cm)",colour="Temperature Treatment (°C)")+
     #ggtitle(" ") 
     ggtitle(paste0(var))
   ggsave(paste0("/Users/Fleur/Documents/Thesis Files/Freeze Study/Figures/Height/Colourblind/HeightchangebyweekforSample",var,".png"))
   #   print(x) 
   } 
#copy line below inplace of 'print(x)' to save each graph as .png
#ggsave(paste0("Regrowth_Over_Time_by_",var,".png"),path="C\\Users\\Fleur\\Documents\\Thesis Files\\Freeze Study\\Figures\\Height\\")


##
##
#DATA TRANSFORMATION STEP
#For Wide data
#install.packages('writexl',)
library(writexl)
write_xlsx(df,"C:/Users/Fleur/Documents/Thesis Files/Freeze Study/HeightAvg")
## use SAS for data transformation/transpose
WideAvgHeightData=read.csv("Heightavg_wide.csv", sep = ",")
df2<-WideAvgHeightData
head(df)
colnames(df2)<- c('Sample_Number','treat','0','1','2','3')
head(df2)