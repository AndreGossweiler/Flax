library(writexl)
#Incorrect write file below
#write_xlsx(SurvTry2,"SurvData.xlsx", sheetName = "Sheet1", row.names=FALSE)
## use SAS for data transformation/transpose
setwd("C:/Users/Fleur/Documents/Thesis Files/Freeze Study/Correlation tests")
WideSurvData=read.csv("Surv_wide.csv", sep = ",")
SURVTIME<-WideSurvData
head(SURVTIME)
colnames(SURVTIME)<- c('Sample_Number','treat','0','1','2','3')
head(df2)

library(dplyr)
#HeightData %>% group_by(Sample_Number,Time, treat) %>% 
#  summarise_at(vars("Total_HeightRegBrady"),mean, na.rm=T) -> df

setwd("C:/Users/Fleur/Documents/Thesis Files/Freeze Study/Correlation tests")
WideSurvData=read.csv("Surv_wide.csv", sep = ",")

WideSurvData %>% group_by(Sample_Number, treat) %>% 
  summarise_at(vars("ST3"),mean, na.rm=T) -> SURVTIME3


WideTry2=read.csv("VDSExcel.csv", sep = ",")
WideTry2 %>% group_by(Sample_Number, Time, treat) %>% 
  summarise_at(vars("SurvivalRegulated"),mean, na.rm=T) -> SurvTry2
WideTry2 %>% group_by(Sample_Number, Time, treat) %>% 
  summarise_at(vars("Freeze_Damage"),mean, na.rm=T) -> FreezeAvg
HeightWide=read.csv("HeightDifAvg_wide.csv", sep = ",")
HeightWide %>% group_by(Sample_Number, treat) %>% 
  summarise_at(vars("BaselineOne"),mean, na.rm=T) ->Base1Avg
HeightWide %>% group_by(Sample_Number, treat) %>% 
  summarise_at(vars("BaselineTwo"),mean, na.rm=T) ->Base2Avg
HeightWide %>% group_by(Sample_Number, treat) %>% 
  summarise_at(vars("BaselineThree"),mean, na.rm=T) ->Base3Avg

Column2toadd<- Base2Avg$BaselineTwo
Column3toadd<- Base3Avg$BaselineThree
Base1Avg<-cbind(Base1Avg,Column2toadd,Column3toadd)
colnames(Base1Avg)<- c('Sample_Number','treat','0to1Avg','1to2Avg','2to3Avg')
#install.packages("openxlsx")
library(openxlsx)
openxlsx::write.xlsx(SurvTry2, "SurvTry2.xlsx", rowNames=FALSE)
openxlsx::write.xlsx(FreezeAvg, "FreezeAvg.xlsx", rowNames=FALSE)
openxlsx::write.xlsx(Base1Avg, "HeightGrowthAvg.xlsx", rowNames=FALSE)
