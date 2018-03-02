library(openxlsx)
library(plyr)
library(ggplot2)
source('auxiliary.R')
## this file contains all patients who were admited for device implantation 
## between 2012 - April 2017. 1 sheet per year

PatientDataFile = 'raw data/CARDIAC DEVICES 2012 TO 30042017.xlsx'

##read sheet 1 (2012)
PatientData = read.xlsx(PatientDataFile, sheet = 1, cols = c(1,3,4,5,6))

##read sheets 2-5 (2013 - 2016). We'll have to do 2017 later
for (t in 2:5){
  dat = read.xlsx(PatientDataFile, sheet = t, cols = c(1,3,4,5,6) )
  PatientData = rbind(PatientData, dat)
}


PatientList = PatientData[!duplicated(PatientData$MRN),]
PatientList$'Implanted at JHC' = TRUE

write.csv(PatientList, file='clean data/Patient List.csv', row.names = FALSE)
count(PatientList$Sex)
pie(count(PatientList$Sex)$freq)
ageFreq = count(PatientList$Age)
plot(ageFreq)
