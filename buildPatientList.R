library(openxlsx)
library(plyr)
library(ggplot2)
source('auxiliary.R')
## this file contains all patients who were admited for device implantation 
## between 2012 - April 2017. 1 sheet per year

PatientDataFile = 'raw data/CARDIAC DEVICES 2012 TO 30042017.xlsx'

##read sheet 1 (2012)
##columns are
##1 = MN, 2 = AN, 3 = Surname, 4 = Firstname, 5 = Age, 6 = Gender
PatientData = read.xlsx(PatientDataFile, sheet = 1, cols = c(1,2,3,4,5,6))

##read sheets 2-6 (2013 - 2017). 
for (t in 2:6){
  dat = read.xlsx(PatientDataFile, sheet = t, cols = c(1,2,3,4,5,6) )
  PatientData = rbind(PatientData, dat)
}

##add all patients who had TRUE complications to list
## columns are
## 1 = MN, 2 = AN, 3 = Surname, 4 = Firstname, 5 = Age, 6 = Gender, 8 = Confirmed complication
PatientsWithComplicationsFile = 'raw data/20180610 Complication data collection .xlsx'

dat = read.xlsx(PatientsWithComplicationsFile, sheet = 1, cols = (c(1,2,3,4,5,6,8)))

PatientsWithComplicationsData = dat[(!is.na(dat[,7]) & dat[,7] == 'TRUE'),]

for (t in 2:11){

  dat = read.xlsx(PatientsWithComplicationsFile, sheet = t, cols = (c(1,2,3,4,5,6,8)))
  print (t)
  print (names(dat[7]))
  datComplicationTrue = dat[(!is.na(dat[,7]) & dat[,7] == 'TRUE'),]
  if (nrow(datComplicationTrue) > 0){
    PatientsWithComplicationsData = rbind.all.columns(PatientsWithComplicationsData, datComplicationTrue)
    
  }
  else{
    PatientsWithComplicationsData[,colnames(dat[7])] = FALSE
  }
}

## rename the columns to follow one standard naming scheme
colnames(PatientsWithComplicationsData)[c(1,3,4,5,6)] = c('MRN', 'Surname', 'Patient.Given.Name', 'Age', 'Sex')

## combine the Patient Data list with the Patients who had complications into one list
CombinedPatientData = rbind.all.columns(PatientData, PatientsWithComplicationsData)

## calculate total admissions each patient had
## first we need to remove all duplicate ANs - each AN = 1 admission
## then we can tally every time an MN (patient unique identifier) shows up in the list
## finally we create a new column and match up our tally with the patient list dataframe
AdmissionCount = as.data.frame(table(CombinedPatientData$MRN[!duplicated(CombinedPatientData$AN)]))
colnames(AdmissionCount) = c('MRN', 'Cardiac Device Admissions')
UniquePatientList = CombinedPatientData[!duplicated(CombinedPatientData$MRN),]

UniquePatientList2 = merge(UniquePatientList, AdmissionCount, by="MRN")

## if a patient has experienced a complication, change its flag to true
HasComplication = function(PatientMRN, ComplicationColumn){
  ComplicationTrue = any(CombinedPatientData[CombinedPatientData$MRN == PatientMRN, ComplicationColumn], na.rm=TRUE)
  return (ComplicationTrue)
}
UniquePatientList3 = UniquePatientList2
for (t in 7:17){
  ComplicationTrue = sapply(UniquePatientList3$MRN, HasComplication, ComplicationColumn = t)
  UniquePatientList3[t] = ComplicationTrue
}

##get list of device types we are auditing
ListOfDeviceTypesFile = 'raw data/List Of Device Types.csv'
DeviceTypes = read.csv(ListOfDeviceTypesFile)

##get data that categorizes device serial no.s into the device types
CategorisedDeviceListFile = 'raw data/Device Names and Type.csv'
CategorisedDeviceList = read.csv(CategorisedDeviceListFile)


##standardise the names of the device types
CategorisedDeviceList$Device.Type[CategorisedDeviceList$Device.Type == 'ICD '] = 'ICD'
CategorisedDeviceList$Device.Type[CategorisedDeviceList$Device.Type == 'Pacemaker'] = 'PPM'
CategorisedDeviceList$Device.Type[CategorisedDeviceList$Device.Type == 'Pacemaker DC'] = 'PPM DC'
CategorisedDeviceList$Device.Type[CategorisedDeviceList$Device.Type == 'Pacemaker SC'] = 'PPM SC'
CategorisedDeviceList$Device.Type[CategorisedDeviceList$Device.Type == 'PPM SC '] = 'PPM SC'


## load the file that contains all patient device information
## contains 5 sheets for 2013-2017
## 2012 is missing
## in a format ID / Last Name / First Name / Exam Date / Equipment / Vendor / Serial no.
## if a row's ID is blank, then we need to add an ID - it will be the first row above it iwth a valid ID
## the IDs are all MN numbers, but the MN and 00 are missing - the format is MN - 0s - ID, and a valid MN
## contains 8 digits
## once we have fixed the IDs we need to merge the categories using the equipment column as our index

PatientDeviceInformationFile = 'raw data/Device Information.xlsx'
PatientDeviceInformation = read.xlsx(PatientDeviceInformationFile, sheet = 1, cols = c(1:7))
for (t in 2:5){
  dat = read.xlsx(PatientDeviceInformationFile, sheet = t, cols = c(1:7))
  PatientDeviceInformation = rbind(PatientDeviceInformation, dat)
}
fillTheBlanks <- function(x, missing=""){
  currentVal = x[1]
  for (t in 2:length(x)){
    if (is.na(x[t])){
      x[t] = currentVal
    }
    else{
      currentVal = x[t]
    }
  }
  return (x)
}
PatientDeviceInformation$IDFilled = fillTheBlanks(PatientDeviceInformation$ID)
convertPatientIDToMRN = function(x){
  ptMRN = paste("MN", strrep(0,8-nchar(x)),x, collapse = "", sep="")
  return (ptMRN)
}
PatientDeviceInformation$MRN = sapply(PatientDeviceInformation$IDFilled, convertPatientIDToMRN)

colnames(PatientDeviceInformation)[5] = 'Device.Name'
PatientDeviceInformation2 = merge(PatientDeviceInformation, CategorisedDeviceList[,c(1,4)], by="Device.Name")

PatientDeviceInformation2$Vendor[PatientDeviceInformation2$Vendor == 'St Jude'] = 'ST JUDE MEDICAL'

## connect our giant UniquePatientList with Device List
## what we want is a column for PPM DC, PPM SC, ICD, CRTD,CRTP, ICD
## flag as true if they have had that kind of device inserted, false otherwise
## we can also do the brands they have had inserted!
## 
UniquePatientList3$'PPM DC' = UniquePatientList3$MRN %in% PatientDeviceInformation2$MRN[PatientDeviceInformation2$Device.Type == 'PPM DC']
UniquePatientList3$'PPM SC' = UniquePatientList3$MRN %in% PatientDeviceInformation2$MRN[PatientDeviceInformation2$Device.Type == 'PPM SC']
UniquePatientList3$'PPM' = UniquePatientList3$MRN %in% PatientDeviceInformation2$MRN[PatientDeviceInformation2$Device.Type == 'PPM']
UniquePatientList3$'Loop recorder' = UniquePatientList3$MRN %in% PatientDeviceInformation2$MRN[PatientDeviceInformation2$Device.Type == 'Loop recorder']
UniquePatientList3$'CRT-D' = UniquePatientList3$MRN %in% PatientDeviceInformation2$MRN[PatientDeviceInformation2$Device.Type == 'CRT-D']
UniquePatientList3$'CRT-P' = UniquePatientList3$MRN %in% PatientDeviceInformation2$MRN[PatientDeviceInformation2$Device.Type == 'CRT-P']
UniquePatientList3$'ICD' = UniquePatientList3$MRN %in% PatientDeviceInformation2$MRN[PatientDeviceInformation2$Device.Type == 'ICD']
UniquePatientList3$'Known Device' = UniquePatientList3$MRN %in% PatientDeviceInformation2$MRN
UniquePatientList3$'BIOTRONIK' = UniquePatientList3$MRN %in% PatientDeviceInformation2$MRN[PatientDeviceInformation2$Vendor == 'BIOTRONIK']
UniquePatientList3$'Boston Scientific' = UniquePatientList3$MRN %in% PatientDeviceInformation2$MRN[PatientDeviceInformation2$Vendor == 'Boston Scientific']
UniquePatientList3$'Medtronic' = UniquePatientList3$MRN %in% PatientDeviceInformation2$MRN[PatientDeviceInformation2$Vendor == 'Medtronic']
UniquePatientList3$'ST JUDE MEDICAL' = UniquePatientList3$MRN %in% PatientDeviceInformation2$MRN[PatientDeviceInformation2$Vendor == 'ST JUDE MEDICAL']

##now to add data on results of complications
##Infections
##Blood culture growth - can either be no growth (False), bacterial growth (True), or not tested (NA)
##Wound swab growth - can either be no growth (False), bacterial growth (True), or not tested (NA)
##Fever > 38C is either TRUE or FALSE
##IV antibiotics is either TRUE or FALSE
##PO antibiotics is either TRUE or FALSE
##Wound debridement is TRUE or FALSE
##Wound revision is TRUE or FALSE
##Wound evacuation is TRUE or FALSE
##Wound washout is TRUE or FALSE
##Wound explantation is TRUE or FALSE
##Wound reimplantation is TRUE or FALSE

InfectionData = read.xlsx(PatientsWithComplicationsFile, sheet = 1)
ConfirmedInfections = InfectionData[InfectionData$Confirmed.Infection,]
ConfirmedInfections$Blood.cultures[ConfirmedInfections$Blood.cultures == 'Nil'] = FALSE
ConfirmedInfections$Blood.cultures[is.na(ConfirmedInfections$Blood.cultures) | ConfirmedInfections$Blood.cultures == 'Nil taken'] = NA
ConfirmedInfections$Blood.cultures[(!is.na(ConfirmedInfections$Blood.cultures) & ConfirmedInfections$Blood.cultures != FALSE)] = TRUE

ConfirmedInfections$'Wound.MC&S'[ConfirmedInfections$'Wound.MC&S' == 'Nil' | ConfirmedInfections$'Wound.MC&S' == 'Nil growth' ] = FALSE
ConfirmedInfections$'Wound.MC&S'[is.na(ConfirmedInfections$'Wound.MC&S')] = NA
ConfirmedInfections$'Wound.MC&S'[(!is.na(ConfirmedInfections$'Wound.MC&S') & ConfirmedInfections$'Wound.MC&S' != FALSE)] = TRUE

ConfirmedInfections$'Fever.>=.38C'[is.na(ConfirmedInfections$'Fever.>=.38C')]= FALSE

ConfirmedInfections$'IV.Antibiotics'[!is.na(ConfirmedInfections$'IV.Antibiotics')] = TRUE
ConfirmedInfections$'IV.Antibiotics'[is.na(ConfirmedInfections$'IV.Antibiotics')] = FALSE

ConfirmedInfections$'PO.antibiotics'[!is.na(ConfirmedInfections$'PO.antibiotics')] = TRUE
ConfirmedInfections$'PO.antibiotics'[is.na(ConfirmedInfections$'PO.antibiotics')] = FALSE

##all the surgical pips are converted to TRUE / FALSE

for (t in 20:25){
  ConfirmedInfections[is.na(ConfirmedInfections[t]), t] = FALSE
}
UniquePatientList4 = UniquePatientList3
UpdateComplication = function(PatientMRN, ComplicationColumnName){
  ##first, check if that complication column exists in uniquepatientlist; if it doesn't its false
  ##if column exists, get the flag from uniquepatientlist4
  ##get the flag from the CleanedComplications dataframe
  ##if either flag is true, change flag to true
  
  if (! (ComplicationColumnName %in% colnames(UniquePatientList4) )){
    ComplicationFlag = FALSE
  }
  else{
    ComplicationFlag = UniquePatientList4[UniquePatientList4$MRN == PatientMRN, ComplicationColumnName]
  }
  ComplicationFlag2 = any(CleanedComplications[CleanedComplications$UMRN==PatientMRN, ComplicationColumnName], na.rm=TRUE)
  return (ComplicationFlag | ComplicationFlag2)
}
CleanedComplications = ConfirmedInfections
for (t in 15:25){
  if (! (colnames(ConfirmedInfections[t]) %in% colnames(UniquePatientList4) ) ){
    #print (paste('the column ', colnames(ConfirmedInfections[t]), ' is in UniquePatientList4'))
    UniquePatientList4[,colnames(ConfirmedInfections[t])] = FALSE
  }
  UniquePatientList4[,colnames(ConfirmedInfections[t])] = sapply(UniquePatientList4$MRN, UpdateComplication, colnames(ConfirmedInfections[t]) )
}

##Wound dehiscence
##all the surgical pips (washout, revision, debridement, evacuation, explantation, reimplantation)
## are going to be TRUE / FALSE

DehiscenceData = read.xlsx(PatientsWithComplicationsFile, sheet = 2)
ConfirmedDehiscence = DehiscenceData[DehiscenceData$Confirmed.Wound.Dehiscence,]
for (t in 14:19){
  ConfirmedDehiscence[is.na(ConfirmedDehiscence[t]), t] = FALSE
}

CleanedComplications = ConfirmedDehiscence

for (t in 14:19){
  if (! (colnames(CleanedComplications[t]) %in% colnames(UniquePatientList4) ) ){
    #print (paste('the column ', colnames(ConfirmedInfections[t]), ' is in UniquePatientList4'))
    UniquePatientList4[,colnames(CleanedComplications[t])] = FALSE
  }
  UniquePatientList4[,colnames(CleanedComplications[t])] = sapply(UniquePatientList4$MRN, UpdateComplication, colnames(CleanedComplications[t]) )
}


##Haematoma / hemorrhage
##All surgical pips are TRUE/FALSE. if All surgical pips are FALSE then it was managed conservatively


HaematomaData = read.xlsx(PatientsWithComplicationsFile, sheet = 3)
ConfirmedHaematoma = HaematomaData[HaematomaData$Confirmed.haematoma,]
for (t in 20:25){
  ConfirmedHaematoma[is.na(ConfirmedHaematoma[t]), t] = FALSE
  ConfirmedHaematoma[ConfirmedHaematoma[t] == 'Yes', t] = TRUE
  ConfirmedHaematoma[ConfirmedHaematoma[t] == 'Yes irrigation', t] = TRUE
  ConfirmedHaematoma[ConfirmedHaematoma[t] == "No", t] = FALSE
  
}

CleanedComplications = ConfirmedHaematoma

for (t in 20:25){
  if (! (colnames(CleanedComplications[t]) %in% colnames(UniquePatientList4) ) ){
    #print (paste('the column ', colnames(ConfirmedInfections[t]), ' is in UniquePatientList4'))
    UniquePatientList4[,colnames(CleanedComplications[t])] = FALSE
  }
  UniquePatientList4[,colnames(CleanedComplications[t])] = sapply(UniquePatientList4$MRN, UpdateComplication, colnames(CleanedComplications[t]) )
}


##Mechanical Complication
## Skipped cardiac arrest because there's no extra data points we collected for those

MechanicalCxData = read.xlsx(PatientsWithComplicationsFile, sheet = 5)
ConfirmedMechanicalCx = MechanicalCxData[MechanicalCxData$Confirmed.mechanical.complication,]
for (t in 16:19){
  ConfirmedMechanicalCx[is.na(ConfirmedMechanicalCx[t]), t] = FALSE
}

CleanedComplications = ConfirmedMechanicalCx

for (t in 16:19){
  if (! (colnames(CleanedComplications[t]) %in% colnames(UniquePatientList4) ) ){
    #print (paste('the column ', colnames(ConfirmedInfections[t]), ' is in UniquePatientList4'))
    UniquePatientList4[,colnames(CleanedComplications[t])] = FALSE
  }
  UniquePatientList4[,colnames(CleanedComplications[t])] = sapply(UniquePatientList4$MRN, UpdateComplication, colnames(CleanedComplications[t]) )
}

##Pneumothorax
##Either a drain was inserted, or it wasn't - ie conservative mx

PTXData = read.xlsx(PatientsWithComplicationsFile, sheet = 6)
ConfirmedPTX = PTXData[PTXData$Confirmed.Pneumothorax,]
for (t in 16){
  ConfirmedPTX[is.na(ConfirmedPTX[t]), t] = FALSE
  ConfirmedPTX[ConfirmedPTX[t] == 'FALSE ', t] = FALSE
  ConfirmedPTX[ConfirmedPTX[t] == 'TRUE ', t] = TRUE
  ConfirmedPTX[,t] = as.logical(ConfirmedPTX[,t])
}

CleanedComplications = ConfirmedPTX

for (t in 16){
  if (! (colnames(CleanedComplications[t]) %in% colnames(UniquePatientList4) ) ){
    #print (paste('the column ', colnames(ConfirmedInfections[t]), ' is in UniquePatientList4'))
    UniquePatientList4[,colnames(CleanedComplications[t])] = FALSE
  }
  UniquePatientList4[,colnames(CleanedComplications[t])] = sapply(UniquePatientList4$MRN, UpdateComplication, colnames(CleanedComplications[t]) )
}

##Accidental puncture / laceration
##skipped the carditis because no complications detected
## surgical pips, true or false

PunctureData = read.xlsx(PatientsWithComplicationsFile, sheet = 8)
ConfirmedPuncture = PunctureData[PunctureData$Confirmed.accidental.puncture,]
for (t in 14:18){
  ConfirmedPuncture[is.na(ConfirmedPuncture[t]), t] = FALSE

}

CleanedComplications = ConfirmedPuncture

for (t in 14:18){
  if (! (colnames(CleanedComplications[t]) %in% colnames(UniquePatientList4) ) ){
    #print (paste('the column ', colnames(ConfirmedInfections[t]), ' is in UniquePatientList4'))
    UniquePatientList4[,colnames(CleanedComplications[t])] = FALSE
  }
  UniquePatientList4[,colnames(CleanedComplications[t])] = sapply(UniquePatientList4$MRN, UpdateComplication, colnames(CleanedComplications[t]) )
}

##Pericardial effusion / tamponade
##patient either had pericardiocentesis, or didn't

EffusionData = read.xlsx(PatientsWithComplicationsFile, sheet = 9)
ConfirmedEffusion = EffusionData[EffusionData$Confirmed.pericardial.effusion.or.tamponade,]
for (t in 18){
  ConfirmedEffusion[is.na(ConfirmedEffusion[t]), t] = FALSE
  
}

CleanedComplications = ConfirmedEffusion

for (t in 18){
  if (! (colnames(CleanedComplications[t]) %in% colnames(UniquePatientList4) ) ){
    #print (paste('the column ', colnames(ConfirmedInfections[t]), ' is in UniquePatientList4'))
    UniquePatientList4[,colnames(CleanedComplications[t])] = FALSE
  }
  UniquePatientList4[,colnames(CleanedComplications[t])] = sapply(UniquePatientList4$MRN, UpdateComplication, colnames(CleanedComplications[t]) )
}

##Twiddler Syndrome
##patient either had pericardiocentesis, or didn't

EffusionData = read.xlsx(PatientsWithComplicationsFile, sheet = 9)
ConfirmedEffusion = EffusionData[EffusionData$Confirmed.pericardial.effusion.or.tamponade,]
for (t in 18){
  ConfirmedEffusion[is.na(ConfirmedEffusion[t]), t] = FALSE
  
}

CleanedComplications = ConfirmedEffusion

for (t in 18){
  if (! (colnames(CleanedComplications[t]) %in% colnames(UniquePatientList4) ) ){
    #print (paste('the column ', colnames(ConfirmedInfections[t]), ' is in UniquePatientList4'))
    UniquePatientList4[,colnames(CleanedComplications[t])] = FALSE
  }
  UniquePatientList4[,colnames(CleanedComplications[t])] = sapply(UniquePatientList4$MRN, UpdateComplication, colnames(CleanedComplications[t]) )
}

##make some CSVs
write.csv(UniquePatientList4,paste('clean data/' , Sys.Date(), '_JEDI_cleaned-data_identified.csv', sep=""), na="")
UniquePatientList5 = UniquePatientList4
UniquePatientList5$GenID = 1:nrow(UniquePatientList5)

GenIDToMRNIndex = UniquePatientList5[,c(46,1:6)]
write.csv(GenIDToMRNIndex,paste('clean data/' , Sys.Date(), '_JEDI_GENID-to-MRN-index.csv', sep=""), na="")

UniquePatientList5 = UniquePatientList5[,c(46, 5:45)]
for (t in 4:14){
  UniquePatientList5[UniquePatientList5[t] == TRUE,t] = 1
  UniquePatientList5[UniquePatientList5[t] == FALSE,t] = 0
}

for (t in 16:42){
  UniquePatientList5[UniquePatientList5[t] == TRUE,t] = 1
  UniquePatientList5[UniquePatientList5[t] == FALSE,t] = 0
}

write.csv(UniquePatientList5,paste('clean data/' , Sys.Date(), '_JEDI_cleaned-data_deidentified.csv', sep=""), na="")


#write.csv(UniquePatientList4, 'clean data')

