library(openxlsx)
library(plyr)
## produces a list of all equipment implanted at JHC, and its brand
## so Kim Fendel can label it all


DeviceInfoFile = 'raw data/Device Information.xlsx'
DeviceInfo = read.xlsx(DeviceInfoFile, sheet = 1)
for (d in 2:5){
  dat<-read.xlsx(DeviceInfoFile, sheet = d)
  DeviceInfo = rbind(DeviceInfo, dat)
}
levels(factor(DeviceInfo$Equipment))
DeviceTypeList = count(DeviceInfo$'Equipment')

names(DeviceTypeList)[1] = 'Device Name'
DeviceTypeList$'Device Type' = ""


UniqueDevice = DeviceInfo[!duplicated(DeviceInfo$'Equipment'),]
z = merge(DeviceTypeList, UniqueDevice, by.x = 'Device Name', by.y = 'Equipment' )

keeps = c('Device Name', 'freq', 'Vendor', 'Device Type')

write.csv(z[keeps], file='clean data/Device Type List - unlabelled.csv', row.names = FALSE)

