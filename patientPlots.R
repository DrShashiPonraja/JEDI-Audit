PatientList = read.csv('clean data/Patient List.csv')

count(PatientList$Sex)
pie(count(PatientList$Sex)$freq)
ageFreq = count(PatientList$Age)
plot(ageFreq)

PatientList
