library(openxlsx)
TestFile = 'raw data/Test.xlsx'
## some code that can combine a set of excel sheets with partly matching columns

rbind.all.columns <- function(x, y) {
  
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  
  x[, c(as.character(y.diff))] <- NA
  
  y[, c(as.character(x.diff))] <- NA
  
  return(rbind(x, y))
}

TestInfo = read.xlsx(TestFile, sheet = 1)
for (d in 2:4){
  dat<-read.xlsx(TestFile, sheet = d)
  TestInfo = rbind.all.columns(TestInfo, dat)
}
TestInfo
