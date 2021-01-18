#Takes string and removes commas and spaces, convets to number
Numberize <- function(inputVector){
  inputVector <- gsub(",","",inputVector)
  inputVector <- gsub(" ","",inputVector)
  return(as.numeric(inputVector))
}

#pull US2010 Census Data into a DataFrame
readCensus <- function(){
testMunge <- read.csv(url("https://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"))
testMunge <- testMunge[-1:-8,1:5]
testMunge <- testMunge[-52:-58,]
colnames(testMunge) <- c("stateName","april10census","april10base","july10pop","july11pop")
testMunge$stateName <- gsub("\\.","",testMunge$stateName)
testMunge$april10census <- Numberize(testMunge$april10census)
testMunge$april10base <- Numberize(testMunge$april10base)
testMunge$july10pop <- Numberize(testMunge$july10pop)
testMunge$july11pop <- Numberize(testMunge$july11pop)
rownames(testMunge) <- NULL
return(testMunge)}

USstatePops <- readCensus()
USstatePops$april10census[1:3]