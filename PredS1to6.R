##The open datasets for OULAD can be downloaded from the link [https://analyse.kmi.open.ac.uk/#open-dataset] ## 

setwd("~/Synopsis/OU-Stuff/anonymisedData/OU_Demographics/AAA2013J")

## Data preparation ##

studentInfo <- read.csv("studentInfo.csv")
studentRegistration <- read.csv("studentRegistration.csv")
studentInfo <- data.table(studentInfo)
studentRegistration <- data.table(studentRegistration)
stdRegAAA2013J <-  studentRegistration[code_module=="AAA" & code_presentation=="2013J" & is.na(date_unregistration)]
studentInfoAAA2013 <- studentInfo[code_module=="AAA" & code_presentation=="2013J"]
setkey(stdRegAAA2013J, id_student)
setkey(studentInfoAAA2013, id_student)
S6data<- studentInfoAAA2013[stdRegAAA2013J, nomatch=0 ]
S6data <- data.table(S6data)
TMA1AAA2013J <- read.csv("TMA1AAA2013J.csv")
TMA1AAA2013J<- data.table(TMA1AAA2013J)
stdRegAAA2013<- studentRegistration[code_module == "AAA" & code_presentation=="2013J" & is.na(date_unregistration)]
setkey(TMA1AAA2013J, id_student)
setkey(stdRegAAA2013, id_student)
setkey(studentInfoAAA2013, id_student)
roughData<- studentInfoAAA2013[stdRegAAA2013, nomatch=0]
setkey(roughData, id_student)
S1data<- roughData[TMA1AAA2013J, nomatch=0]
S16data <- S6data[S1data, nomatch=0]
S16 <- data.table(S16data)
S16<- S16data[, .(gender, region, highest_education, imd_band, age_band, num_of_prev_attempts, disability, TMA1Result, final_result)]
TMA2AAA2013J <- read.csv("TMA2AAA2013J.csv")
TMA2AAA2013J<- data.table(TMA2AAA2013J)
setkey(TMA2AAA2013J, id_student)
S2data<- roughData[TMA2AAA2013J, nomatch=0]
S16data <- S6data[S1data, nomatch=0]
S126data <- S16data[S2data, nomatch=0]
S126 <- data.table(S126data)
S126<- S126[, .(gender, region, highest_education, imd_band, age_band, num_of_prev_attempts, disability, TMA1Result, TMA2Result, final_result)]
TMA3AAA2013J <- read.csv("TMA3AAA2013J.csv")
TMA3AAA2013J<- data.table(TMA3AAA2013J)
setkey(TMA3AAA2013J, id_student)
S3data<- roughData[TMA3AAA2013J, nomatch=0]
S16data <- S6data[S1data, nomatch=0]
S126data <- S16data[S2data, nomatch=0]
S1236data <- S126data[S3data, nomatch=0]
S1236 <- data.table(S1236data)
S1236 <- data.table(S1236data)
S1236<- S1236[, .(gender, region, highest_education, imd_band, age_band, num_of_prev_attempts, disability, TMA1Result, TMA2Result, TMA3Result, final_result)]
TMA4AAA2013J <- read.csv("TMA4AAA2013J.csv")
TMA4AAA2013J<- data.table(TMA4AAA2013J)
setkey(TMA4AAA2013J, id_student)
S4data<- roughData[TMA4AAA2013J, nomatch=0]
S16data <- S6data[S1data, nomatch=0]
S126data <- S16data[S2data, nomatch=0]
S1236data <- S126data[S3data, nomatch=0]
S1236 <- data.table(S1236data)
S12346data <- S1236data[S4data, nomatch=0]
S12346 <- data.table(S12346data)
S12346<- S12346[, .(gender, region, highest_education, imd_band, age_band, num_of_prev_attempts, disability, TMA1Result, TMA2Result, TMA3Result, TMA4Result, final_result)]
TMA5AAA2013J <- read.csv("TMA5AAA2013J.csv")
TMA5AAA2013J<- data.table(TMA5AAA2013J)
setkey(TMA5AAA2013J, id_student)
S5data<- roughData[TMA5AAA2013J, nomatch=0]
S16data <- S6data[S1data, nomatch=0]
S126data <- S16data[S2data, nomatch=0]
S1236data <- S126data[S3data, nomatch=0]
S1236 <- data.table(S1236data)
S12346data <- S1236data[S4data, nomatch=0]
S12346 <- data.table(S12346data)
S123456data <- S12346data[S5data, nomatch=0]
S123456 <- data.table(S123456data)

S1to6<- S123456[, .(gender, region, highest_education, imd_band, age_band, num_of_prev_attempts, disability, TMA1Result, TMA2Result, TMA3Result, TMA4Result, TMA5Result, final_result)]

## Renaming ##

colnames(S1to6)<- c("Gender", "Region", "Education", "IMD", "Age", "PrevAttempts", "Disability", "TMA1Result", "TMA2Result", "TMA3Result", "TMA4Result", "TMA5Result", "FinalResult")
colnames(S1to6)
levels(S1to6$Region)
levels(S1to6$Region)<- c("EA", "EM", "Ir", "L", "N", "NW", "Sc", "SE", "S", "SW", "W", "WM", "Y")
levels(S1to6$Education)
levels(S1to6$Education) <- c("A", "HE", "LA", "NF", "PG")
levels(S1to6$IMD)
levels(S1to6$IMD)<- c("NA", "L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L9", "L10")
levels(S1to6$Age)
levels(S1to6$Age)<- c("A1", "A2", "A3")

## Pie graphs for each attribute ##

g<- table(S1to6$Gender)
pie(g)
r<- table(S1to6$Region)
pie(r)
e<- table(S1to6$Education)
pie(e)
i<- table(S1to6$IMD)
pie(i)
a<- table(S1to6$Age)
pie(a)
d<- table(S1to6$Disability)
pie(d)
re<- table(S1to6$FinalResult)
pie(re)

## Decision tree for Step S6 (FinalResult) + Prediction accuracy ##

Finaldata<- data.table(S1to6)

TreeS6<- rpart(FinalResult ~ Gender+Region+Education+IMD+Age+Disability, data=Finaldata[1:200, ], 
               method = "class", control=rpart.control(minsplit=9, minbucket=3, cp=0.0001))
TreeS6
rpart.plot(TreeS6, type = 3, extra = 101, fallen.leaves = T)
rpart.plot(TreeS6)
summary(TreeS6)

PredS6<- predict(TreeS6, Finaldata[201:289, ], type = "class")
str(Finaldata)
actualValues <- Finaldata[201:289, FinalResult]

table(actualValues, PredS6)

##########################

## Decision tree for Step S1 (TMA1Result) + Prediction accuracy ##

Finaldata<- data.table(S1to6)

TreeS1<- rpart(TMA1Result ~ Gender+Region+Education+IMD+Age+Disability, data=Finaldata[1:200, ], 
               method = "class", control=rpart.control(minsplit=12, minbucket=3, cp=0.001))
TreeS1
rpart.plot(TreeS1, type = 3, extra = 101, fallen.leaves = T)
rpart.plot(TreeS1)
summary(TreeS1)

PredS1<- predict(TreeS1, Finaldata[201:289, ], type = "class")
str(Finaldata)
actualValues <- Finaldata[201:289, TMA1Result]

table(actualValues, PredS1)


## Decision tree for Step S2 (TMA2Result) + Prediction accuracy ##

Finaldata<- data.table(S1to6)

TreeS2<- rpart(TMA2Result ~ Gender+Region+Education+IMD+Age+Disability, data=Finaldata[1:200, ], 
               method = "class", control=rpart.control(minsplit=100, minbucket=3, cp=0.001))
TreeS2
rpart.plot(TreeS2, type = 3, extra = 101, fallen.leaves = T)
rpart.plot(TreeS2)
summary(TreeS2)

PredS2<- predict(TreeS2, Finaldata[201:289, ], type = "class")
str(Finaldata)
actualValues <- Finaldata[201:289, TMA2Result]

table(actualValues, PredS2)


## Decision tree for Step S3 (TMA3Result) + Prediction accuracy ##

Finaldata<- data.table(S1to6)

TreeS3<- rpart(TMA3Result ~ Gender+Region+Education+IMD+Age+Disability, data=Finaldata[1:200, ], 
               method = "class", control=rpart.control(minsplit=12, minbucket=3, cp=0.001))
TreeS3
rpart.plot(TreeS3, type = 3, extra = 101, fallen.leaves = T)
rpart.plot(TreeS3)
summary(TreeS3)

PredS3<- predict(TreeS3, Finaldata[201:289, ], type = "class")
str(Finaldata)
actualValues <- Finaldata[201:289, TMA3Result]

table(actualValues, PredS3)


## Decision tree for Step S4 (TMA4Result) + Prediction accuracy ##

Finaldata<- data.table(S1to6)

TreeS4<- rpart(TMA4Result ~ Gender+Region+Education+IMD+Age+Disability, data=Finaldata[1:200, ], 
               method = "class", control=rpart.control(minsplit=21, minbucket=9, cp=0.01))
TreeS4
rpart.plot(TreeS4, type = 3, extra = 101, fallen.leaves = T)
rpart.plot(TreeS4)
summary(TreeS4)

PredS4<- predict(TreeS4, Finaldata[201:289, ], type = "class")
str(Finaldata)
actualValues <- Finaldata[201:289, TMA4Result]

table(actualValues, PredS4)

## Decision tree for Step S5 (TMA5Result) + Prediction accuracy ##

Finaldata<- data.table(S1to6)

TreeS5<- rpart(TMA5Result ~ Gender+Region+Education+IMD+Age+Disability, data=Finaldata[1:200, ], 
               method = "class", control=rpart.control(minsplit=11, minbucket=4, cp=0.001))
TreeS5
rpart.plot(TreeS5, type = 3, extra = 101, fallen.leaves = T)
rpart.plot(TreeS5)
summary(TreeS5)

PredS5<- predict(TreeS5, Finaldata[201:289, ], type = "class")
str(Finaldata)
actualValues <- Finaldata[201:289, TMA5Result]

table(actualValues, PredS5)
