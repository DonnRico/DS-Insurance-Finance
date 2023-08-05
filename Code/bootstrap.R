install.packages("remotes")
remotes::install_github("fpechon/rfCountData")
library(rfCountData)



####################################################################
#                         SCRIPT Bootstrap
####################################################################



xtopredict <- DBtrain[1:50000,-c(10,11)]

test <- data.frame()
for (age in seq(20,60,2)) {
  tmp<-data.frame(Gender=factor(2),DriverAge=age,CarAge=3,Area=factor(4),
                  Leasing=factor(2), Power=factor(3), Fract=factor(1), Contract=factor(3),Fuel=factor(2), Exposure=1)
  test<-rbind(test,tmp)
}
exposure<- test$Exposure
test <- test[-7]

RandomForest <- rfPoisson(y = DBtrain[1:50000,]$Nbclaims, offset = log(DBtrain[1:50000,]$Exposure), x = xtopredict, ntree = 50)

prediction <- predict(RandomForest, test, offset = log(exposure))
