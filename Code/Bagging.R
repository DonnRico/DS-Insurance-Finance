
####################################################################
#                         SCRIPT Bagging
####################################################################




DBtrain <- read_delim("DBtrain.csv", delim = ";", 
                      escape_double = FALSE, trim_ws = TRUE)

DBtrain <- DBtrain[,-1]
DBtrain$Gender <- as.factor(DBtrain$Gender)
DBtrain$Area <- as.factor(DBtrain$Area)
DBtrain$Leasing <- as.factor(DBtrain$Leasing)
DBtrain$Power <- as.factor(DBtrain$Power)

DBtrain$Fract <- as.factor(DBtrain$Fract)
DBtrain$Contract <- as.factor(DBtrain$Contract)
DBtrain$Fuel <- as.factor(DBtrain$Fuel)

set.seed(87031800)

smp_size <- floor(0.8 * nrow(DBtrain))
train.idx <- sample(seq_len(nrow(DBtrain)), size = smp_size)
train <- DBtrain[train.idx, ]
test <-  DBtrain[-train.idx, ]
Y <- test[,11]
test <- test[, -11]

M <- 200
nr <- nrow(DBtrain)
size = 10000
lambdaW <- rep(0, nrow(test))
lambdaM <- matrix(0 , M,nrow(test))
DriverAgeM    <- matrix(0 , M,nrow(test))
for (ct in c(1:M)) {
  tmp <-sample(nr, size, replace = TRUE, prob = NULL)
  basetmp <-DBtrain[tmp,]
  d.tree <- rpart(cbind(Exposure,Nbclaims) ~ Gender + DriverAge + CarAge + Area + 
                    Leasing + Power + Fract + Contract + Fuel, data=DBtrain, method="poisson", parms=list(shrink=1), control=
                    rpart.control(cp=0.0005, xval=10))
  lambdaM[ct,]<-lambdaK+predict(d.tree,test)
  DriverAgeM[ct,] <-seq(20,60,2)
}
matplot(DriverAgeM[ct,],lambdaM[ct,],type="l",
        lty=c(1,1),xlab="DriverAge",ylab="lambda", main = "Visualisation of overfitting tree")
legend(19,0.01,c("Women cp=0.0005", "Women cp=0.0001"),lty=c(1,1), col=c("red","blue"))
