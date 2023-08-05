library(rpart)
library(rpart.plot)

####################################################################
#                         SCRIPT Binary trees
####################################################################


## Data set train / test

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
DBtrain$ClaimFreq <- DBtrain$Nbclaims / DBtrain$Exposure

DBtrain$DriverAge <- CutQ(DBtrain$DriverAge, breaks = quantile(DBtrain$DriverAge, seq(0, 1, by = 0.25), na.rm = TRUE))
DBtrain$CarAge <- CutQ(DBtrain$CarAge, breaks = quantile(DBtrain$CarAge, seq(0, 1, by = 0.25), na.rm = TRUE))


set.seed(87031800)

##DBtrain$DriverAge <- CutQ(DBtrain$DriverAge, breaks = quantile(DBtrain$DriverAge, seq(0, 1, by = 0.25), na.rm = TRUE))
##DBtrain$CarAge <- CutQ(DBtrain$CarAge, breaks = quantile(DBtrain$CarAge, seq(0, 1, by = 0.25), na.rm = TRUE))

smp_size <- floor(0.8 * nrow(DBtrain))
train.idx <- sample(seq_len(nrow(DBtrain)), size = smp_size)
train <- DBtrain[train.idx, ]
test <-  DBtrain[-train.idx, ]
Y <- test[,11]
test <- test[, -11]

## partition 

d.tree <- rpart(cbind(Exposure,Nbclaims) ~ Gender + DriverAge + CarAge + Area + 
                  Leasing + Power + Fract + Contract + Fuel, data=train, method="poisson", parms=list(shrink=1), control=
                   rpart.control(cp=0.001, xval=10))

lambdapred <- exp(predict(d.tree, test))
lambdapred
NBpred <- lambdapred * test$Exposure
NBpred
plot(Y$Nbclaims, NBpred)
rsq.rpart(d.tree)
rpart.plot(d.tree, main = "Regression tree train set, cp=0.001")
plotcp(d.tree, lty = 4, col=2, upper = c("split"))
d.tree



## Overfitting

d.tree2 <- rpart(cbind(Exposure,Nbclaims) ~ Gender + DriverAge + CarAge + Area + 
                  Leasing + Power + Fract + Contract + Fuel, data=train, method="poisson", parms=list(shrink=1), control=
                  rpart.control(cp=0.0009, xval = 10))

plot(predict(d.tree2), train$ClaimFreq)
rsq.rpart(d.tree2)
plotcp(d.tree2)
d.tree2


test = data.frame()
for (age in seq(18,80,1)) {
  tmp<-data.frame(DriverAge=age,Gender=factor(2),Area=factor(4),
                  Leasing=factor(2),CarAge=3, Contract=factor(3), Exposure=1, Fuel=factor(2), Power=factor(3), Fract= factor(1))
  test<-rbind(test,tmp)
}

lambda1<-predict(d.tree,test)
lambda2<-predict(d.tree2,test)

matplot(seq(18,80,1), cbind(lambda1,lambda2),type="l",col=c("red","blue"),
        lty=c(1,1),xlab="DriverAge",ylab="lambda", main = "Visualisation of overfitting tree")
legend(21,0.032,c("Women cp=0.0009", "Women cp=0.001"),lty=c(1,1), col=c("red","blue"))


















