

####################################################################
#                         SCRIPT GBM                               #
####################################################################

library(latex2exp)

## Load the dataset, only perform features convertion

Dev.Poisson = function(y,e, yhat){
  lny     <- log(y)
  lny[lny==-Inf]<-0
  lnyhat  <- log(yhat)
  lny[lnyhat==-Inf]<-0
  devia   <- 2*e*(y*lny-y*lnyhat-y+yhat)
  devia   <- sum(devia)
}


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

set.seed(87031800)

smp_size <- floor(0.8 * nrow(DBtrain))
train.idx <- sample(seq_len(nrow(DBtrain)), size = smp_size)
train <- DBtrain[train.idx, ]
test <-  DBtrain[-train.idx, ]
Y <- test[,11]
test <- test[, -11]

boosting <- gbm(Nbclaims~offset(log(Exposure)) + DriverAge + Gender + Area + Fract +
                  CarAge + Leasing + Power + Fuel + Contract, distribution = "poisson", data = train,
                cv.folds = 5, n.cores = 4, shrinkage = 0.001, n.trees = 7000)
summary(boosting)

plot.gbm(boosting, i.var = "DriverAge", type = "response", main = "Partial Effect of Driver Age",
         ylab = "Effect on lambda", xlab = "DriverAge")

plot.gbm(boosting, i.var = "CarAge", type = "response", main = "Partial Effect of Car Age",
         ylab = "Effect on lambda", xlab = "CarAge")

plot.gbm(boosting, i.var = "Area", type = "response", main = "Partial Effect of Area",
         ylab = "Effect on lambda")
plot.gbm(boosting, i.var = "Leasing", type = "response", main = "Partial Effect of Leasing",
         ylab = "Effect on lambda")
plot.gbm(boosting, i.var = "Fract", type = "response", main = "Partial Effect of Fract",
         ylab = "Effect on lambda")
plot.gbm(boosting, i.var = "Power", type = "response", main = "Partial Effect of Power",
         ylab = "Effect on lambda")
plot.gbm(boosting, i.var = "Gender", type = "response", main = "Partial Effect of Gender",
         ylab = "Effect on lambda")
plot.gbm(boosting, i.var = "Fuel", type = "response", main = "Partial Effect of Fuel",
         ylab = "Effect on lambda")
plot.gbm(boosting, i.var = "Contract", type = "response", main = "Partial Effect of Contract",
         ylab = "Effect on lambda")


gbm.best.oob = gbm.perf(boosting,plot.it = TRUE, method = "OOB")
gbm.best.cv = gbm.perf(boosting,plot.it = TRUE, method = "cv")
gbm.best.test = gbm.perf(boosting,plot.it = TRUE, method = "test")


test1 = data.frame()
for (age in seq(20,80,2)) {
  tmp<-data.frame(Exposure=1, Gender=factor(2),DriverAge=age,CarAge=7,Area=factor(1),
                  Leasing=factor(2), Power=factor(1), Fract=factor(1), Contract=factor(3),Fuel=factor(1))
  test1<-rbind(test1,tmp)
}


lambdaK<-predict.gbm(boosting,test1)


test1 <- data.frame()
for (age in seq(20,80,2)) {
  tmp<-data.frame(Exposure=1, Gender=factor(1),DriverAge=age,CarAge=7,Area=factor(1),
                  Leasing=factor(2), Power=factor(1), Fract=factor(1), Contract=factor(3),Fuel=factor(1))
  test1<-rbind(test1,tmp)
}


lambdaM <- predict.gbm(boosting,test1)

plot(seq(20,80,2), exp(lambdaK),type="l",col=c("blue"),main="Expected Lambda, Women", xlab = "DriverAge")
plot(seq(20,80,2),exp(lambdaM),add=TRUE)
matplot(seq(20,80,2), cbind(exp(lambdaK),exp(lambdaM)),type="l",col=c("red","blue"),
        lty=c(1,1),xlab="DriverAge",ylab="lambda",main="Expected Lambda")
legend(30,0.0215,c("Women", "Men"),lty=c(1,1), col=c("red","blue"))

pred_train <- predict.gbm(boosting, train, type = "response")
pred_train
pred_test <- predict.gbm(boosting, test, type = "response")
pred_test

print(Dev.Poisson(test$ClaimFreq, test$Exposure, pred_test))
print(Dev.Poisson(train$ClaimFreq, train$Exposure, pred_train))
sum(pred_test)/length(pred_test)

## Calibration for train

to_comp = aggregate(x = train$ClaimFreq, FUN = function(x) c(mean=mean(x), n=length(x)),
                    by = list(Gender = train$Gender,
                              Area = train$Area,
                              Fract = train$Power))

to_comp2 = aggregate(x = pred_train, FUN = function(x) c(mean=mean(x), n=length(x)),
                     by = list(Gender = train$Gender,
                               Area = train$Area,
                               Fract = train$Power))
O = to_comp$x[,1]*to_comp$x[,2]
E = to_comp2$x[,1]*to_comp2$x[,2]
n = to_comp$x[,2]

chi = sum((O - E)^2/E)
pval = pchisq(chi, df=(nrow(to_comp)-2), lower.tail = F)
cat("C =", chi,"\n")
cat("p-value =", pval)

png(filename = "calibration_GBM_train.png", width = 7, height = 6, units = 'in', res=600)
plot(to_comp2$x[,1], to_comp$x[,1], pch=19, col = "red", xlim=c(0,0.05),ylim=c(0,0.05),
     main = "Group Calibration (GBM)", xlab = "Predicted Frequency", ylab="True Frequency")
abline(a=0,b=1,lty=3)
grid()

legend("topleft", legend = c(TeX(sprintf("$\\hat{C}$ = %.3f", round(chi, 3))),
                                     paste("p-value =", round(pval,3))),lty=c(0,0))
dev.off()


## Calibration for test

to_comp = aggregate(x = test$ClaimFreq, FUN = function(x) c(mean=mean(x), n=length(x)),
                    by = list(Gender = test$Gender,
                              Area = test$Area,
                              Fract = test$Fract))

to_comp2 = aggregate(x = pred_test, FUN = function(x) c(mean=mean(x), n=length(x)),
                     by = list(Gender = test$Gender,
                               Area = test$Area,
                               Fract = test$Fract))
O = to_comp$x[,1]*to_comp$x[,2]
E = to_comp2$x[,1]*to_comp2$x[,2]
n = to_comp$x[,2]

chi = sum((O - E)^2/E)
pval = pchisq(chi, df=(nrow(to_comp)-2), lower.tail = F)
cat("C =", chi,"\n")
cat("p-value =", pval)

png(filename = "calibration_GBM_test.png", width = 7, height = 6, units = 'in', res=600)
plot(to_comp2$x[,1], to_comp$x[,1], pch=16, col = "red", xlim=c(0,0.05),ylim=c(0,0.05),
     main = "Group Calibration (GBM)", xlab = "Predicted Frequency", ylab="True Frequency")
abline(a=0,b=1,lty=3)
grid()
legend("topleft", legend = c(TeX(sprintf("$\\hat{C}$ = %.3f", round(chi, 3))),
                             paste("p-value =", round(pval,3))),lty=c(0,0))
dev.off()



############################################################################################
#  !!Warning!! SCRIPT Prediction DBtest. Run only when other methods have been explored    #
############################################################################################



DBtest <- read_csv("DBtest.csv")
PredCF <- predict.gbm(boosting, DBtest, type = "response")
mean(PredCF)
DBtest <- cbind(DBtest,PredCF)
DBtest

write.csv2(DBtest,"/Users/aymeric/Desktop/Q1 2022/LDAT2310/project/DBtest.csv", row.names = FALSE)

