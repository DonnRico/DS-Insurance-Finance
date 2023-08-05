

####################################################################
#                         SCRIPT NeuralNet
####################################################################
library(readr)
library(ggplot2)
library(kableExtra)
library(Hmisc)
library(corrplot)
library(psych)
library(tweedie)
library(ggfortify)
library(statmod)
library(lme4)
library(tigerstats)
library(fitdistrplus)
library(actuar)
library(pscl)
library(DHARMa)
library(modelsummary)
library(MASS)
library(vcd)
library(AER)
library(car)
library(boot)
library(mgcv)
library(DescTools)
library(neuralnet)
library(dplyr)
library(data.table)
library(gbm)
library(pROC)
library(ggplot2)
library(patchwork)
library(circle)
library(ggforce)
library(nlme)
library(brms)
library(reticulate)

## reticulate::conda_create("r-miniconda-arm64")

reticulate::conda_list()


## list the pah of reticulate python ##

Sys.setenv(RETICULATE_PYTHON = "/Users/aymeric/Library/r-miniconda-arm64/bin/python")


####### Create python environment ##### Don't do this if it's already done

###### SET environment to the reticulate python env ########

Sys.getenv()

##### CONDA_PREFIX should point to r-reticulate ####

library(tensorflow)

library(LocalGLMnet)
tf$compat$v1$enable_eager_execution()

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

# Categ.to.Quant <- function(data,factor,removeLast=TRUE){
#   y = paste("data$",sep = "",factor)
#   x = eval(parse(text=y))
#   ndata = length(x)          #number of lines in the dataset
#   nlgen = length(levels(x))  #number of levels
#   if (!removeLast)
#   {nlgen = nlgen+1}  #number of levels
#   lev   = levels(x)
#   z     = matrix(0,ndata,nlgen-1)
#   nv    = vector("character",nlgen-1)
#   for (ct in 1:nlgen-1)
#   {
#     z[,ct] = ifelse(x==lev[ct],1,0)
#     nv[ct] = paste(factor,sep="",lev[ct])
#   }
#   colnames(z)=nv
#   #remove the column
#   data <- data[, ! names(data) %in% factor, drop = F]
#   data <- data.frame(data,z)
#   return(data)
# }
#
# DBtrain <-Categ.to.Quant(DBtrain,"Gender")
# DBtrain <-Categ.to.Quant(DBtrain,"Area")
# DBtrain <-Categ.to.Quant(DBtrain,"Leasing")
# DBtrain <-Categ.to.Quant(DBtrain,"Power")
# DBtrain <-Categ.to.Quant(DBtrain,"Fract")
# DBtrain <-Categ.to.Quant(DBtrain,"Fuel")
# DBtrain <-Categ.to.Quant(DBtrain,"Contract")

DBtrain <- Factor_transformation(DBtrain, "Gender")
DBtrain <- Factor_transformation(DBtrain,"Area")
DBtrain <- Factor_transformation(DBtrain,"Leasing")
DBtrain <- Factor_transformation(DBtrain,"Power")
DBtrain <- Factor_transformation(DBtrain,"Fract")
DBtrain <- Factor_transformation(DBtrain,"Fuel")
DBtrain <- Factor_transformation(DBtrain,"Contract")

DBtrain$ClaimFreq <- DBtrain$Nbclaims/DBtrain$Exposure

set.seed(87031800)
smp_size <- floor(0.8 * nrow(DBtrain))
train.idx <- sample(seq_len(nrow(DBtrain)), size = smp_size)
train <- DBtrain[train.idx, ]
test <-  DBtrain[-train.idx, ]

## standardization

train <- var_standardization(train, "DriverAge")
test <- var_standardization(test, "DriverAge")
train <- var_standardization(train, "CarAge")
test <- var_standardization(test, "CarAge")

# train$scaledDriverAge <- (train$DriverAge-min(train$DriverAge))/
#   (max(train$DriverAge)-min(train$DriverAge))
# train$scaledCarAge <- (train$CarAge-min(train$CarAge))/
#   (max(train$CarAge)-min(train$CarAge))
# train <- train[, -c(1,2)]
#
# test$scaledDriverAge <- (test$DriverAge-min(test$DriverAge))/
#   (max(test$DriverAge)-min(test$DriverAge))
# test$scaledCarAge <- (test$CarAge-min(test$CarAge))/
#   (max(test$CarAge)-min(test$CarAge))
# test <- test[, -c(1,2)]

## Ready for the neural et, 1 HL is convenient

source(file="DHneuralnetDeviance.r")
source(file="DHplot.nn.r")
source(file="DHcompute.r")
source(file="DHgwplot.r")

set.seed(87031800)
# NN1 <- neuralnet(formula= ClaimFreq~Scaled_DriverAge+Scaled_CarAge+Fuel1+Fuel2+Fuel3+Fract1+Fract2+Area1+
#                    Area2+Area3+Area4+Leasing1+Gender1+Power1+Power2+Power3,
#                     data=train,hidden=c(3,3,5,5,16), threshold=2,stepmax = 3000,
#                     lifesign = "full",lifesign.step = 100,err.fct="poisson")
#
# plot(NN1)

## training set

# LLmod1      <- logLike.EDF(NN1)
# sprintf("Log-likelihood, training set : %s",round(LLmod1$log.lik,5))
# sprintf("AIC, training set : %s",round(LLmod1$AIC,5))
# sprintf("BIC, training set : %s",round(LLmod1$BIC,5))

# covariateC1 <- cbind(train$scaledDriverAge,train$scaledCarAge,train$Fuel1,train$Fuel2,train$Fuel3,train$Fract1, train$Fract2,
#                      train$Area1,train$Area2,train$Area3,train$Area4,train$Leasing1,train$Gender1,train$Power1, train$Power2,train$Power3)
# testC1      <- compute(NN1,covariateC1,train$ClaimFreq,train$Exposure)
# sprintf("Deviance, training set : %s",round(testC1$devstat,5))

# est_lambda= sum(testC1$net.result*train$Exposure)/sum(train$Exposure)
# emp_lambda= sum(train$Nbclaims)/sum(train$Exposure)
# sprintf("Model estimated claim frequency, training set : %s",round(est_lambda,5))
# sprintf("Empirical claim frequency, training set : %s",round(emp_lambda,5))

## test set

# covariateT1 <- cbind(test$scaledDriverAge,test$scaledCarAge,test$Fuel1,test$Fuel2,test$Fuel3,test$Fract1, test$Fract2,
#                      test$Area1,test$Area2,test$Area3,test$Area4,test$Leasing1,test$Gender1,test$Power1, test$Power2,test$Power3)
# testT1  <- compute(NN1,covariateT1,test$ClaimFreq,test$Exposure)
# sprintf("Deviance, test set : %s",round(testT1$devstat,5))
#
# est_lambda= sum(testT1$net.result*test$Exposure)/sum(test$Exposure)
# emp_lambda= sum(test$Nbclaims)/sum(test$Exposure)
#
# sprintf("Model estimated claim frequency, test set : %s",round(est_lambda,5))
#
# sprintf("Empirical claim frequency, test set : %s",round(emp_lambda,5))
#
# NN1$weights
#
# ## plots
#
# gwplot(NN1)


####################################################################
#                         SCRIPT Keras
####################################################################

library(pracma)
library(tensorflow)
library(keras)
library(neuralnet)

loss_PoissonDeviance<-function(y_true,y_pred)
{
  # Like the Python functions, the custom loss functions for R need to
  # operate on tensor objects rather than R primitives. In order to perform
  # these operations, you need to get a reference to the backend using
  # backend(). In my system configuration, this returns a reference to tensorflow.

  K      <- backend()
  y      <- y_true[,1]
  nbl    <- as.integer(K$count_params(y))

  e      <- y_true[,2]
  yhat   <- y_pred

  lny     <- K$log(y)
  test    <- K$equal(y,0)
  zerot   <- K$zeros_like(y)
  lny     <- K$switch(test,zerot,lny)

  # Be careful to the dimension yhat shape=c(1000,1) whereas y has shape c(1000,?)
  # consequence: the product y*hat yields a 1000x1000 matrix
  lnyhat  <- K$log(yhat)
  test    <- K$less_equal(yhat,0)
  zerot   <- K$zeros_like(yhat)
  lnyhat  <- K$switch(test,zerot,lnyhat)

  devia <- 2*e*(y*lny-y*lnyhat[,1]-y+yhat[,1])
  return(K$sum(devia))
} #end loss_PoissonDeviance

# loss_GammaDeviance<-function(y_true,y_pred)
# {
#
#   # Like the Python functions, the custom loss functions for R need to
#   # operate on tensor objects rather than R primitives. In order to perform
#   # these operations, you need to get a reference to the backend using
#   # backend(). In my system configuration, this returns a reference to tensorflow.
#   K      <- backend()
#   y      <- y_true[,1]
#   nbl    <- as.integer(K$count_params(y))
#   e      <- y_true[,2]
#   yhat   <- y_pred
#
#   #Be careful to the dimension yhat shape=c(1000,1) whereas y has shape c(1000,?)
#   # consequence: the product y*hat yields a 1000x1000 matrix
#
#   yonyhat <-y/yhat[,1]
#   test    <- K$equal(yhat[,1],0)
#   zerot   <- K$zeros_like(yonyhat)
#   yonyhat <- K$switch(test,zerot,yonyhat)
#
#   lnyonyhat <- K$log(yonyhat)
#   zerot     <- K$zeros_like(lnyonyhat)
#   lnyonyhat <- K$switch(test,zerot,lnyonyhat)
#
#   devia <- 2*e*(yonyhat-1-lnyonyhat)
#
#   return(K$sum(devia))
# } #end loss_GammaDeviance
#
# print(aa)
# ################Function for the deviance############################
DevianceComp<-function(y,e,yhat,nb.weights,type.err)
{
  nb.data <-length(y)

  if (type.err=="gaussian")
  {
    devia   <- sum(e*(y - yhat)^2)
    mse     <- e*(y-yhat)^2
    phi     <- sum(mse)/(nb.data-nb.weights)
    sigma   <- sqrt(phi/e)
    log.lik <- sum(dnorm(y, mean = yhat, sd = sigma, log = TRUE))
  }
  else if  (type.err=="gamma")
  {
    yonyhat                         <- y/yhat
    yonyhat[is.infinite(yonyhat)]   <- 0
    logyonyhat                      <- log(yonyhat)
    logyonyhat[is.infinite(yonyhat)]<- 0
    devia<-sum(2*e*(yonyhat-1-log(logyonyhat)))
    mse     <- e*(y-yhat)^2
    phi     <- sum(mse/(yhat^2*(nb.data-nb.weights)))
    alphav  <- e/phi
    scalev  <- e/(phi*yhat)
    log.lik <- sum(dgamma(y, shape=alphav, rate = scalev, log = TRUE))
  }
  else if  (type.err=="poisson")
  {
    lny     <- log(y)
    lny[lny==-Inf]<-0
    lnyhat  <- log(yhat)
    lny[lnyhat==-Inf]<-0
    devia   <- 2*e*(y*lny-y*lnyhat-y+yhat)
    devia   <- sum(devia)
    phi     <- 1
    lamv    <- yhat*e
    log.lik <- sum(-log(fact(round(y*e)))+(y*e)*log(lamv)  -lamv)
  }
  else if (type.err=="binomial")
  {
    lny           <- log(y)
    ln1my         <- log(1-y)
    lnyhat        <- log(yhat)
    yhatm[yhat<0] <- 0
    ln1myhat      <- log(1-yhatm)
    lny   [is.infinite(lny)]         <- 0
    ln1my[is.infinite(ln1my)]        <- 0
    lnyhat[is.infinite(lnyhat)]      <- 0
    ln1myhat[is.infinite(ln1myhat)]  <- 0
    devia   <- sum(2*e*(y*lny-y*lnyhat+(1-y)*ln1my-(1-y)*ln1myhat)) #binomial deviance
    phi     <- 1
    log.lik <- sum(dbinom(y, e, yhat, log = TRUE))
  }
  else
  {
    stop("Error function must be: gaussian, poisson, gamma or binomial", call. = FALSE)
  }

  AIC <- 2*nb.weights-2*log.lik
  BIC <- nb.weights*log(nb.data)-2*log.lik
  list(Deviance=devia,log.lik=log.lik, AIC=AIC , BIC = BIC, phi=phi )

} #end DevianceComp
#
# #####################################################################
# #         Neural networks only accept quantitative variables
# #       the function ConvertFactor convert factors in quant var.
# #####################################################################
# Convert.Factor<-function(data,factor,removeLast=TRUE)
#   # convert a categorical variable in a set of binary variables,
#   # data: dataframe and factor : string
# {
#   y = paste("data$",sep = "",factor)
#   x = eval(parse(text=y))
#   ndata = length(x)          #number of lines in the dataset
#   nlgen = length(levels(x))  #number of levels
#   if (!removeLast)
#   {nlgen = nlgen+1}  #number of levels
#   lev   = levels(x)
#   z     = matrix(0,ndata,nlgen-1)
#   nv    = vector("character",nlgen-1)
#   for (ct in 1:nlgen-1)
#   {
#     z[,ct] = ifelse(x==lev[ct],1,0)
#     nv[ct] = paste(factor,sep="",lev[ct])
#   }
#   colnames(z)=nv
#   #remove the column
#   data <- data[, ! names(data) %in% factor, drop = F]
#   data <- data.frame(data,z)
#   return(data)
# }  #end Convert.Factor

# #####################################################################
# #   This procedure convert an quantitative variable into categories
# #####################################################################
# Quant.to.Categ<-function(data,varname,bins)
#   # data: dataframe, varname : string,
#   # bins : vector of ordered numbers
# {
#   name    <- paste("data$",sep = "",varname)
#   x       <- eval(parse(text=name))
#   if (min(x)<=min(bins)) {bins<-c(0,bins)}
#   if (max(x)>max(bins))  {bins<-c(bins,max(x)+1)}
#
#   catx    <- cut(x, breaks = bins)
#   catx    <- as.factor(catx)
#
#   nl      <- length(levels(catx))
#   lev     <- levels(catx)
#   nv      <- vector("character",nl)
#
#   for (ct in 1:nl)
#   {
#     nv[ct] <- paste(varname,sep=".",lev[ct])
#   }
#   #we remove the quantitative variabme
#   data <- data[, ! names(data) %in% varname, drop = F]
#
#   data    <- data.frame(data,catx)
#   names(data)[names(data) == 'catx'] <- varname
#
#   data    <- Convert.Factor(data,varname)
#   return(data)
# } #end Quant.to.Categ

## reLoad data

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

## standardization

DBtrain <- var_standardization(DBtrain, "DriverAge")
DBtrain <- var_standardization(DBtrain, "CarAge")


DBtrain$ClaimFreq <- DBtrain$Nbclaims/DBtrain$Exposure



DBtrain <-Factor_transformation(DBtrain,"Gender")
DBtrain <-Factor_transformation(DBtrain,"Area")
DBtrain <-Factor_transformation(DBtrain,"Leasing")
DBtrain <-Factor_transformation(DBtrain,"Power")
DBtrain <-Factor_transformation(DBtrain,"Fract")
DBtrain <-Factor_transformation(DBtrain,"Fuel")
DBtrain <-Factor_transformation(DBtrain,"Contract")

# LocalGLMnet(DBtrain, "ClaimFreq", "Exposure", c("Scaled_DriverAge", "Scaled_CarAge", "Gender1", "Area1", "Area2", "Area3", "Area4", "Leasing1", "Power1", "Power2", "Power3", "Fract1", "Fract2", "Fuel1", "Fuel2", "Fuel3", "Contract1", "Contract2"), 0.8, 1000,20000,TRUE)

namesnoregress <- c("Exposure","Nbclaims","Contract","ClaimFreq","DriverAge","CarAge")
namesregressor<-! names(DBtrain) %in% namesnoregress

namestmp <- c("scaledAge","scaledVecAge")

## train and test set / perform on train
set.seed(87031800)
smp_size <- floor(0.8 * nrow(DBtrain))
train.idx <- sample(seq_len(nrow(DBtrain)), size = smp_size)
train <- DBtrain[train.idx, ]
test <-  DBtrain[-train.idx, ]

x_train   <- train[,namesregressor]
x_train   <- data.matrix(x_train)
testons <- cbind(train[,"ClaimFreq"], train[,"Exposure"])
y_train   <- cbind(train$ClaimFreq ,train$Exposure)
nb.inputs <- ncol(x_train)
x_test  <- test[,namesregressor]
x_test   <- data.matrix(x_test)
y_test   <- cbind(test$ClaimFreq ,test$Exposure)

library(keras)
library(tensorflow)
######################################################################"
# #   JUSTE POUR AFFICHAGE
listcovariates<- colnames(x_train)
equation      <- paste("ClaimFreq~",listcovariates[1])
for (j in c(2:length(listcovariates))){
  equation=paste(equation,listcovariates[j],sep="+")
}
tmp       <- c("Exposure","Nbclaims","Gender","Leasing", "Fract")
namestmp  <-! names(train) %in% tmp
modelplot <- neuralnet(formula= equation,
                       data=DBtrain[,namestmp],hidden=c(8,4,8,16), threshold=2500000,stepmax = 1,
                       lifesign = "full",lifesign.step = 100)
quartz()

plot(modelplot, information = TRUE,radius=0.1,show.weights=FALSE,fontsize = 10,
     intercept=FALSE,dimension = c(12,12),col.hidden="red",col.entry="green",col.out="blue",
     col.hidden.synapse="cadetblue",col.entry.synapse="cadetblue",col.out.synapse ="cadetblue",
     arrow.length=0.12,file='test.jpg')

## 1 hidden layer with 4 neurons, ReLu activation


set.seed(87031800)

reticulate::py_config()
tensorflow::tf_config()

library(keras)

inputLayer <- layer_input(shape = c(nb.inputs), dtype = "float32")

Attention <-  inputLayer  %>%
  layer_dense(units = (2*nb.inputs)/3, activation = 'tanh', input_shape = c(nb.inputs)) %>%
  layer_dense(units = nb.inputs/3, activation = 'tanh') %>%
  layer_dense(units = nb.inputs/4, activation = 'tanh') %>%
  layer_dense(units = nb.inputs/4, activation = 'tanh') %>%
  layer_dense(units = nb.inputs/3, activation = 'tanh') %>%
  layer_dense(units = (2*nb.inputs)/3, activation = 'tanh') %>%
  layer_dense(units = nb.inputs, activation = 'linear', name= 'Attention')

Response <- list(inputLayer, Attention) %>%
  layer_dot(axes=1) %>%
  layer_dense(units = 1, activation = k_exp, name = 'Response')

model <- keras_model(inputs = c(inputLayer), outputs = c(Response))

model %>% compile(
  loss      = loss_PoissonDeviance,
  optimizer = 'nadam',
  metrics   = c('mse'))

summary(model)

## fit the model

history1 <- model %>% fit(
  x_train, y_train ,
  epochs = 100, batch_size = 20000, shuffle= TRUE)

## regression attention

zz <- keras_model(inputs=model$input, outputs=get_layer(model, 'Attention')$output)
beta.x <- data.frame(zz %>% predict(list(x_train)))
get_weights(model)
b0 <- as.numeric(get_layer(model, 'Response')$weights[[1]])
b0
beta.x <- beta.x * b0
beta.x
plot(model, show_shapes = TRUE, show_dtype = TRUE, show_layer_names = TRUE, show_layer_activations = TRUE)
save_model_weights_tf(model, "modelBenchRand", overwrite = TRUE)
load_model_weights_tf(model, "modelBenchRand")

## gradients

j <- 1
beta.j <- Attention %>% layer_lambda(function(x) x[,j])
model.j <- keras_model(inputs = c(inputLayer), outputs = c(beta.j))

tf$compat$v1$disable_eager_execution()

# ## fonctionne pas 1
#
# custom_layer <- function(x) k_gradients(model.j$outputs, model.j$inputs)
#
# grad <- beta.j %>% custom_layer
#
# ## fonctionne pas 2
#
# custom_layer <- layer_lambda(f=
#   function(x) k_backend$gradients(model.j$outputs, model.j$inputs)
# )
#
# grad <- beta.j %>% custom_layer
#
# ## fonctionne pas 3
#
# grad_layer <- layer_lambda(f=function(x) k_gradients(model.j$outputs, model.j$inputs))
# grad_fn <- keras_model(inputs=inputLayer, outputs=grad_layer(beta.j))
#
#
# model.grad <- keras_model(inputs = c(inputLayer), outputs = c(grad))
# grad.beta <- data.frame(model.grad %>% predict(as.matrix(x_train)))
#

## mémoire

grad <- beta.j %>% layer_lambda(function(x) k_gradients(model.j$outputs, model.j$inputs))
model.grad <- keras_model(inputs = c(Design), outputs = c(grad))
grad.beta <- data.frame(model.grad %>% predict(as.matrix(XX)))

## Compute the results

staMod1 <- DevianceComp(y_train[,1],y_train[,2], model1 %>%predict(x_train[,]),
                          nb.weights=count_params(model1), type.err="poisson")

staMod2 <- DevianceComp(y_test[,1],y_test[,2], model1 %>%predict(x_test[,]),
                        nb.weights=count_params(model1), type.err="poisson")
print(staMod1)
print(staMod2)

model1 %>% evaluate(x_train, y_train,verbose = 0)
model1 %>% evaluate(x_test, y_test,verbose = 0)

testtrain <- model1 %>% predict(x_train)
testvalid <- model1 %>% predict(x_test)

testtrain
testvalid

sum(testvalid)/length(testvalid)

## Calibration for train

## data to aggregate

to_comp = aggregate(x = train$ClaimFreq, FUN = function(x) c(mean=mean(x), n=length(x)),
                    by = list(Gender = train$Gender,
                              Area = train$Area,
                              Fract = train$Power))

to_comp$x[,"mean"]

train1$prediction <- testtrain

to_comp2 = aggregate(x = train1$prediction, FUN = function(x) c(mean=mean(x), n=length(x)),
                     by = list(Gender = train1$Gender,
                               Area = train1$Area,
                               Fract = train1$Power))

to_comp2$V1[,"mean"]
O = to_comp$x[,"mean"]*to_comp$x[,"n"]
E = to_comp2$V1[,"mean"]*to_comp2$V1[,"n"]
n = to_comp$x[,"n"]

chi = sum((O - E)^2/E)
pval = pchisq(chi, df=(nrow(to_comp)-2), lower.tail = F)
cat("C =", chi,"\n")
cat("p-value =", pval)

png(filename = "calibration_NN_train.png", width = 7, height = 6, units = 'in', res=600)
plot(to_comp2$V1[,"mean"], to_comp$x[,"mean"], pch=19, col = "red", xlim=c(0,0.05),ylim=c(0,0.05),
     main = "Group Calibration (GBM)", xlab = "Predicted Frequency", ylab="True Frequency")
abline(a=0,b=1,lty=3)
grid()

legend("topleft", legend = c(TeX(sprintf("$\\hat{C}$ = %.3f", round(chi, 3))),
                             paste("p-value =", round(pval,3))),lty=c(0,0))
dev.off()


## Calibration for test

## data to aggregate

to_comp = aggregate(x = train1$ClaimFreq, FUN = function(x) c(mean=mean(x), n=length(x)),
                    by = list(Gender = train1$Gender,
                              Area = train1$Area,
                              Fract = train1$Power))

to_comp$x[,"mean"]

test1$prediction <- testvalid

to_comp2 = aggregate(x = test1$prediction, FUN = function(x) c(mean=mean(x), n=length(x)),
                     by = list(Gender = test1$Gender,
                               Area = test1$Area,
                               Fract = test1$Power))

to_comp2$V1[,"mean"]
O = to_comp$x[,"mean"]*to_comp$x[,"n"]
E = to_comp2$V1[,"mean"]*to_comp2$V1[,"n"]
n = to_comp$x[,"n"]

chi = sum((O - E)^2/E)
pval = pchisq(chi, df=(nrow(to_comp)-2), lower.tail = F)
cat("C =", chi,"\n")
cat("p-value =", pval)

png(filename = "calibration_NN_test.png", width = 7, height = 6, units = 'in', res=600)
plot(to_comp2$V1[,"mean"], to_comp$x[,"mean"], pch=19, col = "red", xlim=c(0,0.05),ylim=c(0,0.05),
     main = "Group Calibration (GBM)", xlab = "Predicted Frequency", ylab="True Frequency")
abline(a=0,b=1,lty=3)
grid()

legend("topleft", legend = c(TeX(sprintf("$\\hat{C}$ = %.3f", round(chi, 3))),
                             paste("p-value =", round(pval,3))),lty=c(0,0))
dev.off()





NN <- as.data.frame(c("NN1", "NN11","NN2", "NN3", "NN4", "NN22", "NN5", "NN33", "NN44", "NN55", "NN20", "NN100"))
tr <- as.data.frame(cbind(rbind(22.161222458,22.025396347,6.711236954,6.909665585,6.821731091,6.699051857,6.699090481,6.716433048,22.492700577,6.696665287, 6.678380489, 6.695694447), rbind(1.339661121,1.341346264,1.609921217,1.609171391,1.609515786,1.610065103,1.610445619,1.609483242,1.335576057,1.610599399, 1.609516859, 1.600015998)))
te <- as.data.frame(cbind(rbind(22.119956970,21.984992981,6.936674118,7.105242252,7.020767689,6.941919804,6.939745903,6.946818352,2.449251175,6.955858231, 6.948728085, 7.031733036), rbind(1.326634765,1.328316212,1.596442699,1.595726252,1.596091390,1.596601963,1.596987605,1.596055508,1.322554946,1.597150326, 1.596090555, 1.586636305)))
verif <- as.data.frame(c(tr,te,NN), col.names = c("loss train", "mse train", "loss valid", "mse valid", "NN"))

ggplot(verif) + aes(x = NN, y = loss.train) + geom_point(color = "red", size = 2) + geom_point(aes(y = loss.valid), color = "blue", size = 2) + geom_point(aes(y = mse.train), color = "green", size = 3, alpha = 0.4) + geom_point(aes(y = mse.valid), color = "orange", size = 1, alpha = 1)


mcp_save <- ModelCheckpoint('path-to-model/model.hdf5', save_best_only=True, monitor='val_acc', mode='max')
## Creating groups

dbtest <- read.table("DBtest.csv", sep=",", header=TRUE)
dbtest$X[which(dbtest$Gender == 1 & dbtest$Area == 1)] = 1
dbtest$X[which(dbtest$Gender == 1 & dbtest$Area == 2)] = 2
dbtest$X[which(dbtest$Gender == 1 & dbtest$Area == 3)] = 3
dbtest$X[which(dbtest$Gender == 1 & dbtest$Area == 4)] = 4
dbtest$X[which(dbtest$Gender == 1 & dbtest$Area == 5)] = 5
dbtest$X[which(dbtest$Gender == 2 & dbtest$Area == 1)] = 6
dbtest$X[which(dbtest$Gender == 2 & dbtest$Area == 2)] = 7
dbtest$X[which(dbtest$Gender == 2 & dbtest$Area == 3)] = 8
dbtest$X[which(dbtest$Gender == 2 & dbtest$Area == 4)] = 9
dbtest$X[which(dbtest$Gender == 2 & dbtest$Area == 5)] = 10
Profile <- dbtest$X
ageInd  <- dbtest$DriverAge[which(dbtest$Gender == 1 & dbtest$Area == 1)]
dbtest  <- dbtest[,-1]

names(dbtest)
dbtest$CarAge <-  as.numeric(dbtest$CarAge)
dbtest$DriverAge  <-  as.numeric(dbtest$DriverAge)
dbtest$Exposure  <-  as.numeric(dbtest$Exposure)
dbtest$Gender   <-  as.factor(dbtest$Gender)
dbtest$Area   <-  as.factor(dbtest$Area)
dbtest$Leasing  <-  as.factor(dbtest$Leasing)
dbtest$Power  <-  as.factor(dbtest$Power)
dbtest$Fract  <-  as.factor(dbtest$Fract)
dbtest$Contract<-  as.factor(dbtest$Contract)
dbtest$Fuel  <-  as.factor(dbtest$Fuel)

dbtest$scaledDriverAge <- (dbtest$DriverAge-min(dbtest$DriverAge))/
  (max(dbtest$DriverAge)-min(dbtest$DriverAge))
dbtest$scaledCarAge <- (dbtest$CarAge-min(dbtest$CarAge))/
  (max(dbtest$CarAge)-min(dbtest$CarAge))
dbtest <- dbtest[, -c(2,3)]

dbtest      <- Convert.Factor(dbtest,"Gender")
dbtest      <- Convert.Factor(dbtest,"Area")
dbtest      <- Convert.Factor(dbtest,"Leasing")
dbtest      <- Convert.Factor(dbtest,"Power")
dbtest      <- Convert.Factor(dbtest,"Fract")
dbtest      <- Convert.Factor(dbtest,"Contract")
dbtest      <- Convert.Factor(dbtest,"Fuel")
Exposure <- dbtest$Exposure
dbtest <- dbtest[, -1]
dbtest <- data.frame(dbtest)

tmp   <-colnames(x_train) %in% colnames(dbtest)
x_val_name  <-vector("list", 2)
x_val_name [[2]]<-colnames(x_train)
x_valid<-matrix(0,nrow(dbtest),ncol(x_train),dimnames = x_val_name)
x_valid[,tmp]<-data.matrix(dbtest)


covariateIndMA1  <- x_valid[Profile==1,]
covariateIndMA1
covariateIndMA2 <- x_valid[Profile==2,]
covariateIndMA3 <- x_valid[Profile==3,]
covariateIndMA4  <- x_valid[Profile==4,]
covariateIndMA5  <- x_valid[Profile==5,]
covariateIndWA1  <- x_valid[Profile==6,]
covariateIndWA2  <- x_valid[Profile==7,]
covariateIndWA3  <- x_valid[Profile==8,]
covariateIndWA4  <- x_valid[Profile==9,]
covariateIndWA5  <- x_valid[Profile==10,]

testIndMA1 <- model1 %>% predict(covariateIndMA1)
testIndMA2 <- model1 %>% predict(covariateIndMA2)
testIndMA3 <- model1 %>% predict(covariateIndMA3)
testIndMA4 <- model1 %>% predict(covariateIndMA4)
testIndMA5 <- model1 %>% predict(covariateIndMA5)

testIndWA1 <- model1 %>% predict(covariateIndWA1)
testIndWA2 <- model1 %>% predict(covariateIndWA2)
testIndWA3 <- model1 %>% predict(covariateIndWA3)
testIndWA4 <- model1 %>% predict(covariateIndWA4)
testIndWA5 <- model1 %>% predict(covariateIndWA5)

x_valid

train$scaledDriverAge <- (train$DriverAge-min(train$DriverAge))/
  (max(train$DriverAge)-min(train$DriverAge))
train$scaledCarAge <- (train$CarAge-min(train$CarAge))/
  (max(train$CarAge)-min(train$CarAge))
train <- train[, -c(1,2)]

MNCcomp<-glm(Nbclaims~scaledDriverAge+scaledCarAge+Gender1+
               Area1+Area2+Area3+Area4 +Leasing1 +
               Fract1+Fract2+Fuel1+Fuel2+Fuel3+Contract1+Contract2,data=train,
             offset=log(Exposure), family=poisson(link="log"))

dbvalid<-data.frame(x_valid,Exposure=rep(1,nrow(x_valid)))
testIndMA1GLM <- exp(predict(MNCcomp,dbvalid[Profile==1,]))
testIndMA2GLM <- exp(predict(MNCcomp,dbvalid[Profile==2,]))
testIndMA3GLM <- exp(predict(MNCcomp,dbvalid[Profile==3,]))
testIndMA4GLM <- exp(predict(MNCcomp,dbvalid[Profile==4,]))
testIndMA5GLM <- exp(predict(MNCcomp,dbvalid[Profile==5,]))
testIndWA1GLM <- exp(predict(MNCcomp,dbvalid[Profile==6,]))
testIndWA2GLM  <- exp(predict(MNCcomp,dbvalid[Profile==7,]))
testIndWA3GLM  <- exp(predict(MNCcomp,dbvalid[Profile==8,]))
testIndWA4GLM  <- exp(predict(MNCcomp,dbvalid[Profile==9,]))
testIndWA5GLM  <- exp(predict(MNCcomp,dbvalid[Profile==10,]))


graphics.off()
x11()
## covariateIndKCntryC3 Women, countryside
par(mfrow=c(2,2))
plot(ageInd , testIndMA3,type="l",col="blue",xlab="Age",ylab="lambda",
     xlim=c(20,60),ylim=c(0,0.1),main="Women, Countryside, Class 3",lty=1,lwd=2.5 )
lines(ageInd, testIndMA2GLM, col="black", type="l", lty=3,lwd=2.5)
grid(nx=5)
legend(30, 0.02, legend=c("NN","GLM"),
       col=c("blue","red"), lty=1:3, cex=1,lwd=2.5)

plot(ageInd , testIndMUrbanC3,type="l",col="blue",xlab="Age",ylab="lambda",
     xlim=c(20,60),ylim=c(0,0.15),main="Men, Urban, Class 3",lty=1,lwd=2.5 )
lines(ageInd, testIndMUrbanC3GLM, col="black", type="l", lty=3,lwd=2.5)
grid(nx=5)
legend(45, 0.10, legend=c("NN","GLM"),
       col=c( "blue","red","black"), lty=1:3, cex=1,lwd=2.5)

plot(ageInd , testIndMNorthC3,type="l",col="blue",xlab="Age",ylab="lambda",
     xlim=c(20,60),ylim=c(0,0.03),main="Men, North countryside, Class 3",lty=1,lwd=2.5 )
lines(ageInd, testIndMNorthC3GLM, col="black", type="l", lty=3,lwd=2.5)
grid(nx=5)
legend(45, 0.02, legend=c("NN","GLM"),
       col=c( "blue","red","black"), lty=1:3, cex=1,lwd=2.5)

plot(ageInd , testIndMCntryC7,type="l",col="blue",xlab="Age",ylab="lambda",
     xlim=c(20,60),ylim=c(0,0.05),main="Men, Countryside, Class 7",lty=1,lwd=2.5 )
lines(ageInd, testIndMCntryC7GLM, col="black", type="l", lty=3,lwd=2.5)
grid(nx=5)
legend(45, 0.03, legend=c("NN","GLM"),
       col=c( "blue","red","black"), lty=1:3, cex=1,lwd=2.5)





