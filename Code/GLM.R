
####################################################################
#                         SCRIPT GLM,ZIP,GAM
####################################################################

set.seed(87031800)

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

quantile(DBtrain$DriverAge, prob=c(.25,.5,.75), type=1)
quantile(DBtrain$CarAge, prob=c(.25,.5,.75), type=1)

DBtrain$DriverAge <- CutQ(DBtrain$DriverAge, breaks = quantile(DBtrain$DriverAge, seq(0, 1, by = 0.25), na.rm = TRUE))
DBtrain$CarAge <- CutQ(DBtrain$CarAge, breaks = quantile(DBtrain$CarAge, seq(0, 1, by = 0.25), na.rm = TRUE))


ggplot(DBtrain, aes(x=Gender, y = Nbclaims)) + geom_boxplot() +
  stat_summary(fun="mean", geom="point", shape=20, size=1, color="red", fill="red") + scale_y_continuous(limits = c(0,0.4))

Categ.to.Quant<-function(data,factor,removeLast=TRUE)
  # convert a categorical variable in a set of binary variables,
  # data: dataframe, f
  # factor : string,name of the categorical variable
{
  y = paste("data$",sep = "",factor)
  x = eval(parse(text=y))
  ndata = length(x)          #number of lines in the dataset
  nlgen = length(levels(x))  #number of levels
  if (!removeLast)
  {nlgen = nlgen+1}  #number of levels
  lev   = levels(x)
  z     = matrix(0,ndata,nlgen-1)
  nv    = vector("character",nlgen-1)
  for (ct in 1:nlgen-1)
  {
    z[,ct] = ifelse(x==lev[ct],1,0)
    nv[ct] = paste(factor,sep="",lev[ct])
  }
  colnames(z)=nv
  #remove the column
  data <- data[, ! names(data) %in% factor, drop = F]
  data <- data.frame(data,z)
  return(data)
}
DBtrain <-Categ.to.Quant(DBtrain,"Gender")
DBtrain <-Categ.to.Quant(DBtrain,"Area")
DBtrain <-Categ.to.Quant(DBtrain,"Leasing")
DBtrain <-Categ.to.Quant(DBtrain,"Power")
DBtrain <-Categ.to.Quant(DBtrain,"Fract")
DBtrain <-Categ.to.Quant(DBtrain,"Fuel")
DBtrain <-Categ.to.Quant(DBtrain,"Contract")
DBtrain <-Categ.to.Quant(DBtrain,"DriverAge")
DBtrain <-Categ.to.Quant(DBtrain,"CarAge")

smp_size <- floor(0.8 * nrow(DBtrain))
train.idx <- sample(seq_len(nrow(DBtrain)), size = smp_size)
train <- DBtrain[train.idx, ]
test <-  DBtrain[-train.idx, ]
Y <- test[,11]


## GLM with dummy 

MNC<-glm(Nbclaims ~ . - Exposure - ClaimFreq + offset(log(Exposure)), data=train, family=poisson(link = "log"))

summary(MNC)
deviance(MNC)
BIC(MNC)
logLik(MNC)
BIC(MNC)
autoplot(MNC, colour = 'Nbclaims',
         smooth.colour = 'red',predict.size = 3,
         ad.colour = 'blue',
         label.size = 3, label.colour = 'blue')
plot(residuals.glm(MNC))
step(MNC)

MNC2 <- glm(formula = Nbclaims ~ Gender1 + Area1 + Area2 + Area4 + Leasing1 + 
              Power1 + Power2 + Power3 + Fract1 + Fract2 + Fuel1 + Contract1 + 
              Contract2 + DriverAgeQ1 + offset(log(Exposure)), family = poisson(link = "log"), 
            data = train)
summary(MNC2)
lambdapred <- predict.glm(MNC, test, "response")
NBpred <- lambdapred * test$Exposure
sum(lambdapred)/length(lambdapred)
plot(test$Nbclaims, NBpred)

test2 = data.frame()
for (age in seq(20,80,2)) {
  tmp<-data.frame(DriverAge=age,Gender=factor(2),Area=factor(3),
                  Leasing=factor(2),CarAge=3, Contract=factor(3), Exposure=1, Fuel=factor(2), Power=factor(3), Fract= factor(1))
  test2<-rbind(test2,tmp)
}
test2$DriverAge <- CutQ(test2$DriverAge, breaks = quantile(test2$DriverAge, seq(0, 1, by = 0.25), na.rm = TRUE))
test2$CarAge <- CutQ(test2$CarAge, breaks = c(0,3,5,7,16))
lambdaW<-exp(predict(MNC,test2))

test2 = data.frame()
for (age in seq(20,80,2)) {
  tmp<-data.frame(DriverAge=age,Gender=factor(1),Area=factor(3),
                  Leasing=factor(2),CarAge=3, Contract=factor(3), Exposure=1, Fuel=factor(2), Power=factor(3), Fract= factor(1))
  test2<-rbind(test2,tmp)
}
test2$DriverAge <- CutQ(test2$DriverAge, breaks = quantile(test2$DriverAge, seq(0, 1, by = 0.25), na.rm = TRUE))
test2$CarAge <- CutQ(test2$CarAge, breaks = c(0,3,5,7,16))
lambdaM<-exp(predict(MNC,test2))


plot(seq(20,80,2),lambdaW,xlab="DriverAge",ylab="lambda")
plot(seq(20,80,2),lambdaM,add=TRUE)
matplot(seq(20,80,2), cbind(lambdaW,lambdaM),type="l",col=c("red","green"),
        lty=c(1,1),xlab="DriverAge",ylab="lambda" )
legend(30,0.021,c("Women", "Men"),lty=c(1,1), col=c("red","green"))

## Quasipoisson fit 

MNCQ<-glm(Nbclaims ~ .- Exposure - ClaimFreq, data=train, family=quasipoisson(link = "log"), offset=log(Exposure))

sum <- summary(MNCQ)
sum

lambdapred <- exp(predict(MNCQ, test))
NBpred <- lambdapred * test$Exposure
plot(test$Nbclaims, NBpred)

## Fit bien avec poisson

MNCTw<-glm(Nbclaims ~ .- Exposure - ClaimFreq, data=train,
           offset=log(Exposure), family= tweedie(var.power = 1, link.power = 0))
summary(MNCTw)
autoplot(MNCTw, alpha = 0.5)
plot(residuals.glm(MNCTw))


## Poisson regression check
rootogram(fitverif)
Ord_plot(DBtrain$Nbclaims)
anova(MNC, test="Chisq")

## Overdispersion check

library(devtools)
devtools::install_github(repo = "DHARMa", username = "florianhartig", subdir = "DHARMa")
deviance(MNC)/MNC$df.residual
dispersiontest(MNC)
sim_fmp <- simulateResiduals(MNC)
testOverdispersion(sim_fmp)
plot(sim_fmp)

Anova(mzpoiss,type="II",test="Chisq")

ggplot(DBtrain, aes(Nbclaims)) + geom_histogram(binwidth = 0.5) + facet_wrap(~Area)

## Negative binomial


MNB<-glm.nb(Nbclaims ~ . - Exposure - ClaimFreq + offset(log(Exposure)), data=train)

summary(MNB)
BIC(MNB)
AIC(MNB)
deviance(MNB)

lambdapred <- exp(predict.glm(MNB, test))
NBpred <- lambdapred * test$Exposure
plot(test$Nbclaims, NBpred)


## cv
cost <- function(r, pi = 0) mean(abs(r-pi))
cv.glm(DBtrain, MNC,cost, K = 10)

kbl(round(summary(MNC)$coef,3), caption = "Poison GLM after Nbclaim recoding (0,1,2,3)", booktabs = TRUE) |>  kable_classic_2(full_width = F, html_font = "Cambria") |> row_spec(0,background = "#6d071a", color = "white") |> column_spec(1,background = "#6d071a", color = "white")

kbl(round(summary(MNCTw)$coef,3), caption = "Tweedie Poison GLM after Nbclaim recoding (0,1,2,3)", booktabs = TRUE) |>  kable_classic_2(full_width = F, html_font = "Cambria") |> row_spec(0,background = "#6d071a", color = "white") |> column_spec(1,background = "#6d071a", color = "white")

kbl(round(summary(mzpoiss)$count,3), caption = "Tweedie Poison GLM after Nbclaim recoding (0,1,2,3)", booktabs = TRUE) |>  kable_classic_2(full_width = F, html_font = "Cambria") |> row_spec(0,background = "#6d071a", color = "white") |> column_spec(1,background = "#6d071a", color = "white")

## Comparision of models

modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL)

fitverif <- goodfit(DBtrain$Nbclaims)
observed <- fitverif$observed
observed
fitted <-round(fitverif$fitted)
kbl(t(rbind(c(0,1,2,3),observed, fitted)), caption = "Nbclaims, $\\lambda$ = 0.033", booktabs = TRUE, col.names = c(" ", "observed", "fitted")) |> 
  kable_classic_2(full_width = F, html_font = "Cambria") |>
  row_spec(0,background = "#332288", color = "white") |>
  column_spec(1,background = "#332288", color = "white")


## reload for non-EDM models

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

smp_size <- floor(0.8 * nrow(DBtrain))
train.idx <- sample(seq_len(nrow(DBtrain)), size = smp_size)
train <- DBtrain[train.idx, ]
test <-  DBtrain[-train.idx, ]
Y <- test[,11]


## Try of the General Linear Mixed Modelwith poisson but attention because no correlations

MNCMix<-glmer(Nbclaims ~ DriverAge + CarAge + Power + Fract + Contract + Fuel + Leasing + Gender + offset(log(Exposure)) + (1|Area), 
              data=train,
              family=poisson(link="log"))

sum <- summary(MNCMix)
sum
sum$logLik

lambdapred <- exp(predict(MNCMix, test))
NBpred <- lambdapred * test$Exposure
plot(test$Nbclaims, NBpred)

NB_Area <- fixef(MNCMix) + ranef(MNCMix)$Area
NB_Area$Area<-rownames(NB_Area)
names(NB_Area)[1]<-"Intercept"
NB_Area <- NB_Area[,c(2,1)]
table(DBtrain$Area)

ggplot(NB_Area,aes(x=Area,y=exp(Intercept)))+geom_point(colour = NB_Area$Area) + labs(y = "Estimated number of claims")

plot(ranef(MNCMix))
lattice::dotplot(ranef(MNCMix, postVar = TRUE))
isSingular(MNCMix)

## Mix with AR1
install.packages("remotes")
remotes::install_github("stevencarlislewalker/lme4ord")
unzip(f, exdir=tempdir())
load(file.path(tempdir(), '.RData'))
library(lme)

MNCMixAR<-glmmPQL(Nbclaims ~ DriverAge + CarAge + Leasing + Gender+ Power + Fract + Contract + Fuel + offset(log(Exposure)), random = ~ 1|Area, 
              data=train,
              family=poisson(link="log"),correlation=corAR1(),verbose=FALSE)


## ZERO-INFLATED POISSON REGRESSION

library(vcdExtra)
tab <- xtabs(~Nbclaims, data = DBtrain)
tab
zero.test(tab)
mzpoiss <- zeroinfl(Nbclaims ~ Gender + DriverAge + CarAge +
                      Leasing + Power + Fract + Contract + Fuel + Area + offset(Exposure) | Gender + DriverAge + CarAge +
                      Leasing + Power + Fract + Contract + Fuel + Area + offset(Exposure), data = train, dist = "poisson")
sum <- summary(mzpoiss)
sum 
AIC(mzpoiss)
BIC(mzpoiss)

mzpoiss2 <- zeroinfl(Nbclaims ~ Gender +
                      Leasing + Power + Fract + Fuel + offset(Exposure) | Area + offset(Exposure), data = train, dist = "poisson")
sum <- summary(mzpoiss2)
sum 

E2 <- resid(mzpoiss, type = "pearson")
N  <- nrow(DBtrain)
p  <- length(coef(mzpoiss))  
sum(E2^2) / (N - p)

lambdapred <- predict(mzpoiss2, test)
NBpred <- lambdapred * test$Exposure
plot(test$Nbclaims, NBpred)

## GAM 

d.gam <-gam(Nbclaims~ Gender + Area + Leasing + Power+Fract+Contract+Fuel + s(CarAge ,bs="cr",k=7)+ s(DriverAge ,bs="cr",k=7)
             + offset(log(Exposure)),
             data=train, family=poisson(link="log"))
d.gam2 <-gam(Nbclaims~ Gender + s(DriverAge ,bs="cr",k=7)
            + offset(log(Exposure)),
            data=train, family=poisson(link="log"))
summary(d.gam2)
plot(d.gam2, shade = TRUE, shade.col = "lightblue")

lambda <- exp(predict.gam(d.gam,test))
mean(lambda)

test = data.frame()
for (age in seq(18,90,1)) {
  tmp<-data.frame(DriverAge=age,Gender=factor(1),
                   Exposure=1)
  test<-rbind(test,tmp)
}


lambdaK <- exp(predict.gam(d.gam,test))



test = data.frame()
for (age in seq(18,90,1)) {
  tmp<-data.frame(DriverAge=age,Gender=factor(2),
                  Exposure=1)
  test<-rbind(test,tmp)
}


lambdaM <- exp(predict(d.gam2,test))

matplot(seq(18,90,1), cbind(lambdaK,lambdaM),type="l",col=c("red","blue"),
        lty=c(1,1),xlab="DriverAge",ylab="lambda",main="Expected Lambda")
legend(30,0.017,c("Men", "Women"),lty=c(1,1), col=c("red","blue"))

summary(d.gam)
plot(d.gam, shade = TRUE, shade.col = "lightblue")
deviance(d.gam)
gam.check(d.gam)
AIC(d.gam)

BIC(d.gam)

lambdapred <- exp(predict(d.gam, test))
muhat <- lambdapred * test$Exposure
plot(test$Nbclaims, muhat)

## Grouped data

xtabs(~ Gender + Area, data = DBtrain)
