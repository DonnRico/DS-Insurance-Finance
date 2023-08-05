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
library(glmmTMB)
library(brms)
theme_set(theme_bw())



####################################################################
#                         SCRIPT Description
####################################################################




# Data train Import

DBtrain <- read_delim("DBtrain.csv", delim = ";", 
                      escape_double = FALSE, trim_ws = TRUE)

# Data train cleaning

DBtrain <- DBtrain[,-1]
corrplot(cor(DBtrain), method = "color", addCoef.col = "black",tl.col="black", tl.srt=45, insig = "blank")
corr.test(DBtrain, y = NULL, use = "pairwise",method="pearson",adjust="holm", 
          alpha=.05,ci=TRUE,minlength=5,normal=TRUE)
DBtrain$Gender <- as.factor(DBtrain$Gender)
DBtrain$Area <- as.factor(DBtrain$Area)
DBtrain$Leasing <- as.factor(DBtrain$Leasing)
DBtrain$Power <- as.factor(DBtrain$Power)
DBtrain$Fract <- as.factor(DBtrain$Fract)
DBtrain$Contract <- as.factor(DBtrain$Contract)
DBtrain$Fuel <- as.factor(DBtrain$Fuel)
DBtrain$DriverAge <- as.factor(DBtrain$DriverAge)


df <- aggregate(cbind(Exposure, Nbclaims)~DriverAge, data = DBtrain, FUN=c("sum", "mean"))
df <- merge(aggregate(Exposure ~ DriverAge, DBtrain, "sum"), 
      aggregate(Nbclaims ~ DriverAge, DBtrain, "mean"))

ggplot(df,aes(x=DriverAge,y=Exposure,fill=Nbclaims))+geom_bar(stat="identity") + labs(title = "Exposure sum and Number of claims mean by Driver age")

# Descriptive and exploratory analysis

stat <- psych::describe(DBtrain)
stat2 <- Hmisc::describe(DBtrain)
stat
stat2
kbl(round(stat,2), caption = "Descriptive analysis: continuous and categorial (*) variables", booktabs = TRUE) |> 
  kable_classic_2(full_width = F, html_font = "Cambria") |>
  row_spec(0,background = "#332288", color = "white") |>
  column_spec(1,background = "#332288", color = "white")

theme_set(theme_light())
for (i in c(1,4,5,6,7,8,9,11)){
  var <- as.data.frame(DBtrain[, i])
  colnames(var) <- "temp"
  p <- ggplot(var) + aes(y = temp) + geom_bar(fill = "#332288",color = "white") + 
    labs(y = names(DBtrain)[i], title = names(DBtrain)[i])
  print(p)
}

for (i in c(1,4,5,6,7,8,9,11)){
  var <- as.data.frame(DBtrain[, i])
  colnames(var) <- "temp"
  tb <- xtabs(~temp, data = var) |> proportions()
  tbout <- kbl(round(tb,2), booktabs = TRUE, col.names = c(names(DBtrain)[i], "Freq")) |> 
    kable_classic_2(full_width = F, html_font = "Cambria") |>
    row_spec(0,background = "#332288", color = "white") |>
    column_spec(1,background = "#332288", color = "white")
  print(tbout)
}

for (i in c(1,4,5,6,7,8,9,11)){
  var <- as.data.frame(DBtrain[, i])
  colnames(var) <- "temp"
  p <- ggplot(var) + aes(y = temp) + geom_bar(fill = "#332288",color = "#332288", alpha = 0.6) + 
    labs(y = names(DBtrain)[i], title = names(DBtrain)[i])
  print(p)
}


for (i in c(2,3,10)){
  var <- as.data.frame(DBtrain[, i])
  colnames(var) <- "temp"
  p <- ggplot(var) + aes(x = temp) + geom_density(fill = "#332288",color = "#332288", alpha = 0.4) + 
    labs(x = names(DBtrain)[i], title = names(DBtrain)[i], y = "density")
  print(p)
}

ggplot(DBtrain, aes(DriverAge, after_stat(count), fill = Area, colour = Area)) + geom_density(alpha = 0.2)
ggplot(DBtrain, aes(CarAge, after_stat(count), fill = Area, colour = Area)) + geom_density(alpha = 0.2)
ggplot(DBtrain, aes(Exposure, after_stat(count), fill = Area, colour = Area)) + geom_density(alpha = 0.2)

ggplot(DBtrain, aes(DriverAge, after_stat(count), fill = Gender, colour = Gender)) + geom_density(alpha = 0.2)
ggplot(DBtrain, aes(CarAge, after_stat(count), fill = Gender, colour = Gender)) + geom_density(alpha = 0.2)
ggplot(DBtrain, aes(Exposure, after_stat(count), fill = Gender, colour = Gender)) + geom_density(alpha = 0.2)

ggplot(DBtrain, aes(DriverAge, after_stat(count), fill = Power, colour = Power)) + geom_density(alpha = 0.2)
ggplot(DBtrain, aes(CarAge, after_stat(count), fill = Power, colour = Power)) + geom_density(alpha = 0.2)
ggplot(DBtrain, aes(Exposure, after_stat(count), fill = Power, colour = Power)) + geom_density(alpha = 0.2)

ggplot(DBtrain, aes(DriverAge, fill = Nbclaims, colour = Nbclaims)) + geom_density(alpha = 0.2)
ggplot(DBtrain, aes(CarAge, fill = Nbclaims, colour = Nbclaims)) + geom_density(alpha = 0.2)
ggplot(DBtrain, aes(Exposure, fill = Nbclaims, colour = Nbclaims)) + geom_density(alpha = 0.2)

ggplot(DBtrain, aes(DriverAge, fill = Fuel, colour = Fuel)) + geom_density(alpha = 0.2)
ggplot(DBtrain, aes(CarAge, fill = Fuel, colour = Fuel)) + geom_density(alpha = 0.2)
ggplot(DBtrain, aes(Exposure, fill = Fuel, colour = Fuel)) + geom_density(alpha = 0.2)

tb1 <- xtabs(~Nbclaims + Power, data = DBtrain)
kbl(round(tb1,2), booktabs = TRUE, caption = "Nbclaims x Power") |> 
  kable_classic_2(full_width = F, html_font = "Cambria") |>
  row_spec(0,background = "#332288", color = "white") |>
  column_spec(1,background = "#332288", color = "white")


ggplot(DBtrain) + aes(x= Nbclaims, color = Area) + geom_density() 

## Begin of the work on Nbclaims -> claims frequency

ggplot(DBtrain) + aes(x=Nbclaims) + geom_density()
DBtrain$ClaimFreq <- DBtrain$Nbclaims / DBtrain$Exposure

mean(DBtrain$ClaimFreq)

## Stable distribution into the levels for freq of claims

for (i in c(1,4,5,6,7,8,9)){
  var <- as.data.frame(DBtrain[, c(i,12)])
  colnames(var) <- c("temp","ClaimFreq")
  p <- ggplot(var) + aes(x=ClaimFreq, color = temp) + geom_density() + 
    scale_colour_discrete(name=names(DBtrain)[i])
  print(p)
}

for (i in c(1,4,5,6,7,8,9)){
  var <- as.data.frame(DBtrain[, c(i,11)])
  colnames(var) <- c("temp","NbClaims")
  p <- ggplot(var) + aes(x=NbClaims, color = temp) + geom_density() + 
    scale_colour_discrete(name=names(DBtrain)[i])
  print(p)
}

ggplot(DBtrain) + aes(x=ClaimFreq) + geom_density(color = "#6d071a", fill = "#6d071a", alpha = 0.5)

## Pic at 0 so it's convenient to call tweedie models ?

## Analyse pointilleuse de la distribution de nos réponses 

stat3 <- as.matrix(round(psych::describe(DBtrain),2))[c(11,12),]

kbl(t(stat3[,-c(1,2)]), booktabs = TRUE) |> 
  kable_classic_2(full_width = F, html_font = "Cambria") |>
  row_spec(0,background = "#332288", color = "white") |>
  column_spec(1,background = "#332288", color = "white")

ggplot(DBtrain) + aes(x = Nbclaims) + geom_histogram(color = "#332288", fill = "#332288", alpha = 0.4)
ggplot(DBtrain) + aes(x = log(ClaimFreq)) + geom_histogram(color = "#332288", fill = "#332288", alpha = 0.4)

stat4 <- rowPerc(xtabs(~Nbclaims, data = DBtrain))
stat5 <- xtabs(~Nbclaims, data = DBtrain)
stat4
stat5 <- c(round(stat5),70000)
kbl(t(rbind(stat5, stat4)), caption = "Nbclaims", booktabs = TRUE, col.names = c("count", "%")) |> 
  kable_classic_2(full_width = F, html_font = "Cambria") |>
  row_spec(0,background = "#332288", color = "white") |>
  column_spec(1,background = "#332288", color = "white")


DBwithout1 <- DBtrain[which(DBtrain$Nbclaims != 0),]
stat6 <- as.matrix(round(psych::describe(DBwithout1),2))[c(11,12),]
kbl(t(stat6), caption = "Descriptive analysis: NbClaims and ClaimFreq without 1 claim", booktabs = TRUE) |> 
  kable_classic_2(full_width = F, html_font = "Cambria") |>
  row_spec(0,background = "#6d071a", color = "white") |>
  column_spec(1,background = "#6d071a", color = "white")


ggplot(DBwithout1) + aes(x = Nbclaims) + geom_histogram(color = "#6d071a", fill = "#6d071a", alpha = 0.4)

ggplot(DBwithout1) + aes(x = log(ClaimFreq)) + geom_histogram(color = "#6d071a", fill = "#6d071a", alpha = 0.4) + labs(title = "Histogram of log(ClaimFreq) without NbClaims = 1")


## negative Binomial model instead of Poisson ? Very high occurence for 1

descdist(DBtrain$Nbclaims, discrete = TRUE)
descdist(log(DBtrain$ClaimFreq), discrete = FALSE)
fitNB.negativeb <- fitdist(DBtrain$Nbclaims, "nbinom")
fitNB.poisson <- fitdist(DBtrain$Nbclaims, "pois")
fitNB.lnorm <- fitdist(DBtrain$Nbclaims, "lnorm")
plot(fitNB.lnorm)
## Bonne distribution in gauss ??
fitNB.invgauss <- fitdist(DBtrain$ClaimFreq, "invgauss", start = list(mean = 4, shape = 1))
plot(fitNB.invgauss)
plot(fitNB.negativeb)
plot(fitNB.poisson)

## Idem

fitCF.norm <- fitdist(DBtrain$ClaimFreq, "lnorm")
plot(fitCF.norm)

## Interessant fit très bien 

mean(DBtrain$ClaimFreq)

var(DBtrain$ClaimFreq)


sum(DBtrain[which(DBtrain$Nbclaims ==0), "Exposure"]) + 
  sum(DBtrain[which(DBtrain$Nbclaims ==1), "Exposure"]) + 
  sum(DBtrain[which(DBtrain$Nbclaims ==2), "Exposure"]) + 
  sum(DBtrain[which(DBtrain$Nbclaims ==3), "Exposure"])



