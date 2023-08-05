
####################################################################
#                         SCRIPT Plot calibration
####################################################################



## Grouping results calibration


datatoplot <- cbind(test,Y, NBpred)
F1 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area1 == 1 & datatoplot$Leasing1 ==1),c("Exposure", "Y", "NBpred")]
F11 <- c(sum(F1$Exposure),sum(F1$Y),sum(F1$NBpred))
F2 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area2 == 1 & datatoplot$Leasing1 == 1),c("Exposure", "Y", "NBpred")]
F21 <- c(sum(F2$Exposure),sum(F2$Y),sum(F2$NBpred))
F3 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area3 == 1 & datatoplot$Leasing1 == 1),c("Exposure", "Y", "NBpred")]
F31 <- c(sum(F3$Exposure),sum(F3$Y),sum(F3$NBpred))
F4 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area4 == 1 & datatoplot$Leasing1 == 1 ),c("Exposure", "Y", "NBpred")]
F41 <- c(sum(F4$Exposure),sum(F4$Y),sum(F4$NBpred))
F5 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area4 == 0 & datatoplot$Area3 == 0 & datatoplot$Area2 == 0 & datatoplot$Area1 == 0 & datatoplot$Leasing1 == 1),c("Exposure", "Y", "NBpred")]
F51 <- c(sum(F5$Exposure),sum(F5$Y),sum(F5$NBpred))
F1 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area1 == 1 & datatoplot$Leasing1 ==0),c("Exposure", "Y", "NBpred")]
F12 <- c(sum(F1$Exposure),sum(F1$Y),sum(F1$NBpred))
F2 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area2 == 1 & datatoplot$Leasing1 == 0),c("Exposure", "Y", "NBpred")]
F22 <- c(sum(F2$Exposure),sum(F2$Y),sum(F2$NBpred))
F3 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area3 == 1 & datatoplot$Leasing1 == 0),c("Exposure", "Y", "NBpred")]
F32 <- c(sum(F3$Exposure),sum(F3$Y),sum(F3$NBpred))
F4 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area4 == 1 & datatoplot$Leasing1 == 0),c("Exposure", "Y", "NBpred")]
F42 <- c(sum(F4$Exposure),sum(F4$Y),sum(F4$NBpred))
F5 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area4 == 0 & datatoplot$Area3 == 0 & datatoplot$Area2 == 0 & datatoplot$Area1 == 0 & datatoplot$Leasing1 == 0),c("Exposure", "Y", "NBpred")]
F52 <- c(sum(F5$Exposure),sum(F5$Y),sum(F5$NBpred))
H1 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area1 == 1, datatoplot$Leasing1 == 1),c("Exposure", "Y", "NBpred")]
H11 <- c(sum(H1$Exposure),sum(H1$Y),sum(H1$NBpred))
H2 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area2 == 1, datatoplot$Leasing1 == 1),c("Exposure", "Y", "NBpred")]
H21 <- c(sum(H2$Exposure),sum(H2$Y),sum(H2$NBpred))
H3 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area3 == 1, datatoplot$Leasing1 == 1),c("Exposure", "Y", "NBpred")]
H31 <- c(sum(H3$Exposure),sum(H3$Y),sum(H3$NBpred))
H4 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area4 == 1, datatoplot$Leasing1 == 1),c("Exposure", "Y", "NBpred")]
H41 <- c(sum(H4$Exposure),sum(H4$Y),sum(H4$NBpred))
H5 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area4 == 0 & datatoplot$Area3 == 0 & datatoplot$Area2 == 0 & datatoplot$Area1 == 0, datatoplot$Leasing1 == 1),c("Exposure", "Y", "NBpred")]
H51 <- c(sum(H5$Exposure),sum(H5$Y),sum(H5$NBpred))
H1 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area1 == 1, datatoplot$Leasing1 == 0),c("Exposure", "Y", "NBpred")]
H12 <- c(sum(H1$Exposure),sum(H1$Y),sum(H1$NBpred))
H2 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area2 == 1, datatoplot$Leasing1 == 0),c("Exposure", "Y", "NBpred")]
H22 <- c(sum(H2$Exposure),sum(H2$Y),sum(H2$NBpred))
H3 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area3 == 1, datatoplot$Leasing1 == 0),c("Exposure", "Y", "NBpred")]
H32 <- c(sum(H3$Exposure),sum(H3$Y),sum(H3$NBpred))
H4 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area4 == 1, datatoplot$Leasing1 == 0),c("Exposure", "Y", "NBpred")]
H42 <- c(sum(H4$Exposure),sum(H4$Y),sum(H4$NBpred))
H5 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area4 == 0 & datatoplot$Area3 == 0 & datatoplot$Area2 == 0 & datatoplot$Area1 == 0, datatoplot$Leasing1 == 0),c("Exposure", "Y", "NBpred")]
H52 <- c(sum(H5$Exposure),sum(H5$Y),sum(H5$NBpred))
datatoplot2 <-as.data.frame(t(cbind(F11,F21,F31,F41,F51,F12,F22,F32,F42,F52,H11,H21,H31,H41,H51,H12,H22,H32,H42,H52)))
colnames(datatoplot2) <- c("Exposure", "Y", "NBpred")
datatoplot2$Group <- c("F11","F21","F31","F41","F51","F12","F22","F32","F42","F52","H11","H21","H31","H41","H51","H12","H22","H32","H42","H52")



ggplot(datatoplot2, aes(col = Group)) + geom_point(aes(x=Y, y=NBpred)) + 
  geom_circle(aes(x0=Y, y0=NBpred, r = sqrt(Exposure/pi)*1.5)) + geom_abline(intercept = 0, slope = 1) +
  coord_cartesian(ylim = c(0, 900), xlim = c(0,900)) + labs(title = "Actual vs Predicted NBofclaims Calibration GLM Poisson", y = "Actual", x = "Expected")


## Grouping results for mean/variance

datatoplot <- DBtrain
F1 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area1 == 1 & datatoplot$Leasing1 ==1),c("Exposure", "Nbclaims")]
F11 <- c(sum(F1$Exposure),mean(F1$Nbclaims),var(F1$Nbclaims))
F2 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area2 == 1 & datatoplot$Leasing1 == 1),c("Exposure", "Nbclaims")]
F21 <- c(sum(F2$Exposure),mean(F2$Nbclaims),var(F2$Nbclaims))
F3 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area3 == 1 & datatoplot$Leasing1 == 1),c("Exposure", "Nbclaims")]
F31 <- c(sum(F3$Exposure),mean(F3$Nbclaims),var(F3$Nbclaims))
F4 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area4 == 1 & datatoplot$Leasing1 == 1 ),c("Exposure", "Nbclaims")]
F41 <- c(sum(F4$Exposure),mean(F4$Nbclaims),var(F4$Nbclaims))
F5 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area4 == 0 & datatoplot$Area3 == 0 & datatoplot$Area2 == 0 & datatoplot$Area1 == 0 & datatoplot$Leasing1 == 1),c("Exposure", "Nbclaims")]
F51 <- c(sum(F5$Exposure),mean(F5$Nbclaims),var(F5$Nbclaims))
F1 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area1 == 1 & datatoplot$Leasing1 ==0),c("Exposure", "Nbclaims")]
F12 <- c(sum(F1$Exposure),mean(F1$Nbclaims),var(F1$Nbclaims))
F2 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area2 == 1 & datatoplot$Leasing1 == 0),c("Exposure", "Nbclaims")]
F22 <- c(sum(F2$Exposure),mean(F2$Nbclaims),var(F2$Nbclaims))
F3 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area3 == 1 & datatoplot$Leasing1 == 0),c("Exposure", "Nbclaims")]
F32 <- c(sum(F3$Exposure),mean(F3$Nbclaims),var(F3$Nbclaims))
F4 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area4 == 1 & datatoplot$Leasing1 == 0),c("Exposure", "Nbclaims")]
F42 <- c(sum(F4$Exposure),mean(F4$Nbclaims),var(F4$Nbclaims))
F5 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area4 == 0 & datatoplot$Area3 == 0 & datatoplot$Area2 == 0 & datatoplot$Area1 == 0 & datatoplot$Leasing1 == 0),c("Exposure", "Nbclaims")]
F52 <- c(sum(F5$Exposure),mean(F5$Nbclaims),var(F5$Nbclaims))
H1 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area1 == 1, datatoplot$Leasing1 == 1),c("Exposure", "Nbclaims")]
H11 <- c(sum(H1$Exposure),mean(H1$Nbclaims),var(H1$Nbclaims))
H2 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area2 == 1, datatoplot$Leasing1 == 1),c("Exposure", "Nbclaims")]
H21 <- c(sum(H2$Exposure),mean(H2$Nbclaims),var(H2$Nbclaims))
H3 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area3 == 1, datatoplot$Leasing1 == 1),c("Exposure", "Nbclaims")]
H31 <- c(sum(H3$Exposure),mean(H3$Nbclaims),var(H3$Nbclaims))
H4 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area4 == 1, datatoplot$Leasing1 == 1),c("Exposure", "Nbclaims")]
H41 <- c(sum(H4$Exposure),mean(H4$Nbclaims),var(H4$Nbclaims))
H5 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area4 == 0 & datatoplot$Area3 == 0 & datatoplot$Area2 == 0 & datatoplot$Area1 == 0, datatoplot$Leasing1 == 1),c("Exposure", "Nbclaims")]
H51 <- c(sum(H5$Exposure),mean(H5$Nbclaims),var(H5$Nbclaims))
H1 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area1 == 1, datatoplot$Leasing1 == 0),c("Exposure", "Nbclaims")]
H12 <- c(sum(H1$Exposure),mean(H1$Nbclaims),var(H1$Nbclaims))
H2 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area2 == 1, datatoplot$Leasing1 == 0),c("Exposure", "Nbclaims")]
H22 <- c(sum(H2$Exposure),mean(H2$Nbclaims),var(H2$Nbclaims))
H3 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area3 == 1, datatoplot$Leasing1 == 0),c("Exposure", "Nbclaims")]
H32 <- c(sum(H3$Exposure),mean(H3$Nbclaims),var(H3$Nbclaims))
H4 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area4 == 1, datatoplot$Leasing1 == 0),c("Exposure", "Nbclaims")]
H42 <- c(sum(H4$Exposure),mean(H4$Nbclaims),var(H4$Nbclaims))
H5 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area4 == 0 & datatoplot$Area3 == 0 & datatoplot$Area2 == 0 & datatoplot$Area1 == 0, datatoplot$Leasing1 == 0),c("Exposure", "Nbclaims")]
H52 <- c(sum(H5$Exposure),mean(H5$Nbclaims),var(H5$Nbclaims))
datatoplot2 <-as.data.frame(t(cbind(F11,F21,F31,F41,F51,F12,F22,F32,F42,F52,H11,H21,H31,H41,H51,H12,H22,H32,H42,H52)))
colnames(datatoplot2) <- c("Exposure", "Mean", "Var")
datatoplot2$Group <- c("F11","F21","F31","F41","F51","F12","F22","F32","F42","F52","H11","H21","H31","H41","H51","H12","H22","H32","H42","H52")


ggplot(datatoplot2) + geom_point(aes(x=Mean, y=Var, colour = Group)) + 
  geom_circle(aes(x0=Mean, y0=Var, r = sqrt(Exposure/pi)/50000, colour = Group)) + geom_abline(intercept = 0, slope = 1) + 
  labs(title = "Variance versus mean number of claims in groups (circle = exposure)", y = "Variance", x = "Mean") +
  geom_smooth(aes(x=Mean, y=Var), method = "lm", se = FALSE, colour="red", size=0.5)


datatoplot <- DBtrain
F1 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area1 == 1 & datatoplot$Leasing1 ==1),c("Exposure", "ClaimFreq")]
F11 <- c(sum(F1$Exposure),mean(F1$ClaimFreq),var(F1$ClaimFreq))
F2 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area2 == 1 & datatoplot$Leasing1 == 1),c("Exposure", "ClaimFreq")]
F21 <- c(sum(F2$Exposure),mean(F2$ClaimFreq),var(F2$ClaimFreq))
F3 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area3 == 1 & datatoplot$Leasing1 == 1),c("Exposure", "ClaimFreq")]
F31 <- c(sum(F3$Exposure),mean(F3$ClaimFreq),var(F3$ClaimFreq))
F4 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area4 == 1 & datatoplot$Leasing1 == 1 ),c("Exposure", "ClaimFreq")]
F41 <- c(sum(F4$Exposure),mean(F4$ClaimFreq),var(F4$ClaimFreq))
F5 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area4 == 0 & datatoplot$Area3 == 0 & datatoplot$Area2 == 0 & datatoplot$Area1 == 0 & datatoplot$Leasing1 == 1),c("Exposure", "ClaimFreq")]
F51 <- c(sum(F5$Exposure),mean(F5$ClaimFreq),var(F5$ClaimFreq))
F1 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area1 == 1 & datatoplot$Leasing1 ==0),c("Exposure", "ClaimFreq")]
F12 <- c(sum(F1$Exposure),mean(F1$ClaimFreq),var(F1$ClaimFreq))
F2 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area2 == 1 & datatoplot$Leasing1 == 0),c("Exposure", "ClaimFreq")]
F22 <- c(sum(F2$Exposure),mean(F2$ClaimFreq),var(F2$ClaimFreq))
F3 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area3 == 1 & datatoplot$Leasing1 == 0),c("Exposure", "ClaimFreq")]
F32 <- c(sum(F3$Exposure),mean(F3$ClaimFreq),var(F3$ClaimFreq))
F4 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area4 == 1 & datatoplot$Leasing1 == 0),c("Exposure", "ClaimFreq")]
F42 <- c(sum(F4$Exposure),mean(F4$ClaimFreq),var(F4$ClaimFreq))
F5 <- datatoplot[which(datatoplot$Gender1 == 0 & datatoplot$Area4 == 0 & datatoplot$Area3 == 0 & datatoplot$Area2 == 0 & datatoplot$Area1 == 0 & datatoplot$Leasing1 == 0),c("Exposure", "ClaimFreq")]
F52 <- c(sum(F5$Exposure),mean(F5$ClaimFreq),var(F5$ClaimFreq))
H1 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area1 == 1, datatoplot$Leasing1 == 1),c("Exposure", "ClaimFreq")]
H11 <- c(sum(H1$Exposure),mean(H1$ClaimFreq),var(H1$ClaimFreq))
H2 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area2 == 1, datatoplot$Leasing1 == 1),c("Exposure", "ClaimFreq")]
H21 <- c(sum(H2$Exposure),mean(H2$ClaimFreq),var(H2$ClaimFreq))
H3 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area3 == 1, datatoplot$Leasing1 == 1),c("Exposure", "ClaimFreq")]
H31 <- c(sum(H3$Exposure),mean(H3$ClaimFreq),var(H3$ClaimFreq))
H4 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area4 == 1, datatoplot$Leasing1 == 1),c("Exposure", "ClaimFreq")]
H41 <- c(sum(H4$Exposure),mean(H4$ClaimFreq),var(H4$ClaimFreq))
H5 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area4 == 0 & datatoplot$Area3 == 0 & datatoplot$Area2 == 0 & datatoplot$Area1 == 0, datatoplot$Leasing1 == 1),c("Exposure", "ClaimFreq")]
H51 <- c(sum(H5$Exposure),mean(H5$ClaimFreq),var(H5$ClaimFreq))
H1 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area1 == 1, datatoplot$Leasing1 == 0),c("Exposure", "ClaimFreq")]
H12 <- c(sum(H1$Exposure),mean(H1$ClaimFreq),var(H1$ClaimFreq))
H2 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area2 == 1, datatoplot$Leasing1 == 0),c("Exposure", "ClaimFreq")]
H22 <- c(sum(H2$Exposure),mean(H2$ClaimFreq),var(H2$ClaimFreq))
H3 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area3 == 1, datatoplot$Leasing1 == 0),c("Exposure", "ClaimFreq")]
H32 <- c(sum(H3$Exposure),mean(H3$ClaimFreq),var(H3$ClaimFreq))
H4 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area4 == 1, datatoplot$Leasing1 == 0),c("Exposure", "ClaimFreq")]
H42 <- c(sum(H4$Exposure),mean(H4$ClaimFreq),var(H4$ClaimFreq))
H5 <- datatoplot[which(datatoplot$Gender1 == 1 & datatoplot$Area4 == 0 & datatoplot$Area3 == 0 & datatoplot$Area2 == 0 & datatoplot$Area1 == 0, datatoplot$Leasing1 == 0),c("Exposure", "ClaimFreq")]
H52 <- c(sum(H5$Exposure),mean(H5$ClaimFreq),var(H5$ClaimFreq))
datatoplot2 <-as.data.frame(t(cbind(F11,F21,F31,F41,F51,F12,F22,F32,F42,F52,H11,H21,H31,H41,H51,H12,H22,H32,H42,H52)))
colnames(datatoplot2) <- c("Exposure", "Mean", "Var")
datatoplot2$Group <- c("F11","F21","F31","F41","F51","F12","F22","F32","F42","F52","H11","H21","H31","H41","H51","H12","H22","H32","H42","H52")


ggplot(datatoplot2) + geom_point(aes(x=Mean, y=Var, colour = Group)) + 
  geom_circle(aes(x0=Mean, y0=Var, r = sqrt(Exposure/pi)/50000, colour = Group)) + geom_abline(intercept = 0, slope = 1) + 
  labs(title = "Variance versus mean Claim Freq in groups (circle = exposure)", y = "Variance", x = "Mean") +
  geom_smooth(aes(x=Mean, y=Var), method = "lm", se = FALSE, colour="red", size=0.5)

