library(sp)
library(rgdal)
library(raster)
library(spdep)
library(mctest)
library(glmmLasso)

setwd('C:/Users/Carly Scott/Documents/Ecuador Social Science/')
EnglishWorkingFile <- read.csv("EnglishWorkingFile.csv", header = T, na.strings='999')
EnglishWorkingFile$TotalStreamQual <- (EnglishWorkingFile$StreamColor + EnglishWorkingFile$StreamOdor + EnglishWorkingFile$StreamQuality)/3
EnglishWorkingFile$UsageAmount <- EnglishWorkingFile$DrinkCook + EnglishWorkingFile$Irrigation + EnglishWorkingFile$CattleTroughs + EnglishWorkingFile$PersonalHygene + EnglishWorkingFile$WashClothes + EnglishWorkingFile$Recreation + EnglishWorkingFile$RiverSports + EnglishWorkingFile$PureAir + EnglishWorkingFile$NaturalRelaxxation


##Create a service amount indicator
EnglishWorkingFile$ServiceAmount <- EnglishWorkingFile$PotableWater + EnglishWorkingFile$Piped.Water + EnglishWorkingFile$WaterQuebadra +
  EnglishWorkingFile$Sewer + EnglishWorkingFile$Telephone + EnglishWorkingFile$Internet + EnglishWorkingFile$PavedStreets 
water_qual_perc <- EnglishWorkingFile
hilototalstream <- vector()
for (i in 1:length(water_qual_perc$TotalStreamQual))
{
  if (water_qual_perc$TotalStreamQual[i] < 2.24)
  {
    hilototalstream[i] = 0
  }
  if (water_qual_perc$TotalStreamQual[i] >= 2.24)
  {
    hilototalstream[i] = 1
  }
}
water_qual_perc$hilo <- hilototalstream
barplot(table(as.factor(water_qual_perc$hilo)), xlab = "Binary PWQ Indicator", ylab = "Count", main = "Distribution of Above Average vs. Below Average PWQ", xaxt="n")


services <- c("PotableWater", "Piped.Water", "WaterQuebadra", "Sewer", "Telephone", "Internet", "PavedStreets", 'GarbageCollection', 'Electricity')

#===============================================================================================
#Explore Service Frequency
par(mfrow=c(3,3))
for (service in services){
  barplot(table(water_qual_perc[[service]]), xaxt = "n", main = service)
  xtick<-seq(1, 2, by=1)
  axis(side=1, at=xtick, labels = FALSE)
  text(x=xtick,  par("usr")[3], 
       labels = c("Not Present", "Present"), pos = 1, xpd = TRUE, offset = 0.7)
}


#Potentially Interesting Services: PotableWater, Piped Water, Sewer, Telephone, Internet, Pavedstreets
##Almost always present: Garbage Collection and Electricity
##Mostly absent: WaterQuebadra 
#==============================================================================================


colnames(water_qual_perc)[1] <- 'ComCode'
water_qual_perc$ComCode <- NULL
water_qual_perc$Name <- NULL
water_qual_perc$Date <- NULL 
water_qual_perc$ComName <- NULL
water_qual_perc$Other <- NULL
water_qual_perc$OtherProgram <- NULL
water_qual_perc$OtherSpec <- NULL
water_qual_perc$birdsamphibs <- NULL
water_qual_perc$X <- NULL
water_qual_perc$X.1 <- NULL
water_qual_perc$X.2 <- NULL
water_qual_perc$Initiatives.of.Interest <- NULL
water_qual_perc$KindofChange <- NULL
water_qual_perc$Climate <- NULL
water_qual_perc$ChangeinClimate <- NULL
water_qual_perc$Quality.of.Water <- NULL
water_qual_perc$Profession <- NULL
water_qual_perc$Ninguno <- NULL

water_qual_perc$ComNumber <- as.factor(water_qual_perc$ComNumber)
water_qual_perc$Gender <- as.factor(water_qual_perc$Gender)
water_qual_perc$Education <- factor(water_qual_perc$Education, ordered = TRUE, levels = c(1, 2, 3, 4, 5))
water_qual_perc$Residency <- as.factor(water_qual_perc$Residency)

water_qual_perc$PotableWater <- factor(water_qual_perc$PotableWater)
water_qual_perc$Piped.Water <- factor(water_qual_perc$Piped.Water)
water_qual_perc$WaterQuebadra <- factor(water_qual_perc$WaterQuebadra)
water_qual_perc$Sewer <- factor(water_qual_perc$Sewer)
water_qual_perc$Telephone <- factor(water_qual_perc$Telephone)
water_qual_perc$Internet <- factor(water_qual_perc$Internet)
water_qual_perc$PavedStreets <- factor(water_qual_perc$PavedStreets)
water_qual_perc$GarbageCollection <- factor(water_qual_perc$GarbageCollection)
water_qual_perc$Internet <- factor(water_qual_perc$Internet)
water_qual_perc$Electricity <- factor(water_qual_perc$Electricity)
water_qual_perc$ServiceLevel <- factor(water_qual_perc$ServiceLevel, ordered = TRUE, levels=c(1, 2))
water_qual_perc$Basic.Services <- factor(water_qual_perc$Basic.Services, ordered = TRUE, levels=c(3, 2, 1))
water_qual_perc$ImpWaterQuality <- factor(water_qual_perc$ImpWaterQuality, ordered = TRUE, levels = c(1, 2, 3))
water_qual_perc$ImpElectricity <- factor(water_qual_perc$ImpElectricity, ordered = TRUE, levels = c(1, 2, 3))
water_qual_perc$ImpRoads <- factor(water_qual_perc$ImpRoads, ordered = TRUE, levels = c(1, 2, 3))
water_qual_perc$ImpTrash <- factor(water_qual_perc$ImpTrash, ordered = TRUE, levels = c(1, 2, 3))
water_qual_perc$ImpPublicSanitation <- factor(water_qual_perc$ImpPublicSanitation, ordered = TRUE, levels = c(1, 2, 3))
water_qual_perc$ImpPublicTransport <- factor(water_qual_perc$ImpPublicTransport, ordered = TRUE, levels = c(1, 2, 3))
water_qual_perc$ImpPublicSchool <- factor(water_qual_perc$ImpPublicSchool,ordered = TRUE, levels = c(1, 2, 3))
water_qual_perc$ImpAirQuality <- factor(water_qual_perc$ImpAirQuality, ordered = TRUE, levels = c(1, 2, 3))
water_qual_perc$ImpEnvironment <- factor(water_qual_perc$ImpEnvironment, ordered = TRUE, levels = c(1, 2, 3))
water_qual_perc$ProAgriChem <- factor(water_qual_perc$ProAgriChem, ordered = TRUE, levels = c(0,3,2,1))
water_qual_perc$ProTrash <- factor(water_qual_perc$ProTrash, ordered = TRUE, levels = c(0,3,2,1))
water_qual_perc$ProWaterContam <- factor(water_qual_perc$ProWaterContam, ordered = TRUE, levels = c(0, 3, 2, 1))
water_qual_perc$ProAirContan <- factor(water_qual_perc$ProAirContan, ordered = TRUE, levels = c(0, 3, 2, 1))
water_qual_perc$Mosquitos <- factor(water_qual_perc$Mosquitos, ordered = TRUE, levels = c(0, 3, 2, 1))
water_qual_perc$Traffic <- factor(water_qual_perc$Traffic, ordered = TRUE, levels = c(0, 3, 2, 1))
water_qual_perc$Dust <- factor(water_qual_perc$Dust, ordered = TRUE, levels = c(0, 3, 2, 1))
water_qual_perc$Noise <- factor(water_qual_perc$Noise, ordered = TRUE, levels = c(0, 3, 2, 1))
water_qual_perc$Watewater <- factor(water_qual_perc$Watewater, ordered = TRUE, levels = c(0, 3, 2, 1))
water_qual_perc$WaterQuality <- factor(water_qual_perc$WaterQuality, ordered = TRUE, levels = c(1, 2, 3, 4, 5))
water_qual_perc$DrinkCook <- factor(water_qual_perc$DrinkCook)
water_qual_perc$Irrigation <- factor(water_qual_perc$Irrigation)
water_qual_perc$CattleTroughs <- factor(water_qual_perc$CattleTroughs)
water_qual_perc$PersonalHygene <- factor(water_qual_perc$PersonalHygene)
water_qual_perc$WashClothes <- factor(water_qual_perc$WashClothes)
water_qual_perc$Recreation <- factor(water_qual_perc$Recreation)
water_qual_perc$RiverSports <- factor(water_qual_perc$RiverSports)
water_qual_perc$PureAir <- factor(water_qual_perc$PureAir)
water_qual_perc$NaturalRelaxxation <- factor(water_qual_perc$NaturalRelaxxation)
water_qual_perc$Ninguno <- factor(water_qual_perc$Ninguno)
water_qual_perc$WaterServices <- factor(water_qual_perc$WaterServices, ordered = TRUE, levels = c(0, 3, 2, 1))
water_qual_perc$AgriculturalUse <- factor(water_qual_perc$AgriculturalUse, ordered = TRUE, levels = c(0, 3, 2, 1))
water_qual_perc$LiveStock <- factor(water_qual_perc$LiveStock, ordered = TRUE, levels = c(0, 3, 2, 1))
water_qual_perc$Debris <- factor(water_qual_perc$Debris, ordered = TRUE, levels = c(0, 3, 2, 1))
water_qual_perc$Mining <- factor(water_qual_perc$Mining, ordered = TRUE, levels = c(0, 3, 2, 1))
water_qual_perc$Deforestation <- factor(water_qual_perc$Deforestation, ordered = TRUE, levels = c(0, 3, 2, 1))
water_qual_perc$ApplyLaws <- factor(water_qual_perc$ApplyLaws, ordered= TRUE, levels = c(0, 3, 2, 1))
water_qual_perc$Fines <- factor(water_qual_perc$Fines, ordered= TRUE, levels = c(0, 3, 2, 1))
water_qual_perc$Incentives <- factor(water_qual_perc$Incentives, ordered= TRUE, levels = c(0, 3, 2, 1))
water_qual_perc$EducationPrograms <- factor(water_qual_perc$EducationPrograms, ordered= TRUE, levels = c(0, 3, 2, 1))
water_qual_perc$Age <- factor(water_qual_perc$Age, ordered = TRUE, levels = c(1, 2, 3, 4))
water_qual_perc$UsageAmount <- factor(water_qual_perc$UsageAmount, ordered = TRUE, levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
water_qual_perc$ServiceAmount <- factor(water_qual_perc$ServiceAmount, ordered = T, levels=c(0,1,2,3,4,5,6,7))

water_qual_perc <- na.omit(water_qual_perc) #Crucial for dealing with factors 


#======================================
#Model Omitting covariate factors
nonconflcit_glmm <- glmmLasso(hilo~ComType+ ServiceAmount + ImpWaterQuality + ImpElectricity + ImpRoads + ImpTrash +
                                                  ImpPublicSanitation + ImpPublicTransport + ImpPublicSchool + ImpAirQuality +
                                                  ProAgriChem +
                                                  ProTrash + ProWaterContam + ProAirContan + Mosquitos + Traffic + Dust + Noise + Watewater +
                                                  WaterQuality + DrinkCook + Irrigation + CattleTroughs + WashClothes +
                                                  Recreation + RiverSports + PureAir +
                                                  NaturalRelaxxation  + WaterServices + AgriculturalUse + LiveStock + Debris + Mining +
                                                  Deforestation + ApplyLaws + Fines + Incentives + EducationPrograms + Gender + Age + Education +
                                                  Residency + UsageAmount + UTMY, rnd = list(ComNumber=~1), lambda=50, data=water_qual_perc,
                                                family=binomial(link="logit"))
coeffs <- as.data.frame(as.matrix(nonconflcit_glmm$coefficients))
coeffs$ABS_VAL <- abs(coeffs$V1)
sorted_coeffs <- coeffs[order(-coeffs$ABS_VAL),] #55 variables from survey
colnames(sorted_coeffs) <- c("LASSO LogOdds Coeff", "|LASSO Coeff|")
sorted_coeffs 


write.csv(sorted_coeffs, "SansServicesModel.csv")


#==============Spearman's Rho==============================================
#Use Spearman's Rho to check for correlation

rho_mat <- matrix(NA, nrow = 66, ncol = 66)
for (i in 1:length(water_qual_perc)){
  for (j in 1:length(water_qual_perc)){
    foo <- spearman.test(water_qual_perc[,i], water_qual_perc[,j])
    if (foo$p.value <= 0.05)
    {
    rho_mat[i, j] <- foo$estimate
  }
  }
}

rho_df <- as.data.frame(rho_mat)
colnames(rho_df) <- colnames(water_qual_perc)
rownames(rho_df) <- colnames(water_qual_perc)

#write it out to a csv, for easy conditional formatting. Attatch final matrix in appendix of report
write.csv(rho_df, 'SansServiceCorrCheck.csv')
#===============ProbCalc==================================================
#Function to convert binary log odds to probability change
prob_calc <- function(x, startprob){
  logstart <- log(startprob/(1-startprob))
  p <- exp(x + logstart)/(1+exp(x + logstart))
  return(p)
}

poly_prob_calc <- {}
#==============================================================

#Usage Amount Curve
curve(exp(1.8902*x - 1.04)/(1 + exp(1.8902*x - 1.04)),  0, 9, ylim = c(0,1.01), lwd = 1.8, xaxt="n", ylab = 'Probablity of percieving above average PWQ', xlab = 'Usage Amount of Stream', main = "Behavior of Usage Amount")
points(xs, usage_vec, col = 'green4', pch = 19)
lines(xs, usage_vec, col = 'green4', lty = 'longdash', lwd = 1.8)
curve(exp(1.8902*x)/(1+exp(1.8902*x)), add = T, 0, 9, col = 'blue3', lwd = 1.8)
#abline(h = 0.5, col = 'red', lty= 'dotted')
text(x = 7, y = 0.2, labels= 'Adjusted Model Fitted Probability', col = 'black')
text(x = 7, y = 0.1, labels = 'Proportion Respoding High PWQ', col = 'green4')
text(x = 7, y = 0.15, labels = 'Unadjusted Model Fitted Probability', col = 'blue3')
#text(x = 2, y = 0.5, labels = 'Less likely to percieve \n above average PWQ', col = 'red4')
xtick<-seq(0, 9, by=1)
axis(side=1, at=xtick, labels = FALSE) #Relabel xticks
text(x=xtick,  par("usr")[3], 
     labels = xtick, pos = 1, xpd = TRUE, offset = 0.7)

#===============Test for usage amount==================================================
#Explore this extreme exponential connection more
#par(mfrow = c(3,3))
usage_vec <- vector(length = 10)
xs <- seq(0,9,1)
for (i in xs){
  usage_vec[i + 1]<- mean(water_qual_perc[water_qual_perc$UsageAmount == i,]$hilo)
}
plot(xs, usage_vec, main = 'Proportions of High PWQ from Survey')

#=================================================================
##Usage of River for Pure Air
binary_prob_plot('PureAir', 0.6457)

##Usage of River for River Sports
binary_prob_plot('RiverSports', -.63)

#Usage of River to Wash Clothes
binary_prob_plot('WashClothes', .6260)

#usage o
#=====================Function for binaryprobplots==========================================
#Create a function to plot binary probability changes
binary_prob_plot <- function(your_var, model_logodds){
  starting_prob <- summary(water_qual_perc[[your_var]])[2]/length(water_qual_perc[[your_var]])
  model_prob <- prob_calc(model_logodds, starting_prob)
  ys <- c(starting_prob, model_prob) ##how to interpret probabilities? 
  var_step <- stepfun(1, ys, 0)
  plot(var_step, verticals=FALSE, xaxt = 'n', xlim = c(0, 2), ylim = c(0,1), lwd = 2, 
       xlab = 'Binary Presence Indicator', ylab = "Prob. of above average PWQ", main = your_var)
  abline(h=0.5, lty='dotted', col = 'red')
  xtick <- c(0.5, 1.5)
  axis(side = 1, at= xtick, labels = FALSE)
  text(x=xtick, par("usr")[3], labels = c('Absent', 'Present'), pos = 1, xpd = T, offset = 0.7)
  return(ys)
}
#========================================

##Deforestation as major environmental concern
curve(exp(0.62*x -.254*x^2),  0, 3, ylim = c(0,2),xaxt="n", ylab = 'Odds Ratio of percieving above average PWQ', xlab = 'Concern over deforestation', main = "Behavior of Deforestation")
abline(h = 1, col = 'red', lty= 'dashed')
xtick<-seq(0, 3, by=1)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = xtick, pos = 1, xpd = TRUE, offset = 0.7)

##Dust as an environmental problem
curve(exp(-.467*x^2),  0, 3, ylim = c(0,2),xaxt="n", ylab = 'Odds Ratio of percieving above average PWQ', xlab = 'Concern over dust', main = "Behavior of Dust")
abline(h = 1, col = 'red', lty= 'dashed')
xtick<-seq(0, 3, by=1)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = xtick, pos = 1, xpd = TRUE, offset = 0.7)

##Livestock as an environmental concern
curve(exp(0.3*x),  0, 3, ylim = c(0,4),xaxt="n", ylab = 'Odds Ratio of percieving above average PWQ', xlab = 'Concern over livestock', main = "Behavior of Livestock concern")
abline(h = 1, col = 'red', lty= 'dashed')
xtick<-seq(0, 3, by=1)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = xtick, pos = 1, xpd = TRUE, offset = 0.7)

##Weight services from 'common' to 'rare'? create a new metric based on presence, where the lowest would be 


##Agricultural Chemicals as a Main Source of Contamination
curve(exp(-.1875*x^2),  0, 3, ylim = c(0,2),xaxt="n", ylab = 'Odds Ratio of percieving above average PWQ', xlab = 'Concern over agrichem', main = "Behavior of AgriChem")
abline(h = 1, col = 'red', lty= 'dashed')
xtick<-seq(0, 3, by=1)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = xtick, pos = 1, xpd = TRUE, offset = 0.7)

###Desire to improve public sanitation
curve(exp(-.144*x),  0, 3, ylim = c(0,2),xaxt="n", ylab = 'Odds Ratio of percieving above average PWQ', xlab = 'Desire to Improve Public Sanitation', main = "Behavior of ImpPublicSanitation")
abline(h = 1, col = 'red', lty= 'dashed')
xtick<-seq(0, 3, by=1)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = xtick, pos = 1, xpd = TRUE, offset = 0.7)


##Usage Amount
curve(exp(-0.051*x^2),  0, 9, ylim = c(0,2),xaxt="n", ylab = 'Odds Ratio of percieving above average PWQ', xlab = 'Number of Usees', main = "Behavior of Usage Amount")
abline(h = 1, col = 'red', lty= 'dashed')
xtick<-seq(0, 9, by=1)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = xtick, pos = 1, xpd = TRUE, offset = 0.7)


#======test
