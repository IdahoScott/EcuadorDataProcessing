#Final Analysis Script

library(glmmLasso)
library(pspearman)


setwd('/Users/carlyscott/Documents/EcuadorDataProcessing/')
water_qual_perc <- read.csv("EnglishWorkingFile.csv", header = T, na.strings='999')

#Create an indicator of water quality perception from three other catergories
water_qual_perc$TotalStreamQual <- (water_qual_perc$StreamColor + water_qual_perc$StreamOdor + water_qual_perc$StreamQuality)/3

#=======Handle Usage Amount===========

#Create a variable for usage amount as a sum of individual uses
water_qual_perc$UsageAmount <- water_qual_perc$DrinkCook + water_qual_perc$Irrigation + water_qual_perc$CattleTroughs + water_qual_perc$PersonalHygene + water_qual_perc$WashClothes + water_qual_perc$Recreation + water_qual_perc$RiverSports + water_qual_perc$PureAir + water_qual_perc$NaturalRelaxxation

#Due to uneven sample sizes, create three bins of usage:
for (i in 1:dim(water_qual_perc)[1]){
  if (water_qual_perc$UsageAmount[i] == 0){
    water_qual_perc$UsageBins[i] <- 0
  }
  if (water_qual_perc$UsageAmount[i] == 1 | water_qual_perc$UsageAmount[i] ==2 | water_qual_perc$UsageAmount[i] == 3){
    water_qual_perc$UsageBins[i] <- 1
  }
  if (water_qual_perc$UsageAmount[i] ==4 | water_qual_perc$UsageAmount[i] ==5 | water_qual_perc$UsageAmount[i] ==6 | water_qual_perc$UsageAmount[i] ==7 | water_qual_perc$UsageAmount[i] ==8| water_qual_perc$UsageAmount[i] ==9)
  { #Found a mistake
    water_qual_perc$UsageBins[i] <- 2
  }
}

#=======Create a service amount indicator as the sum of all individual services======
water_qual_perc$ServiceAmount <- water_qual_perc$PotableWater + water_qual_perc$Piped.Water + water_qual_perc$WaterQuebadra +
  water_qual_perc$Sewer + water_qual_perc$Telephone + water_qual_perc$Internet + water_qual_perc$PavedStreets 


#=============Create response variable =================
#Create a binary indicator of water quality perception based on mean

hilototalstream <- vector()
for (i in 1:length(water_qual_perc$TotalStreamQual))
{
  if (water_qual_perc$TotalStreamQual[i] < 2.24) #2.24 is the mean of TotalStreamQual
  {
    hilototalstream[i] = 0
  }
  if (water_qual_perc$TotalStreamQual[i] >= 2.24)
  {
    hilototalstream[i] = 1
  }
}
water_qual_perc$hilo <- hilototalstream  #Assign this indicator to the dataframe


#=========Clean your data================
#This section of the code does some serious data wrangling. Basically, every variable
#needs to be manually converted to a factor, and the levels of the factor need to be assigned
#based on the survey structure. Some columns (NULL) need to be deleted because they contain natural
#language responses, or are not relevant to the analysis.

colnames(water_qual_perc)[1] <- 'ComCode' #Because the spanish accents in some areas give encoding errors
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
water_qual_perc$UsageBins <- factor(water_qual_perc$UsageBins, ordered = T, levels=c(0, 1, 2))

water_qual_perc <- na.omit(water_qual_perc) #Crucial for dealing with factors, otherwise regression modeling will fail 

#=========Check for collinearity===================
#Use Spearman's Rho to check for correlation

rho_mat <- matrix(NA, nrow = dim(water_qual_perc)[2], ncol = nrow = dim(water_qual_perc)[2]) #Create an empty nxn matrix
for (i in 1:length(water_qual_perc)){
  for (j in 1:length(water_qual_perc)){
    foo <- spearman.test(water_qual_perc[,i], water_qual_perc[,j]) #create a dummy variable
    if (foo$p.value <= 0.05) #If the correlation is significant...
    {
      rho_mat[i, j] <- foo$estimate #Assign it to the rho_matirx
    }
  }
}

rho_df <- as.data.frame(rho_mat) 
colnames(rho_df) <- colnames(water_qual_perc) #Assign names to the nxn dataframe
rownames(rho_df) <- colnames(water_qual_perc)

#write it out to a csv, for easy conditional formatting. Attatch final matrix in appendix of report
write.csv(rho_df, 'SansServiceCorrCheck.csv')

#=========Create your model and optimize lamda(shrinkage parameter)======
lambdas <- seq(0, 200, 5) #Attempt every lambda between 0 and 200 in intervals of 5
lambda_opt <- matrix(data = NA, nrow = 40, ncol = 2) #Create an empty matrix
for (i in lambdas){ #This snippet takes a little bit to run
  nonconflcit_glmm <- glmmLasso(hilo~ComType+ ServiceAmount + ImpWaterQuality + ImpElectricity + ImpRoads + ImpTrash +
                                  ImpPublicSanitation + ImpPublicTransport + ImpPublicSchool + ImpAirQuality +
                                  ProAgriChem +
                                  ProTrash + ProWaterContam + ProAirContan + Mosquitos + Traffic + Dust + Noise + Watewater +
                                  WaterQuality + DrinkCook + Irrigation + CattleTroughs + WashClothes +
                                  Recreation + RiverSports + PureAir +
                                  NaturalRelaxxation  + WaterServices + AgriculturalUse + LiveStock + Debris + Mining +
                                  Deforestation + ApplyLaws + Fines + Incentives + EducationPrograms + Gender + Age + Education +
                                  Residency + UsageBins + UTMY, rnd = list(ComNumber=~1), lambda=i, data=water_qual_perc,
                                family=binomial(link="logit"))
  lambda_opt[i/5,1] <- i
  lambda_opt[i/5,2] <- nonconflcit_glmm$aic
  
}
plot(lambda_opt) #shows the behavior of AIC with changing lambda
l_opt <- lambda_opt[lambda_opt[,2] == min(lambda_opt[,2]),][1] #l_opt is your optimal lambda by AIC minimization

#Use optimal lambda to create final model

surveydata_glmm <- glmmLasso(hilo~ComType+ ServiceAmount + ImpWaterQuality + ImpElectricity + ImpRoads + ImpTrash +
                                ImpPublicSanitation + ImpPublicTransport + ImpPublicSchool + ImpAirQuality +
                                ProAgriChem +
                                ProTrash + ProWaterContam + ProAirContan + Mosquitos + Traffic + Dust + Noise + Watewater +
                                WaterQuality + DrinkCook + Irrigation + CattleTroughs + WashClothes +
                                Recreation + RiverSports + PureAir +
                                NaturalRelaxxation  + WaterServices + AgriculturalUse + LiveStock + Debris + Mining +
                                Deforestation + ApplyLaws + Fines + Incentives + EducationPrograms + Gender + Age + Education +
                                Residency + UsageBins + UTMY, rnd = list(ComNumber=~1), lambda=l_opt, data=water_qual_perc,
                              family=binomial(link="logit"))

coeffs <- as.data.frame(as.matrix(surveydata_glmm$coefficients)) #grab lasso-returned coefficients
coeffs$ABS_VAL <- abs(coeffs$V1) #take absolute value to deem importance, regardless of direction
sorted_coeffs <- coeffs[order(-coeffs$ABS_VAL),] #sort coefficients by importance
colnames(sorted_coeffs) <- c("LASSO LogOdds Coeff", "|LASSO Coeff|") #Rename columns to something meaningful
sorted_coeffs #print the sorted coefficients
write.csv(sorted_coeffs, 'Coeffs_fixed_usage_ResolveError.csv') #Write the sorted coefficients to a csv


#==========Create Plots====================
#Usage Cases
sorted_coeffs$OddsRatio <- exp(sorted_coeffs$`LASSO LogOdds Coeff`) #convert log odds to the more interpretable "odds ratio"
binary_vars <- c('DrinkCook1', 'PureAir1', 'WashClothes1', 'Irrigation1', 'RiverSports1', 'CattleTroughs1', 'NaturalRelaxxation1') #From exported csv, enter names of significant use cases
binary_vars_sub <- subset(sorted_coeffs, rownames(sorted_coeffs) %in% binary_vars)
binary_vars_sub$dist <- abs(binary_vars_sub$OddsRatio - 1)
binary_vars_sub <- binary_vars_sub[order(-binary_vars_sub$dist),] 

legend_cols <- c('aquamarine4', 'blue', 'slateblue2', 'orchid3', 'turquoise2', 'navyblue', 'seagreen2')
par(mar=c(5.1, 4.1, 4.1, 10.1),xpd=TRUE)
plot(binary_vars_sub$OddsRatio, xaxt = 'n', ylim = c(0,2), pch = 19, cex = 1.8, col = legend_cols, main  = 'Relative odds ratios of above average WQP \nfor usage cases', ylab = 'Relative odds ratio', xlab = '')
lines(c(0.75, 7.25), c(1, 1), col = 'darkgrey', lty = 'dashed')

for (i in 1:7){
  lines(c(i, i), c(1, binary_vars_sub$OddsRatio[i]), lwd = 2, col = legend_cols[i])
}
#text(2, 0.9, '1:1 Odds Ratio', cex = 0.8, col ='gray42')
legend(7.3, 2, c('Drinking/Cooking', 'Pure Air', 'Washing Clothes', 'Irrigation', 'River Sports', 'Cattle Troughs', 'Natural Relaxation'), bty = 'n', col = legend_cols, pch = 19, lty = 1)

#Improve Public Services

#ImpPublicSanitation
imp_pubsan <- function(x) exp(-.07*x -0.0647*x^2) #These are coming from polynomial parts listed in csv

#ImpTrash
imp_trash <- function(x) exp(-0.05413*x)

#ImpPublicTran
imp_tran <- function(x) exp(-0.038*x)

#ImpElectricity

imp_electric <- function(x) exp(-0.03598*x^2)

#ImpRoads
imp_roads <- function(x) exp(-0.0243*x^2 - 0.019*x)

xs <- c(seq(0.25,1.25,0.25), seq(1.75,2.75, 0.25), seq(3, 4, 0.25))
odds <- c(imp_pubsan(1), imp_trash(1), imp_electric(1), imp_roads(1), imp_tran(1), imp_pubsan(2), imp_trash(2), imp_electric(2), imp_roads(2),imp_tran(2),
          imp_pubsan(3), imp_trash(3), imp_electric(3), imp_roads(3), imp_tran(3))
group <- c(rep(1, 5), rep(2, 5), rep(3, 5))
treatment <- rep(c('pubsan', 'trash', 'electric', 'roads', 'tran'), 3)
color <- rep(c("#FBCF35FF", "#ED4C1CFF", "#9C7E70FF", "#5AC2F1FF", "#11776CFF" ), 3)
discrete_vars <- data.frame(odds, group, treatment, color) ##Create a function to make these plots?
discrete_vars$dist <- abs(discrete_vars$odds - 1)
discrete_vars <- discrete_vars[
  with(discrete_vars, order(group, -dist)),
  ]
discrete_vars$x <- xs
par(mar=c(5.1, 4.1, 4.1, 9.1),xpd=TRUE)
plot(discrete_vars$x, discrete_vars$odds, pch = 19, cex = 1.6, ylim = c(0,1.5), col = as.character(discrete_vars$color), xaxt = 'n', ylab = 'Relative odds ratio', xlab = 'Degree of Desire', main = 'Relative odds ratio of above average WQP \nbased on desire to improve public services')
lines(c(0.1, 4.15), c(1, 1), col = 'darkgrey', lty = 'dashed')
for (i in 1:15){
  lines(c(discrete_vars$x[i], discrete_vars$x[i]), c(1, discrete_vars$odds[i]), lwd = 2, col = as.character(discrete_vars$color[i]))
}

xtick<-c(0.75, 2.25, 3.5)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = c('Low', 'Medium', 'High'), pos = 1, xpd = TRUE, offset = 0.7)
legend(4.15,1.5, c('Public Sanitation', 'Trash Collection', 'Electricity', 'Roads', 'Transit'), bty = 'n', col = color, pch = 19, lty = 1, pt.cex = 1, cex = 0.9)

#Environmental and Water Concerns
#Deforestation
def <- function(x) exp(.55109*x -(.3349*x^2)) #Again these formulas come from coefficient csv

#Dust
dust <- function(x) exp(-.1878*x^2 + 0.0128*x^3) 

#ProAirContam
aircontam <- function(x) exp(0.0244*x^2 - (.02216*x^3))

#create a discrete plot from this information
xs <- c(0.25, 0.5, 0.75, 1.25, 1.5, 1.75, 2.25, 2.5, 2.75)
odds <- c(def(1), dust(1), aircontam(1), def(2), dust(2), aircontam(2), def(3), dust(3), aircontam(3))
group <- c(rep(1, 3), rep(2, 3), rep(3, 3))
color <- rep(c('darkorchid3', 'orange3', 'gold2'), 3)
treatment <- rep(c('def', 'dust',  'aircontam'), 3)
discrete_vars <- data.frame(odds, group, treatment, color)
discrete_vars$dist <- abs(discrete_vars$odds - 1)
discrete_vars <- discrete_vars[
  with(discrete_vars, order(group, -dist)),
  ]
discrete_vars$x <- xs

par(mar=c(5.1, 4.1, 4.1, 9.1),xpd=TRUE)
plot(discrete_vars$x, discrete_vars$odds, pch = 19, cex = 1.6, ylim = c(0,2), col = as.character(discrete_vars$color), xaxt = 'n', ylab = 'Relative odds ratio', xlab = 'Degree of concern', main = 'Relative odds ratio of above average WQP \nbased on degree of environmental concerns')
#gap.plot(discrete_vars$x, discrete_vars$odds, gap = c(3.5, 14), ytics = c(seq(0, 3.5, .5), seq(14, 15.5, 0.5)), pch = 19, cex = 1.6, col = as.character(discrete_vars$color), xaxt = 'n', ylab = 'Odds Ratio', xlab = '', main = 'Change in Odds Ratio based on Degree of Concern')
lines(c(0.15, 2.85), c(1, 1), col = 'darkgrey', lty = 'dashed')
for (i in 1:9){
  lines(c(discrete_vars$x[i], discrete_vars$x[i]), c(1, discrete_vars$odds[i]), lwd = 2, col = as.character(discrete_vars$color[i]))
}
xtick<-c(0.5, 1.5,2.5)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = c('Low Concern', 'Medium Concern', 'High Concern'), pos = 1, xpd = TRUE, offset = 0.7)
legend(3,2, c('Deforestation', 'Dust', 'Air \n Contamination'), bty = 'n', col = color, pch = 19, lty = 1, pt.cex = 1, cex = 0.8)

#Usage Bin Behavior
dev.off()
usage <- function(x) exp(.6144*x)
usage_vec <- vector()
xs <- seq(0, 2, 1)
for (i in 1:3){
  usage_vec[i] <- usage(i - 1)
}

plot(xs, usage_vec, pch = 19, ylim = c(0, 3.5), xaxt = 'n', xlab = '', cex = 1.8, col = 'darkblue', ylab = 'Relative odds ratio', main = 'Relative odds ratio of above average WQP \nbased on degree of usage')
#lines(xs, usage_vec, col = 'darkblue')
for (i in 1:3){
  lines(c(i-1, i-1), c(1, usage_vec[i]), lwd = 2, col = 'navyblue')
}
abline(h = 1, col ='darkgrey', lty = 'dashed')
xtick<-c(0, 1, 2)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = c('No Usage', 'Low Usage', 'High Usage'), pos = 1, xpd = TRUE, offset = 0.7)


