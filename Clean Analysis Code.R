##### Sociology Model: Clean Code

##Packages used, if code does not work, double check this list is complete
library(sp)
library(rgdal)
library(raster)
library(spdep)
library(ltm)
library(rms)
library(mctest)
library(spatialreg) #now needed to do spatial filtering
library(ppcor)

##There are different files, one with the base social data, one which incorporates the chemical data. Read both in here.
setwd('C:/Users/Carly Scott/Documents/Ecuador Social Science/')
#EnglishWorkingFile is the perception data
EnglishWorkingFile <- read.csv("EnglishWorkingFile.csv", header = T, na.strings='999')

#Chemloc is the chemical data by location
chemloc <- read.csv("ChemByLocale..csv")

##From here we will go through similar steps with both files. Here we create new columns for the total quality criteria and assign spatial reference

chemloc$TotalQual <- (chemloc$StreamColor + chemloc$StreamOdor + chemloc$StreamQuality)/3

coordinates(chemloc) <- ~UTMX + UTMY
proj4string(chemloc) <- CRS("+init=epsg:32717")
lonlats_chemloc <- spTransform(chemloc, CRS("+init=epsg:4326"))

EnglishWorkingFile$TotalStreamQual <- (EnglishWorkingFile$StreamColor + EnglishWorkingFile$StreamOdor + EnglishWorkingFile$StreamQuality)/3
EnglishWorkingFile$UsageAmount <- EnglishWorkingFile$DrinkCook + EnglishWorkingFile$Irrigation + EnglishWorkingFile$CattleTroughs + EnglishWorkingFile$PersonalHygene + EnglishWorkingFile$WashClothes + EnglishWorkingFile$Recreation + EnglishWorkingFile$RiverSports + EnglishWorkingFile$PureAir + EnglishWorkingFile$NaturalRelaxxation

test_data <- EnglishWorkingFile

coordinates(EnglishWorkingFile) <- ~UTMX + UTMY
proj4string(EnglishWorkingFile) <- CRS("+init=epsg:32717")
lonlats_EnglishFile <- spTransform(EnglishWorkingFile, CRS("+init=epsg:4326"))

#Now we create neighborhoods for each file. This will be used for SpatialFiltering later. The neighborhood for both is k = 5. This is the five nearest neighbors. 

chem.knn <- knearneigh(coordinates(lonlats_chemloc), k = 5)
chem.nb <- knn2nb(chem.knn)
chem.w <- nb2listw(knn2nb(chem.knn), style = "W")

##Please forgive my stupid naming here, any neighborhood starting with pnt is associated with the EnglishWorkingFile
pnts.knn <- knearneigh(coordinates(lonlats_EnglishFile), k = 5)
pnt.nb <- knn2nb(pnts.knn)
pnt.w <- nb2listw(knn2nb(pnts.knn), style = "W")
pnt.mat <- nb2mat(knn2nb(pnts.knn), style = "B")


##But how did we even know spatial dependence was an issue? In order to assess this, I first calculated the Moran's I.
### To do this, I converted the long/lat system to a grid of very small squares. Then in each square, I took the average TotalStreamQuality from the points within it

Long.Range <- c(-4.068951, -3.933070)
Lat.Range <- c(-79.22997, -79.17489)

LojaGrid <- expand.grid(x = seq(from = Lat.Range[1], to = Lat.Range[2], by = 0.0005),
                            y = seq(from = Long.Range[1], to = Long.Range[2], by = 0.0005))
coordinates(LojaGrid) <- ~x + y
projection(LojaGrid) <- CRS("+init=epsg:4326")
gridded(LojaGrid) <- TRUE
fullgrid(LojaGrid) <- TRUE
LojaGrid.r <- raster(LojaGrid)
Data.r <- rasterize(lonlats_EnglishFile, LojaGrid.r, fun = mean) #ignore warnings, they're coming from NA values

Data.poly <- rasterToPolygons(Data.r)
centroids <- coordinates(Data.poly)
ord.k5 <- knn2nb(knearneigh(centroids, k = 5))
ord.w <- nb2listw(ord.k5, style = "W")

Data.poly$lmstreamqual <- lag.listw(ord.w, Data.poly$TotalStreamQual)
moran.test(Data.poly$lmstreamqual, ord.w, randomisation = T, alternative = "two.sided")

##Now we know there is a high degree of global spatial autocorrelation. Let's assign stream quality to a binary indicator for both files.
###In both cases, the cut off is the average stream quality value for that data set.
hilototalstream <- vector()
for (i in 1:length(EnglishWorkingFile@data$TotalStreamQual))
{
  if (EnglishWorkingFile@data$TotalStreamQual[i] < 2.24)
  {
    hilototalstream[i] = 0
  }
  else
  {
    hilototalstream[i] = 1
  }
}
EnglishWorkingFile@data$hilo <- hilototalstream

binstream <- vector()
for (i in 1:length(chemloc@data$TotalQual))
{
  if (chemloc@data$TotalQual[i] <= 2.1873)
  {
    binstream[i] = 0
  }
  else
  {
    binstream[i] = 1
  }
}

chemloc$hilo <- binstream

#We also need to know the multicollinearity between the variables. There wasn't any detected for the EnglishWorkingFile using a VIF test
#For chemloc file
cov.matrix <- cbind(chemloc$Temp, chemloc$pH, chemloc$Cond, chemloc$DO, chemloc$Nitrate, chemloc$Nitrite, chemloc$Ammonium, 
                    chemloc$DIN, chemloc$DRP, chemloc$Turbidity, chemloc$Col.Total, chemloc$Col.Fecal, chemloc$QBR.And, chemloc$HFI)
cov.df <- as.data.frame(cov.matrix)
colnames(cov.df) <- c("Temp", "pH", "Cond", "DO", "Nitrate", "Nitrite", "Ammonium", "DIN", "DRP", "Turbidity", "Col.Total", "Col.Fecal", "QBR", "HFI")
omccoll <- omcdiag(cov.df, chemloc$hilo)
imccoll <- imcdiag(cov.df, chemloc$hilo, method = "VIF")
corrcoeffs <- pcor(cov.df, method = "pearson")
#For EnglishWorkingFile
explanitory.df <- cbind(EnglishWorkingFile$ComType, EnglishWorkingFile$Age, EnglishWorkingFile$Gender,
                        EnglishWorkingFile$Education, EnglishWorkingFile$Residency, EnglishWorkingFile$PotableWater, 
                        EnglishWorkingFile$GarbageCollection, EnglishWorkingFile$DrinkCook, EnglishWorkingFile$WashClothes, 
                        EnglishWorkingFile$PureAir)
colnames(explanitory.df) <- c("CommunityType", "Age", "Gender", "Education", "Residency", "PotableWater", "GarbageCollection",
                              "DrinkCook", "WashClothes", "PureAir")
omc.soc <- omcdiag(explanitory.df, EnglishWorkingFile$hilo)
imc.soc <- imcdiag(explanitory.df, EnglishWorkingFile$hilo, method = "VIF")
corrcoeffs.soc <- pcor(explanitory.df, method = "pearson")

##However, the chemloc file still has a couple touchy variables. The total fecal counts are way out of scale with the other variables
###Here, I divide by 100 to put it in units "Hundreds of Colonies" rather than "Colonies"
chemloc$hundfecal <- chemloc$Col.Fecal/100

#Now for both models, we need to run the SpatialFiltering function, then incorporate it in the glm
#The full demographic model
demo.soc.sf <- SpatialFiltering(hilo ~ as.factor(ComType) + Age + Gender + Education + Residency + PotableWater + GarbageCollection + DrinkCook + WashClothes + PureAir, data = EnglishWorkingFile@data, nb = pnt.nb, style = "W" )
demo.soc.sf.bin <- glm(hilo ~ fitted(demo.soc.sf) + as.factor(ComType)+ Age + Gender + Education + Residency + PotableWater + GarbageCollection + DrinkCook + WashClothes + PureAir, data = EnglishWorkingFile@data, family = binomial)
demo.soc.lrm <- lrm(hilo ~ fitted(demo.soc.sf) + ComType + Age + Gender + Education + Residency + PotableWater + GarbageCollection + DrinkCook + WashClothes + PureAir, data = EnglishWorkingFile@data, x = TRUE, y = TRUE )

#The best only-social model
best.soc.sf <- SpatialFiltering(hilo ~ Age + PotableWater + GarbageCollection + DrinkCook + WashClothes + PureAir, data = EnglishWorkingFile@data, nb = pnt.nb, style = "W")
best.soc.sf.bin <- glm(hilo ~ Age + PotableWater + GarbageCollection + DrinkCook + WashClothes + PureAir + fitted(best.soc.sf), data = EnglishWorkingFile@data, family = binomial)
best.soc.lrm <- lrm(hilo ~ Age + PotableWater + GarbageCollection + DrinkCook + WashClothes + PureAir + fitted(best.soc.sf), data = EnglishWorkingFile@data, x = TRUE, y = TRUE)
#The best chemical-social model, with collinear variables removed- careful, these SpatialFiltering steps take a long time to process
socchem.sf <- SpatialFiltering(hilo ~ Age + DrinkCook + WashClothes + PureAir + 
                                 Nitrate + hundfecal + pH, ExactEV = TRUE, data = chemloc@data, nb = chem.nb, style = "W")
socchem.bin <- glm(hilo ~ Age + DrinkCook + WashClothes + PureAir + 
                     Nitrate + hundfecal + pH + fitted(socchem.sf), data = chemloc@data, family = binomial)
socchem.lrm <- lrm(hilo ~ Age + DrinkCook + WashClothes + PureAir + 
                     Nitrate + hundfecal + pH + fitted(socchem.sf), data = chemloc@data, x=TRUE, y = TRUE)

#One of the criteria we are using, the c-index, is coming from the lrm function. However, this is frequently over estimated and has to be corrected.
#To correct we will use the validate function, and bootstrap 1000 values. Then the function will give us a corrected Dxy. To calculate the corrected c-index: (1 + Dxy)/2

soccchem.valid <- validate(socchem.lrm, method = "boot", B = 1000)
demo.soc.valid <- validate(demo.soc.lrm, method = "boot", B = 1000)
best.soc.valid <- validate(best.soc.lrm, method = "boot", B = 1000)


#######################Lasso Regression
library(caret)
library(glmnet)
library(glmmLasso)
library(tidyverse)

hilototalstream <- vector()
for (i in 1:length(test_data$TotalStreamQual))
{
  if (test_data$TotalStreamQual[i] < 2.24)
  {
    hilototalstream[i] = 0
  }
  else
  {
    hilototalstream[i] = 1
  }
}
test_data$hilo <- hilototalstream
test_data$ï..ComCode <- NULL
test_data$Name <- NULL
test_data$Date <- NULL
test_data$ComName <- NULL
test_data$Other <- NULL
test_data$OtherProgram <- NULL
test_data$OtherSpec <- NULL
test_data$birdsamphibs <- NULL
test_data$X <- NULL
test_data$X.1 <- NULL
test_data$X.2 <- NULL
test_data$Initiatives.of.Interest <- NULL
test_data$KindofChange <- NULL
test_data$Climate <- NULL
test_data$ChangeinClimate <- NULL
test_data$Quality.of.Water <- NULL
test_data$Profession <- NULL

test_data$ComNumber <- as.factor(test_data$ComNumber)
test_data$Gender <- as.factor(test_data$Gender)
test_data$Education <- factor(test_data$Education, ordered = TRUE, levels = c(1, 2, 3, 4, 5))
test_data$Residency <- as.factor(test_data$Residency)

#test_data$ComType <- as.factor(test_data$ComType)
test_data$PotableWater <- factor(test_data$PotableWater)
test_data$Piped.Water <- factor(test_data$Piped.Water)
test_data$WaterQuebadra <- factor(test_data$WaterQuebadra)
test_data$Sewer <- factor(test_data$Sewer)
test_data$Telephone <- factor(test_data$Telephone)
test_data$Internet <- factor(test_data$Internet)
test_data$PavedStreets <- factor(test_data$PavedStreets)
test_data$GarbageCollection <- factor(test_data$GarbageCollection)
test_data$Internet <- factor(test_data$Internet)
test_data$Electricity <- factor(test_data$Electricity)
test_data$ServiceLevel <- factor(test_data$ServiceLevel, ordered = TRUE, levels=c(1, 2))
test_data$Basic.Services <- factor(test_data$Basic.Services, ordered = TRUE, levels=c(3, 2, 1))
test_data$ImpWaterQuality <- factor(test_data$ImpWaterQuality, ordered = TRUE, levels = c(1, 2, 3))
test_data$ImpElectricity <- factor(test_data$ImpElectricity, ordered = TRUE, levels = c(1, 2, 3))
test_data$ImpRoads <- factor(test_data$ImpRoads, ordered = TRUE, levels = c(1, 2, 3))
test_data$ImpTrash <- factor(test_data$ImpTrash, ordered = TRUE, levels = c(1, 2, 3))
test_data$ImpPublicSanitation <- factor(test_data$ImpPublicSanitation, ordered = TRUE, levels = c(1, 2, 3))
test_data$ImpPublicTransport <- factor(test_data$ImpPublicTransport, ordered = TRUE, levels = c(1, 2, 3))
test_data$ImpPublicSchool <- factor(test_data$ImpPublicSchool,ordered = TRUE, levels = c(1, 2, 3))
test_data$ImpAirQuality <- factor(test_data$ImpAirQuality, ordered = TRUE, levels = c(1, 2, 3))
test_data$ImpEnvironment <- factor(test_data$ImpEnvironment, ordered = TRUE, levels = c(1, 2, 3))
test_data$ProAgriChem <- factor(test_data$ProAgriChem, ordered = TRUE, levels = c(0,3,2,1))
test_data$ProTrash <- factor(test_data$ProTrash, ordered = TRUE, levels = c(0,3,2,1))
test_data$ProWaterContam <- factor(test_data$ProWaterContam, ordered = TRUE, levels = c(0, 3, 2, 1))
test_data$ProAirContan <- factor(test_data$ProAirContan, ordered = TRUE, levels = c(0, 3, 2, 1))
test_data$Mosquitos <- factor(test_data$Mosquitos, ordered = TRUE, levels = c(0, 3, 2, 1))
test_data$Traffic <- factor(test_data$Traffic, ordered = TRUE, levels = c(0, 3, 2, 1))
test_data$Dust <- factor(test_data$Dust, ordered = TRUE, levels = c(0, 3, 2, 1))
test_data$Noise <- factor(test_data$Noise, ordered = TRUE, levels = c(0, 3, 2, 1))
test_data$Watewater <- factor(test_data$Watewater, ordered = TRUE, levels = c(0, 3, 2, 1))
test_data$WaterQuality <- factor(test_data$WaterQuality, ordered = TRUE, levels = c(1, 2, 3, 4, 5))
test_data$DrinkCook <- factor(test_data$DrinkCook)
test_data$Irrigation <- factor(test_data$Irrigation)
test_data$CattleTroughs <- factor(test_data$CattleTroughs)
test_data$PersonalHygene <- factor(test_data$PersonalHygene)
test_data$WashClothes <- factor(test_data$WashClothes)
test_data$Recreation <- factor(test_data$Recreation)
test_data$RiverSports <- factor(test_data$RiverSports)
test_data$PureAir <- factor(test_data$PureAir)
test_data$NaturalRelaxxation <- factor(test_data$NaturalRelaxxation)
test_data$Ninguno <- factor(test_data$Ninguno)
test_data$WaterServices <- factor(test_data$WaterServices, ordered = TRUE, levels = c(0, 3, 2, 1))
test_data$AgriculturalUse <- factor(test_data$AgriculturalUse, ordered = TRUE, levels = c(0, 3, 2, 1))
test_data$LiveStock <- factor(test_data$LiveStock, ordered = TRUE, levels = c(0, 3, 2, 1))
test_data$Debris <- factor(test_data$Debris, ordered = TRUE, levels = c(0, 3, 2, 1))
test_data$Mining <- factor(test_data$Mining, ordered = TRUE, levels = c(0, 3, 2, 1))
test_data$Deforestation <- factor(test_data$Deforestation, ordered = TRUE, levels = c(0, 3, 2, 1))
test_data$ApplyLaws <- factor(test_data$ApplyLaws, ordered= TRUE, levels = c(0, 3, 2, 1))
test_data$Fines <- factor(test_data$Fines, ordered= TRUE, levels = c(0, 3, 2, 1))
test_data$Incentives <- factor(test_data$Incentives, ordered= TRUE, levels = c(0, 3, 2, 1))
test_data$EducationPrograms <- factor(test_data$EducationPrograms, ordered= TRUE, levels = c(0, 3, 2, 1))
test_data$Age <- factor(test_data$Age, ordered = TRUE, levels = c(1, 2, 3, 4))
test_data$UsageAmount <- factor(test_data$UsageAmount, ordered = TRUE, levels = c(0, 1, 2, 3, 4, 5, 6, 7))

test_data <- na.omit(test_data) #Crucial for dealing with factors 


#new approach glmm lasso models
#build one with random effects due to location (utmx, utmy)
#build a second with random effects due to community number (ask chloe for more info)
#Use unsupervised learning to ascertain results, compare to intuitive approach -->more rigorous?



##Need to remove totalstreamquality, write a function to clean up code

lambdas <- seq(0, 200, 5)
lambda_opt <- matrix(data = NA, nrow = 40, ncol = 2)
for (i in lambdas){
  nonconflcit_glmm <- glmmLasso(hilo~1 + ComType + PotableWater + Piped.Water + WaterQuebadra + Sewer + Telephone + Internet + PavedStreets + GarbageCollection + Internet + 
                                  PavedStreets + GarbageCollection + Electricity + Basic.Services + ServiceLevel + ImpWaterQuality + 
                                  ImpElectricity + ImpRoads + ImpTrash + ImpPublicSanitation + ImpPublicTransport + ImpPublicSchool +
                                  ImpEnvironment + ProAgriChem + ProTrash + ProWaterContam + ProAirContan + Mosquitos + Traffic + Dust + Noise + Watewater + 
                                  WaterQuality + DrinkCook + Irrigation + CattleTroughs +
                                  Recreation + RiverSports + PureAir + NaturalRelaxxation + Ninguno + WaterServices + AgriculturalUse + LiveStock + Debris + Mining + Deforestation +
                                  ApplyLaws + Fines + Incentives + EducationPrograms + Gender + Age + Education + Residency + UsageAmount +UTMY, 
                                rnd = list(ComNumber=~1),
                                lambda=i,
                                data=test_data, 
                                family=binomial(link="logit"))
  lambda_opt[i/5,1] <- i
  lambda_opt[i/5,2] <- nonconflcit_glmm$aic
  
}

##Using AIC optimize lambda

###Remove factors which are part of TotalStreamQUal, optimized lambda = 50
nonconflcit_glmm <- glmmLasso(hilo~ComType+ PotableWater + Piped.Water + WaterQuebadra + Sewer + Telephone + Internet + PavedStreets + GarbageCollection + Internet + 
                             PavedStreets + GarbageCollection + Electricity + Basic.Services + ServiceLevel + ImpWaterQuality + 
                             ImpElectricity + ImpRoads + ImpTrash + ImpPublicSanitation + ImpPublicTransport + ImpPublicSchool +
                             ImpEnvironment + ProAgriChem + ProTrash + ProWaterContam + ProAirContan + Mosquitos + Traffic + Dust + Noise + Watewater + 
                             WaterQuality + DrinkCook + Irrigation + CattleTroughs +
                             Recreation + RiverSports + PureAir + NaturalRelaxxation + Ninguno + WaterServices + AgriculturalUse + LiveStock + Debris + Mining + Deforestation +
                             ApplyLaws + Fines + Incentives + EducationPrograms + Gender + Age + Education + Residency + UsageAmount + UTMY, 
                           rnd = list(ComNumber=~1),
                           lambda=40,
                           data=test_data, 
                           family=binomial(link="logit")) ##add community type as factor


##Similarity in k-means, elastic net within lasso function, use latitude --- ask Decio for further questions

##Model a better fit when including utmy
##order importance of variables
coeffs <- as.data.frame(as.matrix(nonconflcit_glmm$coefficients))
coeffs$ABS_VAL <- abs(coeffs$V1)
sorted_coeffs <- coeffs[order(-coeffs$ABS_VAL),] #55 variables from survey

write.csv(sorted_coeffs, "factor_coeffs.csv")


##what about sociochemical model? Do we need to worry about collinearity between UTMs and chemical data??

###Exploring coefficients
UsageAmounts <- c(0, 1, 2, 3, 4, 5, 6, 7)
UA_Per_prob <- c(0.824, .95)

utms <- seq(9550028, 9565061, 1)
curve(0.999952846*x, 9550028, 9565061)

odds <- vector()
for (i in 1:length(utms)){
  odds[i] <- exp(i*-4.72E-05)/(1+ exp(i*-4.72E-05))

}

#Create Plots exploring important variable distributions/


curve(exp(0.03717*x^3 - 0.06947*x^2 + .4276*x)/(1+exp(0.03717*x^3 - 0.06947*x^2 + .4276*x)),  0, 3, xaxt="n", ylim = c(0, 1), ylab = 'Probability of percieving above average water quality', xlab = 'Degree wastewater is considered a main river contaminant', main = "Behavior of Wastewater Concern")
xtick<-seq(0, 3, by=1)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = c("None", "Low", "Medium", "High"), pos = 1, xpd = TRUE, offset = 0.7)
