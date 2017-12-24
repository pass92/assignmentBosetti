# Experiment on Water Conductivity with different Factor (salt,sugar,bicarbonate) on 2 Level
# Date:     22 Dicember 2017
# Author:   Luca Passerini
# Location: Trento, Trentino-Alto Adige 
# Temperature: 22°

###### Factors #####
# Salt
# Sugar
# Bicarbonate
###### Levels ######
# Salt: -1 = 0g ; 1 = 10g
# Sugar: -1 = 0g ; 1 = 10g
# Bicarbonate: -1 = 0g ; 1 = 10g
##### Solvent #######
# 150g of tap water
#
###### Instrument ######
  #Amperometer#
# range: 0.001 -> 10A
# resolution: 0.001A
  #Balance#
# range: 0g -> 3000g
# resolution: 1g
  #Power Supply AC#
# voltage: 12V
# max current: 4A
# frequency: 50HZ
# distance between electrode = 5cm


factors <- list(A=c(-1,+1), B=c(-1,+1), C=c(-1,+1), Rep=1:3)
data_from_exp <- expand.grid(factors)

#add a column to see the initial order of experiment
Order <- c(1:24)
data_from_exp <- cbind(data_from_exp,Order)

# add a random column to execute the experiment in random Way
RunOrder <- sample(1:24, 24, replace=F)

# add column RunOrder to data frame data
data_from_exp <- cbind(data_from_exp,RunOrder)

# runif(24, 0, 1) devo usare questo opure va bene pure il sample function per randomizzare gli esperiemtni

# Order to execute the experiment
data_from_exp <- data_from_exp[with(data_from_exp, order(RunOrder)), ]

# Add the Response column of the system
Response <- c(NA)
data_from_exp <- cbind(data_from_exp,Response)

# Write table to collect all data from pratical experiment
write.table(data_from_exp,file = "/Users/lucapasserini/Library/Mobile Documents/com~apple~CloudDocs/Università/Magistrale/Ingegneria/Second year/First term/Design and control of product and process/Report/R_experiment/data_from_exp.txt", sep = "\t", row.names=F)

########################            RUN PHASE             ##############################

data_1 <-read.table("/Users/lucapasserini/Library/Mobile Documents/com~apple~CloudDocs/Università/Magistrale/Ingegneria/Second year/First term/Design and control of product and process/Report/R_experiment/data.dat")

# Save factors and response in variables
response = data_1[,7]  #tri different output and use impedenza instead current
salt = as.factor(data_1[,1])
sugar = as.factor(data_1[,2])
bicarbonate = as.factor(data_1[,3])

#Boxplots
boxplot(response~salt, data = data_1, xlab = "Salt", ylab = "Response") 
boxplot(response~sugar, data = data_1, xlab = "Sugar", ylab = "Response")
boxplot(response~bicarbonate, data = data_1, xlab = "Bicarbonate", ylab = "Response")

#Interaction betwwen factors
interaction.plot(salt,sugar,response) 
interaction.plot(salt,bicarbonate,response) 
interaction.plot(sugar,bicarbonate,response) 


####### Model 0 ######
data_1.lm<-lm(response~salt*sugar*bicarbonate, data=data_1)

#Anova
data_1.anova <- anova(data_1.lm)
data_1.anova

####### Model 1 ######
data_1.lm2    <- lm(response~salt*bicarbonate+sugar)
data_1.anova2 <- anova(data_1.lm2)
data_1.anova2  
#Run Order vs Residuals
plot(data_1.lm2$residuals, xlab = "Run Order", ylab = "Residuals")
#QQplots
qn <- qqnorm(data_1.lm2$residuals)
qqline(data_1.lm2$residuals)
#Residuals vs Fitted Value
plot(data_1.lm2$fitted.values, data_1.lm2$res, xlab="Fitted Values", ylab="Resisuals")
#Shapiro
shapiro.test(data_1.lm2$residuals) #72%
#Histogram of residuals
hist(data_1.lm2$residuals,xlab = "Residuals")

####### Model 2 ######
data_1.lm3    <- lm(sqrt(response)~salt*bicarbonate+sugar)
data_1.anova3 <- anova(data_1.lm3)
data_1.anova3
#Run Order vs Residuals
plot(data_1.lm3$residuals, xlab = "Run Order", ylab = "Residuals")
#QQplots
qn <- qqnorm(data_1.lm3$residuals)
qqline(data_1.lm3$residuals)
#Residuals vs Fitted Value
plot(data_1.lm3$fitted.values, data_1.lm3$res, xlab="Fitted Values", ylab="Resisuals")
#Shapiro
shapiro.test(data_1.lm3$residuals) # 0.85%
#Histogram of residuals
hist(data_1.lm3$residuals)

####### Model 3 ######
library(MASS)
bc <- boxcox(response~salt*bicarbonate+sugar,lambda = seq(0,1.2,1/10))
lambda <- bc$x[which.max(bc$y)]
lambda

finalfinal <- lm((response)^(lambda) ~ salt * bicarbonate + sugar)
#Anova
anova.finalfinal <- anova(finalfinal)
anova.finalfinal
#Run Order vs Residuals
plot(finalfinal$residuals, xlab = "Run Order", ylab = "Residuals")
#QQplots
qn <- qqnorm(finalfinal$residuals)
qqline(finalfinal$residuals)
#Residuals vs Fitted Value
plot(finalfinal$fitted.values, finalfinal$res, xlab="Fitted Values", ylab="Resisuals")
#Shapiro
shapiro.test(finalfinal$residuals)
#Histogram
hist(finalfinal$residuals, freq = T,xlab = "Residuals")


