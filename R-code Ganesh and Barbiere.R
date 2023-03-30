install.packages("sf")
library("ggplot2")
library("sf")
library(dplyr)


#######2019########
data19 <- Reports_final_2019_Ganesh_Barbiere
names(data19)
attach(data19)
data19 <- na.omit(data19)
modelStart19 <- lm(`HappinessScore` ~ 1, data=data19)
modelMax19 <-lm(`HappinessScore` ~ .-`Country name` -`Region`, data=data19)

##Step-up##
step(modelStart19, direction="forward", scope=formula(modelMax19), test="F")

detach(data19)


#######2021########
data21 <- Reports_final_2021_Ganesh_Barbiere
names(data21)
attach(data21)
data21 <- na.omit(data21)
modelStart21 <- lm(`HappinessScore` ~ 1, data=data21)
modelMax21 <-lm(`HappinessScore` ~ .-`Country name` -`Region`, data=data21)

##step-down##
step(modelMax21, direction="backward", test="F")

detach(data21)


###BIC###
data19 <- Reports_final_2019_Ganesh_Barbiere
data21 <- Reports_final_2021_Ganesh_Barbiere
linear_model_generator <- function(x){
  attribute_list = list()
  if (x[1] == 1){
    attribute_list <- append(attribute_list, "LoggedGDPperCapita")
  }
  if (x[2] == 1){
    attribute_list <- append(attribute_list, "FreedomToMakeLifeChoices")
  }
  if (x[3] == 1){
    attribute_list <- append(attribute_list, "Generosity")
  }
  if (x[4] == 1){
    attribute_list <- append(attribute_list, "PerceptionsOfCorruption")
  }
  if (x[5] == 1){
    attribute_list <- append(attribute_list, "LifeExpectancyTimesSocialSupport")
  }
  if (x[6] == 1){
    attribute_list <- append(attribute_list, "HealthyLifeExpectancy")
  }
  if (x[7] == 1){
    attribute_list <- append(attribute_list, "SocialSupport")
  }
  return(attribute_list)
}

n <- 7
l <- rep(list(0:1), n)

df = expand.grid(l)

##BIC2019##

empty_2019 <- lm(`HappinessScore` ~ 1, data=data19)

BIC_list_2019 <- list()

BIC_list_2019 <- list(1:128)
BIC_list_2019[1] <- BIC(empty_2019)

for (j in 2:128){
  frmla <- as.formula(paste("HappinessScore", paste(linear_model_generator(df[j,]), sep = "", 
                                                    collapse = " + "), sep = " ~ "))
  my_model = lm(formula = frmla, data = data19)
  BIC_list_2019[j] = BIC(my_model)
}

v = unlist(BIC_list_2019)
index = which(v == min(v))
frmla19 <- as.formula(paste("HappinessScore", paste(linear_model_generator(df[index,]), sep = "", 
                                                    collapse = " + "), sep = " ~ "))

optimal_BICmodel_2019 = lm(formula = frmla19, data = data19)

summary(optimal_BICmodel_2019)


##BIC2021##

empty_2021 <- lm(`HappinessScore` ~ 1, data=data21)

BIC_list_2021 <- list()

BIC_list_2021 <- list(1:128)
BIC_list_2021[1] <- BIC(empty_2021)

for (j in 2:128){
  frmla <- as.formula(paste("HappinessScore", paste(linear_model_generator(df[j,]), sep = "", 
                                                    collapse = " + "), sep = " ~ "))
  my_model = lm(formula = frmla, data = data21)
  BIC_list_2021[j] = BIC(my_model)
}

w = unlist(BIC_list_2021)
index = which(w == min(w))
frmla21 <- as.formula(paste("HappinessScore", paste(linear_model_generator(df[index,]), sep = "", 
                                                    collapse = " + "), sep = " ~ "))

optimal_BICmodel_2021 = lm(formula = frmla21, data = data21)
summary(optimal_BICmodel_2021)


####Regression Final####
Multi_Regression_2019 = lm(HappinessScore ~ FreedomToMakeLifeChoices + PerceptionsOfCorruption + LifeExpectancyTimesSocialSupport 
                           ,data = data19)
summary(Multi_Regression_2019)

Multi_Regression_2021 = lm(HappinessScore ~ LoggedGDPperCapita + SocialSupport + 
                             FreedomToMakeLifeChoices + HealthyLifeExpectancy + LifeExpectancyTimesSocialSupport, data = data21)
summary(Multi_Regression_2021)



####Regression Diagnostic####
optimal_BICmodel_2019_trans = lm(HappinessScore ~ LifeExpectancyTimesSocialSupport + 
                                   FreedomToMakeLifeChoices + PerceptionsOfCorruption,data = data19)
optimal_BICmodel_2021_trans = lm(HappinessScore ~ LoggedGDPperCapita + SocialSupport + 
                                   FreedomToMakeLifeChoices + HealthyLifeExpectancy + LifeExpectancyTimesSocialSupport, data = data21)

plot(fitted(optimal_BICmodel_2019_trans), residuals(optimal_BICmodel_2019_trans), main = "Diagnostic plot for 2019", xlab = "Fitted values", ylab = "Residuals")
abline(h=0)
text(6.7, -1.8, "Mean: 7.330583e-17")
plot(fitted(optimal_BICmodel_2021_trans), residuals(optimal_BICmodel_2021_trans), main = "Diagnostic plot for 2021", xlab = "Fitted values", ylab = "Residuals")
abline(h=0)
text(6.85, -1.7, "Mean: -7.951795e-17")


##Test##
shapiro.test(residuals(optimal_BICmodel_2019_trans))
shapiro.test(residuals(optimal_BICmodel_2021_trans))

##Qq plot##
qqnorm(residuals(optimal_BICmodel_2019_trans), main = "QQ-Plot for 2019")
qqline(residuals(optimal_BICmodel_2019_trans))

qqnorm(residuals(optimal_BICmodel_2021_trans), main = "QQ-Plot for 2021")
qqline(residuals(optimal_BICmodel_2021_trans))


##########ANOVA#########
d19 <- select(Reports_final_2019, `Country name`, "year", "HappinessScore", "Region")
d21 <- select(Reports_final_2021, `Country name`, "year", "HappinessScore", "Region")
datatot1 <- rbind(d19, d21)
anova1=aov(HappinessScore ~ factor(year)*factor(Region), data = datatot1)
summary(anova1)