# Dataset described at archive.ics.uci.edu/ml/datasets/Wine+Quality

x <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep=";")
head(x)
dim(x)

set.seed(1)
test.which <- sample(1:nrow(x), floor(nrow(x)/10))
test <- x[test.which, ]
dim(test)
train <- x[-test.which, ]
dim(train)

# *****************************************************************************
# PART 1

# take a look at boxplots without any transformations
par(mfrow = c(3,4), oma = c(0,0,2,0))
for (i in 1:11) {
  append = ""
  if (i == 1) {
    append = "g(tartaric acid)/dm^3"
  }
  if (i == 2) {
    append = "g(acetic acid)/dm^3"
  }  
  if (i == 3 | i == 4) {
    append = "g/dm^3"
  }  
  if (i == 5) {
    append = "g(sodium chloride)/dm^3"
  }  
  if (i == 6 | i == 7) {
    append = "mg/dm^3"
  }  
  if (i == 8) {
    append = "g/cm^3"
  }  
  if (i == 10) {
    append = "g(potassium sulphate)/dm^3"
  }  
  if (i == 11) {
    append = "vol.%"
  }  
  boxplot(train[ ,i], ylab = paste(as.character(colnames(train)[i]), append))
}
mtext("Boxplots of 11 Variables Before Transformation", outer = TRUE, cex = 1.5)


# --> Most variables display some positive skewness



# Go through each variable, comparing boxplots/histograms for the original data
# and transformations sensibly applied to right-skewed data.  For each, 
# select transformation that outputs the most normally distributed data.
# Transformational approaches based off of: 
# https://www.statisticssolutions.com/transforming-data-for-normality/

# (A lot of techniques are based on gaussian data.. the more gaussian your data is
# the more legitimate the techniques are for your data.)

# 1 
par(mfrow = c(2,3))
boxplot((train[ ,1]))
boxplot(sqrt(train[ ,1]))
boxplot(log(train[ ,1])) # wins
hist((train[ ,1]), breaks = 20)
hist(sqrt(train[ ,1]), breaks = 20)
hist(log(train[ ,1]), breaks = 20) # wins

# 2
par(mfrow = c(2,3))
boxplot((train[ ,2]))
boxplot(sqrt(train[ ,2]))
boxplot(log(train[ ,2])) # wins
hist((train[ ,2]), breaks = 20)
hist(sqrt(train[ ,2]), breaks = 20)
hist(log(train[ ,2]), breaks = 20) # wins

# 3
par(mfrow = c(2,3))
boxplot((train[ ,3]))
boxplot(sqrt(train[ ,3])) # wins
boxplot(log(train[ ,3]))
hist((train[ ,3]), breaks = 20)
hist(sqrt(train[ ,3]), breaks = 20) # wins
hist((log(train[ ,3])), breaks = 20)

# 4
par(mfrow = c(2,4))
boxplot((train[ ,4]))
boxplot(sqrt(train[ ,4]))
boxplot((log(train[ ,4])))
boxplot(log(log(train[ ,4]))) # wins
hist((train[ ,4]), breaks = 20)
hist(sqrt(train[ ,4]), breaks = 20)
hist((log(train[ ,4])), breaks = 20)
hist(log(log(train[ ,4])), breaks = 20) # wins

# 5
par(mfrow = c(2,3))
boxplot((train[ ,5]))
boxplot(sqrt(train[ ,5]))
boxplot(log(train[ ,5])) # wins
hist((train[ ,5]), breaks = 20)
hist(sqrt(train[ ,5]), breaks = 20)
hist(log(train[ ,5]), breaks = 20) # wins

# 6
par(mfrow = c(2,3))
boxplot((train[ ,6]))
boxplot(sqrt(train[ ,6]))
boxplot(log(train[ ,6])) # wins
hist((train[ ,6]), breaks = 20)
hist(sqrt(train[ ,6]), breaks = 20)
hist(log(train[ ,6]), breaks = 20) # wins

# 7
par(mfrow = c(2,4))
boxplot((train[ ,7]))
boxplot(sqrt(train[ ,7]))
boxplot(log(train[ ,7])) # wins
boxplot(log(1 + (log(train[ ,7])))/(1 - (log(train[ ,7])))/2)
hist((train[ ,7]), breaks = 20)
hist(sqrt(train[ ,7]), breaks = 20)
hist(log(train[ ,7]), breaks = 20) # wins
hist(log(1 + (log(train[ ,7])))/(1 - (log(train[ ,7])))/2, breaks = 20)

# 8
par(mfrow = c(2,3))
boxplot((train[ ,8])) # wins
boxplot(sqrt(train[ ,8]))
boxplot(log(train[ ,8]))
hist((train[ ,8]), breaks = 20) # wins
hist(sqrt(train[ ,8]), breaks = 20)
hist(log(train[ ,8]), breaks = 20)

# 9
par(mfrow = c(2,3))
boxplot((train[ ,9]))
boxplot(sqrt(train[ ,9]))
boxplot(log(train[ ,9])) # wins
hist((train[ ,9]), breaks = 20)
hist(sqrt(train[ ,9]), breaks = 20)
hist(log(train[ ,9]), breaks = 20) # wins

# 10
par(mfrow = c(2,3))
boxplot((train[ ,10]))
boxplot(sqrt(train[ ,10]))
boxplot(log(train[ ,10])) # wins
hist((train[ ,10]), breaks = 20)
hist(sqrt(train[ ,10]), breaks = 20)
hist(log(train[ ,10]), breaks = 20) # wins

# 11
par(mfrow = c(2,4))
boxplot((train[ ,11]))
boxplot(sqrt(train[ ,11]))
boxplot(log(train[ ,11])) # wins
boxplot(log(log(train[ ,11])))
hist((train[ ,11]), breaks = 20)
hist(sqrt(train[ ,11]), breaks = 20)
hist(log(train[ ,11]), breaks = 20) # wins
hist(log(log(train[ ,11])), breaks = 20)


# Revisit boxplots with transformations applied
par(mfrow = c(3,4), oma = c(0,0,2,0))
boxplot(log(train[ ,1]), ylab = paste(as.character(colnames(train)[1]), "log g(tartaric acid)/dm^3"))
boxplot(log(train[ ,2]), ylab = paste(as.character(colnames(train)[2]), "log g(acetic acid)/dm^3")) 
boxplot(sqrt(train[ ,3]), ylab = paste(as.character(colnames(train)[3]), "sqrt g/dm^3"))
boxplot(log(log(train[ ,4])), ylab = paste(as.character(colnames(train)[4]), "log log g/dm^3"))
boxplot(log(train[ ,5]), ylab = paste(as.character(colnames(train)[5]), "log g(sodium chloride)/dm^3"))
boxplot(log(train[ ,6]), ylab = paste(as.character(colnames(train)[6]), "log mg/dm^3"))
boxplot(log(train[ ,7]), ylab = paste(as.character(colnames(train)[7]), "log mg/dm^3"))
boxplot((train[ ,8]), ylab = paste(as.character(colnames(train)[8]), "g/cm^3"))
boxplot(log(train[ ,9]), ylab = paste('log', as.character(colnames(train)[9])))
boxplot(log(train[ ,10]), ylab = paste(as.character(colnames(train)[5]), "log g(potassium sulphate)/dm^3"))
boxplot(log(train[ ,11]), ylab = paste(as.character(colnames(train)[5]), "log vol.%"))
mtext("Boxplots of 11 Variables After Transformation", outer = TRUE, cex = 1.5)

# Create a mew dataframe that includes any transformed variables in place of the original ones
y = train
y[ ,1] = log(y[ ,1])
y[ ,2] = log(y[ ,2])
y[ ,3] = sqrt(y[ ,3])
y[ ,4] = log(log(y[ ,4]))
y[ ,5] = log(y[ ,5])
y[ ,6] = log(y[ ,6])
y[ ,7] = log(y[ ,7])
y[ ,9] = log(y[ ,9])
y[ ,10] = log(y[ ,10])
y[ ,11] = log(y[ ,11])


#which(is.na(y[ ,4]) %in% TRUE)
#y[ ,4][1018]


# Verify new dataframe
head(train)
head(y)

# Create a vector of variable names from dataset
y_names = colnames(y)
names



# *****************************************************************************
# PART 2
# install.packages("olsrr")
library(olsrr)


# MALLOWS CP
# Use Mallow's Cp with forward step-wise selection to select a model.
# install.packages("olsrr")

# Determine first variable to include in the model based on Mallow's Cp
full_model <- lm(quality ~ ., data = y)
cp = c()
for (i in 1:(dim(y)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,i])
  mallow <- ols_mallows_cp(model, full_model)
  cp <- c(cp, mallow)
}
cp
min(cp)
all_selected <- c()
index <- which(cp == min(cp))
index
all_selected <- c(all_selected,index)
all_selected
selected = y_names[index]
selected
# Mallow's Cp statistic nearest to the number of predictors in the model is for y[ ,11]: alcohol (285.24)
# Alcohol will be first predictor added to model based off selection criterion.
y_new <- y[ ,!y_names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine second variable to include in the model based on Mallow's Cp
cp = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,i])
  mallow <- ols_mallows_cp(model, full_model)
  cp <- c(cp, mallow)
}
cp
min(cp)
index <- which(cp == min(cp))
index
selected = names[index]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
# Mallow's Cp statistic nearest to the number of predictors in the model is for  y[ ,2]: volatile acidity (113.32)
# Volatile acidity will be second predictor added to model based off selection criterion.
y_new <- y_new[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine third variable to include in the model based on Mallow's Cp
cp = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,i])
  mallow <- ols_mallows_cp(model, full_model)
  cp <- c(cp, mallow)
}
min(cp)
cp
index <- which(cp == min(cp))
index
selected = names[index]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
# Mallow's Cp statistic nearest to the number of predictors in the model is for  y[ ,1]: fixed acidity (102.42)
# Fixed acidity will be third predictor added to model based off selection criterion.
y_new <- y_new[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine fourth variable to include in the model based on Mallow's Cp
cp = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,1] + y[ ,i])
  mallow <- ols_mallows_cp(model, full_model)
  cp <- c(cp, mallow)
}
min(cp)
cp
index <- which(cp == min(cp))
index
selected = names[index]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
# Mallow's Cp statistic nearest to the number of predictors in the model is for  y[ ,9]: pH (101.08)
# pH will be fourth predictor added to model based off selection criterion.
y_new <- y_new[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine fifth variable to include in the model based on Mallow's Cp
cp = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,1] + y[ ,9] + y[ ,i])
  mallow <- ols_mallows_cp(model, full_model)
  cp <- c(cp, mallow)
}
min(cp)
cp
index <- which(cp == min(cp))
index
selected = names[index]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
# Mallow's Cp statistic nearest to the number of predictors in the model is for  y[ ,10]: sulphates (100.12)
# Sulphates will be fifth predictor added to model based off selection criterion.
y_new <- y_new[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine sixth variable to include in the model based on Mallow's Cp
cp = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,10] + y[ ,9] + y[ ,10] + y[ ,i])
  mallow <- ols_mallows_cp(model, full_model)
  cp <- c(cp, mallow)
}
min(cp)
cp
index <- which(cp == min(cp))
index
selected = names[index]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
# Mallow's Cp statistic nearest to the number of predictors in the model is for  y[ ,7]: total sulfer dioxide (25.83)
# Total sulfer dioxide will be sixth predictor added to model based off selection criterion.
y_new <- y_new[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine seventh variable to include in the model based on Mallow's Cp
cp = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,10] + y[ ,9] + y[ ,10] + y[ ,7] + y[ ,i])
  mallow <- ols_mallows_cp(model, full_model)
  cp <- c(cp, mallow)
}
min(cp)
cp
index <- which(cp == min(cp))
index
selected = names[index]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
# Mallow's Cp statistic nearest to the number of predictors in the model is for  y[ ,8]: density (17.58)
# Density will be seventh predictor added to model based off selection criterion.
y_new <- y_new[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine eighth variable to include in the model based on Mallow's Cp
cp = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,10] + y[ ,9] + y[ ,10] + y[ ,7] +y[ ,8] + y[ ,i])
  mallow <- ols_mallows_cp(model, full_model)
  cp <- c(cp, mallow)
}
min(cp)
# Minimum Mallow's Cp statistic is less favorable than in the prior round.
# No additional variables will be included in the model based of selection criterion.

# Model based on Mallow's Cp
model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,10] + y[ ,9] + y[ ,10] + y[ ,7] +y[ ,8])
summary(model)



# AIC
# Use AIC with forward step-wise selection to select a model.

# Determine first variable to include in the model based on AIC
aic = c()
for (i in 1:(dim(y)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,i])
  stat <- ols_aic(model)
  aic <- c(aic, stat)
}
aic
min(aic)
index <- which(aic == min(aic))
index
selected = y_names[index]
selected
all_selected = c()
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
# Minimum AIC statistic for  y[ ,11]: alcohol (3108.98)
# Alcohol will be first predictor added to model based off selection criterion.
y_new <- y[ ,!y_names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine second variable to include in the model based on AIC
aic = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,i])
  stat <- ols_aic(model)
  aic <- c(aic, stat)
}
aic
min(aic)
index <- which(aic == min(aic))
index
selected = names[index]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
# Minimum AIC statistic for  y[ ,2]: volatile acidity (2957.58)
# Volatile acidity will be second predictor added to model based off selection criterion.
y_new <- y_new[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine third variable to include in the model based on AIC
aic = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,i])
  stat <- ols_aic(model)
  aic <- c(aic, stat)
}
aic
min(aic)
index <- which(aic == min(aic))
index
selected = names[index]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
# Minimum AIC statistic for  y[ ,1]: fixed acidity (2947.53)
# Fixed acidity will be third predictor added to model based off selection criterion.
y_new <- y_new[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine fourth variable to include in the model based on AIC
aic = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,1] + y[ ,i])
  stat <- ols_aic(model)
  aic <- c(aic, stat)
}
aic
min(aic)
index <- which(aic == min(aic))
index
selected = names[index]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
# Minimum AIC statistic for  y[ ,6]: free sulphur dioxide (2943.53)
# Free sulphur dioxide will be fourth predictor added to model based off selection criterion.
y_new <- y_new[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine fifth variable to include in the model based on AIC
aic = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,1] + y[ ,6] + y[ ,i])
  stat <- ols_aic(model)
  aic <- c(aic, stat)
}
aic
min(aic)
index <- which(aic == min(aic))
index
selected = names[index]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
# Minimum AIC statistic for  y[ ,10]: sulphates (2940.26)
# Sulphates will be fifth predictor added to model based off selection criterion.
y_new <- y_new[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine sixth variable to include in the model based on AIC
aic = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,1] + y[ ,6] + y[ ,10] + y[ ,i])
  stat <- ols_aic(model)
  aic <- c(aic, stat)
}
aic
min(aic)
index <- which(aic == min(aic))
index
selected = names[index]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
# Minimum AIC statistic for  y[ ,8]: density (2879.40)
# Density will be sixth predictor added to model based off selection criterion.
y_new <- y_new[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine seventh variable to include in the model based on AIC
aic = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,1] + y[ ,6] + y[ ,10] + y[ ,8] + y[ ,i])
  stat <- ols_aic(model)
  aic <- c(aic, stat)
}
aic
min(aic)
index <- which(aic == min(aic))
index
selected = names[index]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
# Minimum AIC statistic for  y[ ,9]: pH (2875.65)
# pH will be seventh predictor added to model based off selection criterion.
y_new <- y_new[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine eighth variable to include in the model based on AIC
aic = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,1] + y[ ,6] + y[ ,10] + y[ ,8] + y[ ,i])
  stat <- ols_aic(model)
  aic <- c(aic, stat)
}
aic
min(aic)
# Minimum AIC statistic is less favorable than in the prior round.
# No additional variables will be included in the model based of selection criterion.

# Model based on AIC
model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,1] + y[ ,6] + y[ ,10] + y[ ,8])
summary(model)



# BIC
# Use BIC with forward step-wise selection to select a model.

# Determine first variable to include in the model based on BIC
bic = c()
for (i in 1:(dim(y)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,i])
  stat <- ols_sbic(model, full_model)
  bic <- c(bic, stat)
}
bic
min(bic)
index <- which(bic == min(bic))
index
selected = y_names[index]
selected
all_selected = c()
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
# Minimum BIC statistic for  y[ ,11]: alcohol (-978.27)
# Alcohol will be first predictor added to model based off selection criterion.
y_new <- y[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine second variable to include in the model based on BIC
bic = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,i])
  stat <- ols_sbic(model, full_model)
  bic <- c(bic, stat)
}
bic
min(bic)
index <- which(bic == min(bic))
index
selected = y_names[index]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
# Minimum BIC statistic for  y[ ,2]: volatile acidity (-1129.39)
# Volatile acidity will be first predictor added to model based off selection criterion.
y_new <- y_new[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine third variable to include in the model based on BIC
bic = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,i])
  stat <- ols_sbic(model, full_model)
  bic <- c(bic, stat)
}
bic
min(bic)
index <- which(bic == min(bic))
index
selected = y_names[index]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
# Minimum BIC statistic for  y[ ,10]: sulphates (-1190.84)
# Sulphates will be third predictor added to model based off selection criterion.
y_new <- y_new[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine fourth variable to include in the model based on BIC
bic = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,10] + y[ ,i])
  stat <- ols_sbic(model, full_model)
  bic <- c(bic, stat)
}
bic
min(bic)
index <- which(bic == min(bic))
index
selected = y_names[index]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
# Minimum BIC statistic for  y[ ,5]: chlorides (-1201.82)
# Chlorides will be fourth predictor added to model based off selection criterion.
y_new <- y_new[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine fifth variable to include in the model based on BIC
bic = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,10] + y[ ,5] + y[ ,i])
  stat <- ols_sbic(model, full_model)
  bic <- c(bic, stat)
}
bic
min(bic)
index <- which(bic == min(bic))
index
selected = y_names[index]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
# Minimum BIC statistic for  y[ ,7]: total sulfur dioxide (-1210.56)
# Total sulfur dioxide will be fifth predictor added to model based off selection criterion.
y_new <- y_new[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names



# Determine sixth variable to include in the model based on BIC
bic = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,10] + y[ ,5] + y[ ,7] + y[ ,i])
  stat <- ols_sbic(model, full_model)
  bic <- c(bic, stat)
}
bic
min(bic)
index <- which(bic == min(bic))
index
selected = y_names[index]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
# Minimum BIC statistic for  y[ ,1]: fixed acidity (-1215.05)
# Fixed acidity will be sixth predictor added to model based off selection criterion.
y_new <- y_new[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine seventh variable to include in the model based on BIC
bic = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,10] + y[ ,5] + y[ ,7] + y[ ,1] + y[ ,i])
  stat <- ols_sbic(model, full_model)
  bic <- c(bic, stat)
}
bic
min(bic)
index <- which(bic == min(bic))
index
selected = y_names[index]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
# Minimum BIC statistic for  y[ ,6]: free sulfur dioxide (-1219.99)
# Free sulfur dioxide will be seventh predictor added to model based off selection criterion.
y_new <- y_new[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine eighth variable to include in the model based on BIC
bic = c()
for (i in 1:(dim(y_new)[2]-1)) {
  model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,10] + y[ ,5] + y[ ,7] + y[ ,1] + y[ ,6] + y[ ,i])
  stat <- ols_sbic(model, full_model)
  bic <- c(bic, stat)
}
bic
min(bic)
# Minimum BIC statistic is equal to that of the prior round.
# No additional variables will be included in the model based of selection criterion.

# Model based on BIC
model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,10] + y[ ,5] + y[ ,7] + y[ ,1] + y[ ,6])
summary(model)


# *****************************************************************************
# PART 3
# Perform iterative fitting by looking at residual plots.

# Determine first variable to include by looking at scatterplots of variables against the response
all_selected = c()
par(mfrow = c(3,4), oma = c(0,0,2,0))
for (i in 1:11) {
  plot(y$quality, y[ ,i])
}
# Plot 11 (alcohol) seems to be the strongest predictor of quality based on scatterplots. (Trend most apparent)
# Trend appears to be linear
selected <- y_names[11]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
y_new <- y[ ,!y_names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine second variable to include by looking at scatterplots of variables against the residual 
# (after including alcohol in the model)
par(mfrow = c(3,4), oma = c(0,0,2,0))
model <- lm(y$quality ~ y[ ,11])
for (i in 1:10) {
 plot(model$residuals, y_new[ ,i]) 
}
# Plot 2 (volatile acidity) seems to be the strongest predictor of quality based on scatterplots.
# Trend appears to be linear
selected <- names[2]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
y_new <- y_new[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine third variable to include by looking at scatterplots of variables against the residual 
# (after including selected variables in the model)
par(mfrow = c(3,4), oma = c(0,0,2,0))
model <- lm(y$quality ~ y[ ,11] + y[ ,2])
for (i in 1:9) {
  plot(model$residuals, y_new[ ,i]) 
}
# Plot 9 (sulphates) seems to be the strongest predictor of quality based on scatterplots.
# Trend appears to be linear
selected <- names[9]
selected
all_selected <- c(all_selected, which(y_names %in% selected))
all_selected
y_new <- y_new[ ,!names %in% selected]
dim(y_new)
names = colnames(y_new)
names


# Determine fourth variable to include by looking at scatterplots of variables against the residual 
# (after including selected variables in the model)
# This time, plot each residual plot within a "null lineup".  Give myself 20 seconds to see if any variable
# stands out against the null field.
# install.packages("BBmisc")
library("BBmisc")

par(mfrow = c(3,3), oma = c(0,0,2,0))
model <- lm(y$quality ~ y[ ,11] + y[ ,2] + y[ ,10])
for (i in 1:8) {
  answer <- sample(1:9,1)
  for (j in 1:9) {
    if (j==answer) {
      var <- normalize(y_new[ ,i], method = "standardize")
      plot(model$residuals, var)
    }
    else {
      fakes <- rnorm(length(y_new[ ,1]))
      var <- normalize(fakes, method = "standardize")
      plot(model$residuals,var)
    }
  }
  Sys.sleep(10)
  print(answer)
  Sys.sleep(10)
}
selected <- names[7]
selected
# No variables seem to stand out against the null lineup.
# Variable selection accomplished.


# *****************************************************************************
# PART 6
# Use Ridge regression to find a fit for quality with the (transformed) explanatory variables.
# install.packages("glmnet")
library(glmnet)

# First standardize transformed variables so that they don't depend on scale
r = y
for (i in 1:11) {
  var <- normalize(y[ ,i], method = "standardize")
  r[ ,i] <- var
}
head(r)
head(r[ , 1:11])


# (Double log transformation created two NaN values, which must be dropped before ridge regression)
which(is.na(y[ ,4]) %in% TRUE)
y[ ,4][915]
r <- rbind(r[1:914, ], r[917:1440, ])
dim(r)

x <- as.matrix(r[,1:11])
y1 <- as.matrix(r[,12])


# Run ridge regression
CV.ridge <- cv.glmnet(x, y1, alpha=0)
par(mfrow = c(1,1))
plot(CV.ridge) # CV test error vs log lambda
abline(v=log(CV.ridge$lambda.min), col=3, lty=2)
abline(v=log(CV.ridge$lambda.1se), col=4, lty=2)
CV.ridge

ridge <- coef(CV.ridge, s=CV.ridge$lambda.1se)
ridge


# Run lasso regression
CV.lasso <- cv.glmnet(x, y1, alpha=1)
plot(CV.lasso) # CV test error vs lambda
plot(CV.lasso$glmnet.fit, xvar="lambda", label=TRUE) # optimal coefficients vs lambda

lasso <- coef(CV.lasso, s=CV.lasso$lambda.1se)
lasso



# *****************************************************************************
# PART 4
# Perform the following simulation 100,000 times.  Permute the response (wine quality) values
# at random in order to break any relationship with the explanatory variables, and record the 11
# coefficients' significance probabilities when all 11 of the explanatory variables are included
# in a multiple linear model.  For each variable, calculate the proportion of simulation trials
# in which its significance probability was less than .05.  Do the proportions seem the be close to .05?

# Create an empty dataframe to store significance probabilities
column= rep(NA, dim(y)[1])
significance_probs <- data.frame(column, column, column, column, column, column, column, column, column, column, column)
dim(significance_probs)
colnames(significance_probs) <- colnames(y)[1:11]
colnames(significance_probs)

z <- y[1:11]
head(z)

# Run 100,000 iterations
for (i in 1:100000) {
  # progress indicator
  if(i%%1000==0) {
    print(paste("----", i))
  }

  # set random permutation
  random_quality <- sample(train$quality, size = length(train$quality), replace = FALSE)
  y$quality <- random_quality

  # Regress the permuted quality variable against all 11 supplied predictors
  model <- lm(y$quality ~ ., data = z)
  significance_probs[i,] <- summary(model)$coefficients[ ,4][2:12]
}

head(significance_probs)


# For each variable, calculate the proportion of simulation trials in which its significance probability
# was less than .05
install.packages("expss")
library("expss")

proportions <- c()
for (i in 1:11) {
  count <- count_if(lt(.05), significance_probs[ ,i])
  proportion <- count/100000
  proportions <- c(proportions, proportion)
}

proportions <- data.frame(proportions,  row.names = colnames(significance_probs))
proportions


# *****************************************************************************
# PART 5
# Calculate the proportion of the simulation trials in which at least one significance probability
# was smaller than the Bonferroni threshold of (.05/11).

# Create a vector of threshold values
thresholds <- c()
threshold <- 0
i <- 1
while (threshold <= .05) {
  threshold <- (.05/11)*i
  thresholds <- c(thresholds,threshold)
  i <- i+1
}
thresholds


# Iterate through the thresholds, calculating the proportion of total observations that have at least one
# variable which are significant according to that threshold.
false_positives_by_threshold <- data.frame(thresholds, proportion_false_pos = NA)
false_positives_by_threshold
for (i in 1:(length(thresholds))) {
  total <- 0
  for (j in 1:100000){
    # progress indicator
    if(j%%1000==0) {
      print(paste("----", j))
    }
    
    count <- count_row_if(lt(thresholds[i]), significance_probs[j,])
    if (count > 0) {
      total = total + 1
    }
  }
  proportion <- total/100000
  false_positives_by_threshold[i,2] <- proportion
}
false_positives_by_threshold

# Draw a scatterplot showing these thresholds as x values and the estimated total false positive rates as y values.
par(mfrow = c(1,1))
plot(false_positives_by_threshold, xlab = "Thresholds", ylab = "False Positive Rate", type = "b", main = "False Positive Rate by Threshold")


# Return to the actual values and fit the multiple linear model that includes 
# only the variables whose original significance probabilities were below the threshold you just proposed.
# Bonferroni threshold of .05/11 () gives a total false positive rate of .042
true_model <- lm(y$quality ~ y$fixed.acidity, data = y)
summary(true_model)
true_model <- lm(y$quality ~ y$volatile.acidity, data = y) # IN
summary(true_model)
true_model <- lm(y$quality ~ y$citric.acid, data = y)
summary(true_model)
true_model <- lm(y$quality ~ y$residual.sugar, data = y)
summary(true_model)
true_model <- lm(y$quality ~ y$chlorides, data = y)
summary(true_model)
true_model <- lm(y$quality ~ y$free.sulfur.dioxide, data = y)
summary(true_model)
true_model <- lm(y$quality ~ y$total.sulfur.dioxide, data = y)
summary(true_model)
true_model <- lm(y$quality ~ y$density, data = y) # IN
summary(true_model)
true_model <- lm(y$quality ~ y$pH, data = y)
summary(true_model)
true_model <- lm(y$quality ~ y$sulphates, data = y) # IN
summary(true_model)
true_model <- lm(y$quality ~ y$alcohol, data = y) # IN
summary(true_model)


true_model <- lm(y$quality ~ y$volatile.acidity + y$density + y$sulphates + y$alcohol)
summary(true_model)





# *****************************************************************************
# PART 7
# Apply fits to predict the test data

head(test)

# Tranform variables
y = test
y[ ,1] = log(y[ ,1])
y[ ,2] = log(y[ ,2])
y[ ,3] = sqrt(y[ ,3])
y[ ,4] = log(log(y[ ,4]))
y[ ,5] = log(y[ ,5])
y[ ,6] = log(y[ ,6])
y[ ,7] = log(y[ ,7])
y[ ,9] = log(y[ ,9])
y[ ,10] = log(y[ ,10])
y[ ,11] = log(y[ ,11])

# Standardize transformed variables for use in Ridge and LASSO
r = y
for (i in 1:11) {
  var <- normalize(y[ ,i], method = "standardize")
  r[ ,i] <- var
}
head(r)
head(r[ , 1:11])

x <- as.matrix(r[,1:11])
y1 <- as.matrix(r[,12])



# Sample mean of the response variable
mean(y[ ,12])
sum((y$quality - mean(y[ ,12]))^2)

# Full least-squares regression with untransformed variables
test_var <- as.matrix(test[,1:11])
model <- lm(test[ ,12] ~ test_var)
summary(model)
sum((y$quality - fitted(model))^2)

# Full least-squares regression with untransformed variables
y_var <- as.matrix(y[,1:11])
model <- lm(y[ ,12] ~y_var)
summary(model)
sum((y$quality - fitted(model))^2)

# Least-squares fit with the transformed variables that were "significant" (end of Part 5 above)
model <- lm(y$quality ~ y$volatile.acidity + y$density + y$sulphates + y$alcohol)
summary(model)
sum((y$quality - fitted(model))^2)

# Least-squares fit with the transformed variables selected by Mallow's Cp
model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,10] + y[ ,9] + y[ ,10] + y[ ,7] +y[ ,8])
summary(model)
sum((y$quality - fitted(model))^2)

# Least-squares fit with the transformed variables selected by AIC
model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,1] + y[ ,6] + y[ ,10] + y[ ,8])
summary(model)
sum((y$quality - fitted(model))^2)

# Least-squares fit with the transformed variables selected by BIC
model <- lm(y[ ,12] ~ y[ ,11] + y[ ,2] + y[ ,10] + y[ ,5] + y[ ,7] + y[ ,1] + y[ ,6])
summary(model)
sum((y$quality - fitted(model))^2)

# Ridge regression fit with the transformed variables
CV.ridge <- cv.glmnet(x, y1, alpha=0)
ridge <- coef(CV.ridge, s=CV.ridge$lambda.1se)
ridge
fit <- CV.ridge$glmnet.fit
y_pred <- predict(fit, s=CV.ridge$lambda.1se, newx = x)
dim(y_pred)
sum((y_pred - y$quality)^2)

# LASSO regression fit with the transformed variables
CV.lasso <- cv.glmnet(x, y1, alpha=1)
lasso <- coef(CV.lasso, s=CV.lasso$lambda.1se)
lasso
fit <- CV.lasso$glmnet.fit
y_pred <- predict(fit, s=CV.lasso$lambda.1se, newx = x)
dim(y_pred)
sum((y_pred - y$quality)^2)

# Least-squares fit with the transformed variables selected by iterative fitting
model <- lm(y$quality ~ y[ ,11] + y[ ,2] + y[ ,10])
summary(model)
sum((y$quality - fitted(model))^2)


# *****************************************************************************
# PART 8
# Devise at least one more variation on the approaches listed above.

# Apply ridge regression using the variables and terms that came from the iterative fitting process
x <- as.matrix(data.frame(r[,11], r[,2], r[,10]))
y1 <- as.matrix(r[,12])
head(y1)

CV.ridge <- cv.glmnet(x, y1, alpha=0)
ridge <- coef(CV.ridge, s=CV.ridge$lambda.1se)
ridge
fit <- CV.ridge$glmnet.fit
y_pred <- predict(fit, s=CV.ridge$lambda.1se, newx = x)
dim(y_pred)
sum((y_pred - y$quality)^2)




