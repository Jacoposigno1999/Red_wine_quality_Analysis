library(corrplot)
library(dplyr)
library(car)
library(MASS)
library(nnet)
library(boot)
library(effects)
library(ResourceSelection)
library(ROCit)
library(ggplot2)
library(caret)
library(faraway)
library(leaps)
library(ggcorrplot)

data = read.csv("winequality-red.csv")
#abbiamo visto che per ora togliere fixed.acidity è la cosa migliore 

summary(data)
str(data)

#create a new column total sulfur - free sulfur = boundsulfur, then remove the total sulfur column
data$bound.sulfur.dioxide = data$total.sulfur.dioxide - data$free.sulfur.dioxide
data = subset(data, select = -total.sulfur.dioxide)
cor(data)
data = data[,c(1,2,3,4,5,10,7,8,9,6,12,11)] #sto solo riordinando le colonne in maniera più sensata

#we see strange max value in residual.sugar, chlorides, free.sulfur.dioxide, total.sulfur.dioxide, sulphates 

# Plotting sorted data
for (i in colnames(data)){
  name = colnames(data[i])
  x11();par(mfrow=c(2,2))
  hist(data[,i], main = name, xlab = " ", ylab = " ")
  plot(density(data[,i]), xlab = " ", ylab = " ", main = name)
  plot(sort(data[,i]), pch=16, xlab = " ", ylab = " ", main=name)
  boxplot(data[,i], main = name, xlab = " ", ylab = " ")}
par(mfrow=c(1,1))

#boxplot
par(mfrow = c(1,1))
for (i in colnames(data)){
  name = colnames(data[i])
  boxplot(data[,i] ~ data$quality, main = paste0("Effect of ", name, " over quality"), ylab = colnames(data[i]), xlab = "quality", col = c('red','orange'))
}

#Correlation matrix
df = data
df$quality = as.numeric(df$quality)
correlation_matrix <- cor(df[,-11]) # Compute the correlation matrix cor(X,Y) = cov(X,Y) / (sd(X)*sd(Y))
ggcorrplot(correlation_matrix, hc.order = TRUE, type = "lower",
           outline.col = "white",
           colors = c("red4", "white", "springgreen4"),
           lab = TRUE)

density_cor = lm(density ~ ., data) #guardiamo quanto le altre variabili sono in grado di spegare density R2 = 0.84
summary(density_cor)

#trasforming the 6 categories in 3
data$quality = factor(data$quality,c(3, 4, 5, 6, 7, 8),c("scadente", "scadente", "scadente", "mediocre",
                                                         "ottimo", "ottimo"))
str(data)
Tab_prog = table(data$quality)
my_col = hcl.colors(n = 3, palette = "viridis")
x11();barplot(Tab_prog, main = "Quality of wine", col = my_col) # all groups are well represented

#train and test
data_tot = data
set.seed(99)
train_tot <- sample (nrow(data), nrow(data)*0.7) 
test_tot <- data[-train_tot,]
train_tot = data[train_tot,]
table(train_tot$quality)
table(test_tot$quality)

#a questo punto ci serve un modello (file models)

#VIF --> ha senso farlo solo su modelli lineari 
linear <- lm(quality ~.,data)
vif = vif(linear)
plot(vif,xaxt="n",cex=2, pch =16, xlab ="predictor", main = "VIF WITH ALL VARIABLES")
axis(1, at=1:11, labels=colnames(data)[1:11],srt=45,cex=0.5)
axis(1,at=7,labels="total.sulfur.dioxide",pos=1)
abline( h = 2.5, col = 'grey', lty = 2, lwd = 2)
abline( h = 5,   col = 'blue', lty = 2, lwd = 2)
abline( h = 10,  col = 'red', lty = 2, lwd = 2)
legend(x = "topright", legend = c("2.5", "5"), lty = c(2,2), lwd = c(2,2), col = c('grey','blue') )
#abbiamo problemi con fixed acidity e density, se togliamo fixed risolviamo.

#Vif senza fixed.acidity
linear <- lm(quality ~. - fixed.acidity ,data); summary(linear)
vif = vif(linear)
plot(vif, xaxt="n",cex=2, pch = 16, ylim = c(1,8), xlab ="predictor", main = "VIF WITHOUT FIXED.ACIDITY")
axis(1, at=1:10, labels=colnames(data)[1:10],srt=45,cex=0.5)

abline( h = 2.5, col = 'grey', lty = 2, lwd = 2)
abline( h = 5,   col = 'blue', lty = 2, lwd = 2)
abline( h = 10,  col = 'red', lty = 2, lwd = 2)
legend(x = "topright", legend = c("2.5", "5"), lty = c(2,2), lwd = c(2,2), col = c('grey','blue') )


fixed_r2 = lm(fixed.acidity ~., data); summary(fixed_r2)
data = subset(data, select = -fixed.acidity)

set.seed(420)
train <- sample (nrow(data), nrow(data)*0.7) 
test <- data[-train,]
train = data[train,]
table(train$quality)
table(test$quality)


############################################ GGPLOT2 #######################################################

data %>%
  ggplot(aes(density, fixed.acidity, colour = quality)) +
  geom_point(aes(size = alcohol, alpha = 0.5)) +
  labs(title = "Titolo") + theme_bw()


data %>%
  ggplot(aes(density, alcohol, colour = quality)) +
  geom_point(size = 4) +
  labs(title = "Titolo") + theme_bw()
