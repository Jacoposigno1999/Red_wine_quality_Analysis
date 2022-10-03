data <- read.csv("winequality-red.csv")

r = dim(data)[2]

for(i in 1:r){
  name = names(data)[i]
  x11();par(mfrow=c(2,2))
  hist(data[,i], main = name, xlab = " ", ylab = " ")
  plot(density(data[,i], na.rm = T), xlab = " ", ylab = " ", main = name)
  plot(sort(data[,i]), pch=16, xlab = " ", ylab = " ", main=name)
  boxplot(data[,i], main = name, xlab = " ", ylab = " ")
} 

pairs(data, pch = 16)
data$bound <- data$total.sulfur.dioxide-data$free.sulfur.dioxide 
for(i in 1:r){
  x11();name = names(data)[i]
  boxplot(data[,i]~data$quality, main = name, xlab = " ", ylab = " ")
} 

data2 <- data[,-c(1,7)]
data3 <- data[,-c(1,7)]

library(corrplot)
corrplot(correlation_matrix, method = 'ellipse') 
correlation_matrix <- cor(data)
library(corrplot)
correlation_matrix3 <- cor(data3)
corrplot(correlation_matrix3, method = 'ellipse') 

cor(data$volatile.acidity,data$citric.acid)

cor(data$total.sulfur.dioxide,data$free.sulfur.dioxide) 
cor(data$bound,data$free.sulfur.dioxide) 
cor(data$total.sulfur.dioxide,data$bound) 


cutalc <- cut(data$alcohol,7)
ic <- c(8,9,10,11,12,13,15)
matplot(ic,prop.table(table(cutalc,data2$quality),1),lty=c(1,2,5),
        type="l",ylab="Proportion",xlab="alcohol")
legend('topright',legend = c("Bad" ,"Medium","Good"),lty=c(1,2,5),col=c(1,2,3))


library(manipulate)
manipulate(prplot(linot,i),i=slider(1,10)) 

corfix <- lm(fixed.acidity~citric.acid,data)
summary(corfix)
plot(corfix)
plot(fixed.acidity~citric.acid, data)
corvolcit<- lm(fixed.acidity~citric.acid+volatile.acidity, data)
summary(corvolcit)
shapiro.test(data$quality) 


fix <- lm(fixed.acidity~citric.acid+density+pH,data)
summary(fix)

dens <- lm(density~citric.acid+fixed.acidity+pH,data)
summary(dens)

ph <- lm(pH~citric.acid+fixed.acidity+density,data)
summary(ph)

fixt <- lm(fixed.acidity~.,data)
summary(fixt)

denst <- lm(density~.,data)
summary(denst)

pht <- lm(pH~.,data)
summary(pht)

fixd <- lm(fixed.acidity~.-density-pH,data)
summary(fixd)

data2$quality <- as.factor(data2$quality)
levels(data2$quality) <- c("bad","bad","bad","medium","good","good")
polf <- polr(quality~.,data2)
summary(polf)

my_col = hcl.colors(n = 3, palette = "viridis")
Tab_prog = table(data2$quality)
Tab_prog = table(train$quality)
barplot(Tab_prog, main = "Quality of wine", col = my_col)

linot <- lm(quality~.,data3)
vifinot <- vif(linot)
plot(vifinot, pch = 16, ylab = "Vif values", main = "Variance Inflation plot")
text(x = vifinot, labels = names(vifinot))
abline( h = 2.5, col = 'grey', lty = 2, lwd = 2)
abline( h = 5,   col = 'blue', lty = 2, lwd = 2)
abline( h = 10,  col = 'red', lty = 2, lwd = 2)
legend(x = "topright", legend = c("2.5", "5", "10"), lty = c(2,2,2), lwd = c(2,2,2), col = c('grey','blue','red') ) 

lino <- lm(quality~.,data)
vifino <- vif(lino)
x11();plot(vifino, pch = 16, ylab = "Vif values", main = "Variance Inflation plot")
text(x = vifino, labels = names(vifino))
abline( h = 2.5, col = 'grey', lty = 2, lwd = 2)
abline( h = 5,   col = 'blue', lty = 2, lwd = 2)
abline( h = 10,  col = 'red', lty = 2, lwd = 2)
legend(x = "topright", legend = c("2.5", "5", "10"), lty = c(2,2,2), lwd = c(2,2,2), col = c('grey','blue','red') ) 


par(mfrow=c(1,3))
lino2 <- lm(quality~.-fixed.acidity,data)
vifino2 <- vif(lino2)
plot(vifino2, pch = 16, ylab = "Vif values", main = "Variance Inflation plot")
text(x = vifino2, labels = names(vifino2))
abline( h = 2.5, col = 'grey', lty = 2, lwd = 2)
abline( h = 5,   col = 'blue', lty = 2, lwd = 2)
abline( h = 10,  col = 'red', lty = 2, lwd = 2)
legend(x = "topright", legend = c("2.5", "5", "10"), lty = c(2,2,2), lwd = c(2,2,2), col = c('grey','blue','red') )

lino3 <- lm(quality~.-density,data)
vifino3 <- vif(lino3)
plot(vifino3, pch = 16, ylab = "Vif values", main = "Variance Inflation plot")
text(x = vifino3, labels = names(vifino3))
abline( h = 2.5, col = 'grey', lty = 2, lwd = 2)
abline( h = 5,   col = 'blue', lty = 2, lwd = 2)
abline( h = 10,  col = 'red', lty = 2, lwd = 2)
legend(x = "topright", legend = c("2.5", "5", "10"), lty = c(2,2,2), lwd = c(2,2,2), col = c('grey','blue','red') )

lino4 <- lm(quality~.-pH,data)
vifino4 <- vif(lino4)
plot(vifino4, pch = 16, ylab = "Vif values", main = "Variance Inflation plot")
text(x = vifino4, labels = names(vifino4))
abline( h = 2.5, col = 'grey', lty = 2, lwd = 2)
abline( h = 5,   col = 'blue', lty = 2, lwd = 2)
abline( h = 10,  col = 'red', lty = 2, lwd = 2)
legend(x = "topright", legend = c("2.5", "5", "10"), lty = c(2,2,2), lwd = c(2,2,2), col = c('grey','blue','red') ) 

boxcox(lino,plotit = T,lambda=seq(0.5,1.5,by=0.1)) 

boxcox(linot,plotit = T,lambda=seq(0.5,1.5,by=0.1))

ct <- regsubsets(quality~.,data2)
dt <- summary(ct)
dt$which

data2 <- data2[,c(1:9,11,10)]
#train and test
set.seed(420)
train <- sample (1599, 1599*0.7) 
test <- data2[-train,]
train <- data2[train,]
library(MASS)
mod<- polr(quality~.,data2)
summary(mod)
names(data2)
variables = c("volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","density"
              ,"pH","sulphates","alcohol","bound")
variable_tests = data.frame("var" = variables, pval = rep(0,length(variables))) #exclude id variable
for(i in 1:length(variables)){
  name = variables[i]
  formula = as.formula( paste("quality ~ ", paste0(".-", name)  ) )
  mod_reduced = polr(formula, data =data2)
  variable_tests$pval[i] = anova(mod_reduced, mod)[2,7]
}
variable_tests #remove density
round(variable_tests$pval, digits = 3)

mod2<- polr(quality~.-density,data2)
variables = c("volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide"
              ,"pH","sulphates","alcohol","bound")
variable_tests = data.frame("var" = variables, pval = rep(0,length(variables))) #exclude id variable
for(i in 1:length(variables)){
  name = variables[i]
  formula = as.formula( paste("quality ~ ", paste0(".-density-", name)  ) )
  mod_reduced = polr(formula, data =data2)
  variable_tests$pval[i] = anova(mod_reduced, mod2)[2,7]
}
variable_tests #remove free
round(variable_tests$pval, digits = 3)

mod3<- polr(quality~.-density-free.sulfur.dioxide,data2)
variables = c("volatile.acidity","citric.acid","residual.sugar","chlorides"
              ,"pH","sulphates","alcohol","bound")
variable_tests = data.frame("var" = variables, pval = rep(0,length(variables))) #exclude id variable
for(i in 1:length(variables)){
  name = variables[i]
  formula = as.formula( paste("quality ~ ", paste0(".-density-free.sulfur.dioxide-", name)  ) )
  mod_reduced = polr(formula, data =data2)
  variable_tests$pval[i] = anova(mod_reduced, mod3)[2,7]
}
variable_tests 
round(variable_tests$pval, digits = 3)

mod4<- polr(quality~.-density-free.sulfur.dioxide-citric.acid,data2)
variables = c("volatile.acidity","residual.sugar","chlorides"
              ,"pH","sulphates","alcohol","bound")
variable_tests = data.frame("var" = variables, pval = rep(0,length(variables))) 
for(i in 1:length(variables)){
  name = variables[i]
  formula = as.formula( paste("quality ~ ", paste0(".-density-free.sulfur.dioxide-citric.acid-", name)  ) )
  mod_reduced = polr(formula, data =data2)
  variable_tests$pval[i] = anova(mod_reduced, mod4)[2,7]
}
variable_tests 
round(variable_tests$pval, digits = 3)

mod5<- polr(quality~.-density-free.sulfur.dioxide-citric.acid-residual.sugar,data2)
variables = c("volatile.acidity","chlorides"
              ,"pH","sulphates","alcohol","bound")
variable_tests = data.frame("var" = variables, pval = rep(0,length(variables))) 
for(i in 1:length(variables)){
  name = variables[i]
  formula = as.formula( paste("quality ~ ", paste0(".-density-free.sulfur.dioxide-citric.acid-residual.sugar-", name)  ) )
  mod_reduced = polr(formula, data =data2)
  variable_tests$pval[i] = anova(mod_reduced, mod5)[2,7]
}
variable_tests 
round(variable_tests$pval, digits = 3) 

library(MASS)
polt <- polr(quality~.,data2)
summary(polt)
(ctable <- coef(summary(polt)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(polt))
exp(cbind(OR = coef(polt), ci))
polti <- step(polt) 
anova(polt,polti)
(poltu <- drop1(polt, test="Chisq"))
poltt <- polr(quality~volatile.acidity+chlorides+pH+sulphates+alcohol+bound,data2) 
anova(polti,poltt) 
drop1(poltt,test="Chisq") 



(R2_N=(1-exp((poltt$deviance-poll_null$deviance)/1599))/(1-exp((-poll_null$deviance)/1599))) 


poltr <- polr(quality~.,train)
drop1(poltr,test="Chisq")
poltr2 <- polr(quality~.-residual.sugar,train)
drop1(poltr2,test="Chisq")
poltr3 <- polr(quality~.-residual.sugar-citric.acid,train)
drop1(poltr3,test="Chisq")
poltr4 <- polr(quality~.-residual.sugar-citric.acid-density,train)
drop1(poltr4,test="Chisq")
poltr5 <- polr(quality~.-residual.sugar-citric.acid-density-free.sulfur.dioxide,train)
drop1(poltr5,test="Chisq") 

poltp <- polr(quality~.,train,method="probit")
drop1(poltp,test="Chisq")
poltp2 <- polr(quality~.-residual.sugar,train,method="probit")
drop1(poltp2,test="Chisq")
poltp3 <- polr(quality~.-residual.sugar-citric.acid,train,method="probit")
drop1(poltp3,test="Chisq")
poltp4 <- polr(quality~.-residual.sugar-citric.acid-density,train,method="probit")
drop1(poltp4,test="Chisq")
poltp5 <- polr(quality~.-residual.sugar-citric.acid-density-free.sulfur.dioxide,train,method="probit")
drop1(poltp5,test="Chisq") 

poltc <- polr(quality~.,train,method="cloglog")
drop1(poltc,test="Chisq")
poltc2 <- polr(quality~.-free.sulfur.dioxide,train,method="cloglog")
drop1(poltc2,test="Chisq")
poltc3 <- polr(quality~.-free.sulfur.dioxide-citric.acid,train,method="cloglog")
drop1(poltc3,test="Chisq")
poltc4 <- polr(quality~.-free.sulfur.dioxide-citric.acid-density,train,method="cloglog")
drop1(poltc4,test="Chisq")
poltc5 <- polr(quality~.-residual.sugar-citric.acid-density-free.sulfur.dioxide,train,method="cloglog")
drop1(poltc5,test="Chisq") 

library(caret)
train(quality~.,data2, "polr") 
train(quality~volatile.acidity+chlorides+pH+sulphates+alcohol+bound,data2, "polr") 

train(quality~volatile.acidity+chlorides+pH+sulphates+alcohol+bound,train, "polr") 

pred1 <- predict(polr(quality ~ volatile.acidity+chlorides+pH+sulphates+alcohol+bound , 
                      data = train ), 
                 newdata=test)
tab <- table(test$quality,pred1)
round((sum(diag(tab))/sum(tab))*100,2)

pred2 <- predict(polr(quality ~ volatile.acidity+chlorides+pH+sulphates+alcohol+bound , 
                      data = train, method="probit" ), 
                 newdata=test)
table(test$quality,pred2)

pred3 <- predict(polr(quality ~ volatile.acidity+chlorides+pH+sulphates+alcohol+bound , 
                      data = train, method="cloglog" ), 
                 newdata=test)
table(test$quality,pred3) 






poll_null <- polr(quality~1,train)
poll_null2 <- polr(quality~1,train, method="probit")
poll_null3 <- polr(quality~1,train, method="cloglog")

(R2_Nc=(1-exp((poltc5$deviance-poll_null$deviance)/1119))/(1-exp((-poll_null$deviance)/1119)))
(R2_Np=(1-exp((poltp5$deviance-poll_null2$deviance)/1119))/(1-exp((-poll_null2$deviance)/1119)))
(R2_Nr=(1-exp((poltr5$deviance-poll_null3$deviance)/1119))/(1-exp((-poll_null3$deviance)/1119))) 

library(generalhoslem)
lipsitz.test(poltt) 
logitgof(data2$quality,fitted(poltt)) 



library(effects)
chlor_eff <- effect("chlorides", poltt)
pH_eff <- effect("pH", poltt)
sulphates_eff <- effect("sulphates", poltt)
volatile.acidity_eff <- effect("volatile.acidity", poltt)
alcohol_eff <- effect("alcohol", poltt)
bound_eff <- effect("bound",poltt)
plot(chlor_eff)
plot(pH_eff)
plot(sulphates_eff) 
plot(volatile.acidity_eff) 
plot(alcohol_eff)
plot(bound_eff)

poll_null <- polr(quality~1,data2)
anova(poll_null,poltt) 

ilogit(poltt$zeta)
diff(ilogit(poltt$zeta))
(ctable <- coef(summary(poltt)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(poltt))
exp(cbind(OR = coef(poltt), ci))
library(Hmisc)
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}
(s <- with(data, summary(as.numeric(quality) ~ chlorides+pH+sulphates+volatile.acidity+alcohol+bound, fun=sf)))
s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]
s
plot(s, which=1:3, pch=1:3, xlab='logit', main=' ') 
pum <- prop.table(table(cut(data2$alcohol,3),data2$quality),1)
logit(pum[,1])-logit(pum[,1]+pum[,2])
library(car)
poTest(poltt)

nuovo <- data2[dim(data2)[1],]
poltt_ult <- polr(quality ~ volatile.acidity + chlorides + pH + sulphates + alcohol + bound,data=data2[-dim(data2)[1],])
(predicted_mean_poltt_ult= predict(poltt_ult, newdata = nuovo, type = "probs"))
pr=predict(poltt,data.frame(nalc=ic,row.names=ic),type="probs")
matplot(ic,pred1,type="l",ylim=c(0,1))
legend('topright',legend = c('Democrat','Idependent','Republican'),lty=c(1,2,3),col=c(1,2,3))


x <- seq(-30,30,by=0.05)
plot(x,dlogis(x),type="l")
abline(v=c(5.173,8.053)+8.5*0.90718)
abline(v=c(5.173,8.053)+10.5*0.90718,lty=2,col=2)
abline(v=c(5.173,8.053)+12.5*0.90718,lty=5,col=3)
legend('topright',legend = c("8.5", "10.5","12.5"),col=c(1,2,3),lty=c(1,2,5)) 




x_grid = seq(9, 15, length.out = 1000)
p_grid = predict(poltt, data.frame (volatile.acidity=0.31,citric.acid=0.47,residual.sugar=3.6,chlorides=0.067,free.sulfur.dioxide=18,
                                    density=0.99549,pH=3.39,sulphates=0.66,alcohol = x_grid,bound=24), type = "probs")

matplot(x = x_grid, y = p_grid, type = 'l', col = my_col, lty = 1, lwd = 2,
        main = "Estimated probabilities ", ylim = c(0,1),xlim=c(8,16))
legend("topright", legend = c("bad", "medium", "good"), lty = 1, col = my_col, lwd = 2)