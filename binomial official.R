# binomial model
db = read.csv("winequality-red.csv")


db$quality<-as.factor(db$quality)
levels(db$quality)<-c("poor","poor","poor","good", "good", "good")

db$good<-ifelse(db$quality=="good",1,0)
# create response variable which takes value 0 if quality is bad and 1 if it's good

db<-subset(db, select = -quality)
db$bound.sulfur.dioxide = db$total.sulfur.dioxide - db$free.sulfur.dioxide
db = subset(db, select = -total.sulfur.dioxide)
db = db[,c(1,2,3,4,5,10,7,8,9,6,12,11)]
# we order the columns of the dbset

db = subset(db, select = -fixed.acidity) #removing fixed

data = db #in data density non è moltiplicato per 100

#densiy *100
db$density = db$density *100

my_col = hcl.colors(n = 3, palette = "viridis")
Tab_prog = table(db$good)
x11();barplot(Tab_prog, main = "Quality of wine", col = my_col)
# both levels are well represented

# plot variables
# plot(db$fixed.acidity, db$pH, col=db$good+2)
# you can visualize any variable wrt another one
# adding the color allows to separate good and bad quality
# for example in this graph we see that fixed acidity and pH are correlated

############################################ MODELS ################################################
#binomial model
glmod=glm(good~.,db, family = binomial)
summary(glmod)
# hosmer-lemershow test
hoslem.test(data$good,predict(glmod,type="response"))
# we can't reject H0: model suits the db

# standard residual check
par(mfrow=c(2,2))
plot(glmod)

glmod_ph = glm(good ~ volatile.acidity +chlorides +alcohol+ bound.sulfur.dioxide +sulphates + pH, family = binomial, data )
glmod_PH=glm(good~. -pH ,db, family = binomial); summary(glmod_PH)
anova (glmod,backward_bic, test = "Chisq")


# observations number 653 and 1430 may be outliers (?)

#modello nullo
greve = glm(good ~ 1, db, family = binomial) #mi serve per fare i back and fordward
########################## AIC ##############################
stepmod<-step(glmod) # --> volatile.acidity, citric.acid, chlorides, alcohol, pH, sulphates, bound.sulfur.dioxide
summary(stepmod)
plot(stepmod)
anova(stepmod, glmod, test="Chisq")# p-value = 0.4941
# slightly different from the step function in the multinomial model

both_aic = step(object = greve , scope = formula(glmod), direction="both", k = 2, trace = 0)
both_aic$anova
both_aic$coefficients # --> uguale a quello prima
summary(both_aic) #aic
# TUTTI I MODELLI CON LE PROCEDURE AIC SONO UGUALI

hoslem.test(data$good,predict(stepmod,type="response")) #p-value = 0.2543





n = nrow(data)
#BIC 
forward_bic = step(object = greve, scope = formula(glmod), direction="forward", k = log(n), trace = 0)
forward_bic$anova
forward_bic$coefficients #--> #alcohol, volatile.acidity, bound.sulfur.dioxide, sulphates, chlorides 
summary(forward_bic) 

backward_bic = step(object = glmod, direction="backward", k = log(n), trace = 0)
backward_bic$anova
backward_bic$coefficients # uguale a quello sopra
summary(backward_bic) #bic

both_bic = step(object = glmod, scope = formula(greve), direction="both", k = log(n), trace = 0)
both_bic$anova
both_bic$coefficients
summary(both_bic)# --> uguale

hoslem.test(data$good,predict(forward_bic,type="response")) #p-value = 0.7571

anova(forward_bic, glmod, test="Chisq")# p-value  0.1487
anova(forward_bic, stepmod, test="Chisq")# p value 0.05665
# TUTTI I MODELLI CON LE PROCEDURE BIC SONO UGUALI

# hosmer-lemershow test
hoslem.test(data$good,predict(backward_bic,type="response"))

par(mfrow = c(2,2))
plot(backward_bic)
par(mfrow = c(1,1))


############################################# MODEL WITH DENSITY^2 ##########################
glmod2=glm(good~.+ I(density^2) ,db, family = binomial)
summary(glmod2)

stepmod2 = step(glmod2) #va a togliere density alla seconda
summary(stepmod2)

##### stud res
st = rstudent(both_bic)
head(sort(rstudent(both_bic), decreasing = TRUE))

halfnorm(rstudent(both_bic), nlab = 1)
abline(h = 4.17604 )

cr = qt(0.05/(2*1599),df=1599-6-1, lower.tail = F); cr
which(abs(st) > cr)   #nessun outlier !!!!!!!!!!!!!!!!!!!!! 

############################################# REMOVE 653 #####################################################################
summary(db$fixed.acidity)
which(rownames(db)==653) # observation in position 653 corresponds to 653
# 653 was a strange observation in the plot but we checked and it is not an outlier 
# 653 is the value with max fixed.acidity and alcohol

#QUI ANDIAMO A CONFRONTARE I LIVELLI DI SIGNIFICATIVITA' CON E SENZA 653 --> NON CAMBIA NIENTE
glmod_653<-glm(good~.,db[-653,], family = "binomial")
summary(glmod_653)# --> citric becomes significant 
summary(glmod)
plot(glmod_653)

pval_glmod<-summary(glmod)$coefficients[,4]
pval_glmod_653<-summary(glmod_653)$coefficients[,4]
cbind(pval_glmod, pval_glmod_653)
# we compare the p-values of the full model and the model without the outlier 653
cbind(pval_glmod, pval_glmod_653)>0.05
# there is no difference in the significance of the parameters

#BIC MODEL WITHOUT 653
backward_bic653 = step(object = glmod_653, direction="backward", k = log(n), trace = 0)
backward_bic653$anova
backward_bic653$coefficients #--> #alcohol, volatile.acidity, bound.sulfur.dioxide, sulphates, chlorides
summary(backward_bic653) #bic
#VEDIAMO CHE NON CAMBIA PRATICAMENTE NULLA, CONTINUIAMO A USARE 
####################################### FINE WITHOUT 653 #################################################################
#Cooks distance
head(sort(cooks.distance(glmod), decreasing = T)) #tell us about influential observations
head(sort(cooks.distance(backward_bic), decreasing = T))
hist(cooks.distance(backward_bic))


########### Come cambiano i beta by removing each observation ######

par(mfrow = c(1,1))
for(i in seq(1,6)){
  nome= colnames(dfbeta(backward_bic))[i]
  plot(dfbeta(backward_bic)[,i], ylab = nome)
  identify(dfbeta(backward_bic)[,i],labels =rownames(db) )
}
#VEDIAMO CHE L'OBS 755 CAMBIA ABBASTANZA IL LA STIMA DI CLHORIDES --> ABBIAMO FATTO IL MODELLO SENZA 755
#VEDIAMO CHE NON CAMBIA NULLA
# QUINDI A QUESTO PUNTO IL MODELLO MIGLIORE è ANCORA BIC_BACKWARD CON TUTTE LE OSSERVAZIONI

# using the deviance explained and R2_N
n=nrow(db)
R2_d=1-glmod$deviance/glmod$null.deviance; R2_d
R2_N=(1-exp((glmod$deviance-glmod$null.deviance)/n))/(1-exp((-glmod$null.deviance)/n));R2_N

R2_d_s=1-stepmod$deviance/stepmod$null.deviance; R2_d_s
R2_N_s=(1-exp((stepmod$deviance-stepmod$null.deviance)/n))/(1-exp((-glmod$null.deviance)/n));R2_N_s

R2_d_bic=1-backward_bic$deviance/backward_bic$null.deviance; R2_d_bic
R2_N_bic=(1-exp((backward_bic$deviance-backward_bic$null.deviance)/n))/(1-exp(-backward_bic$null.deviance)/n);R2_N_bic
# both R2_d and R2_N increase in the model without 653
# so without observation 653 we increase the explained deviance
# R2_d = 1 - (D_res/D_null)
# R2_N uses the likelihood 


#######################################  DROP1 #################################################
poltr <- glm(good~.,db,family=binomial)
drop1(poltr,test="Chisq")
poltr2 <- glm(good~.-residual.sugar,db,family=binomial)
drop1(poltr2,test="Chisq")
poltr3 <- glm(good~.-residual.sugar-free.sulfur.dioxide  ,db,family=binomial)
drop1(poltr3,test="Chisq")
poltr4 <- glm(good~.-residual.sugar-density-free.sulfur.dioxide ,db,family = binomial)
drop1(poltr4,test="Chisq")
poltr5 <- glm(good~.-residual.sugar-citric.acid-density-free.sulfur.dioxide,db,family=binomial)
drop1(poltr5,test="Chisq")
poltr6 <- glm(good~.-residual.sugar-pH-density-free.sulfur.dioxide-citric.acid,db,family=binomial)
drop1(poltr6,test="Chisq")
dropmod <- glm(good~.-residual.sugar-pH-density-free.sulfur.dioxide-citric.acid,db,family=binomial)
drop1(dropmod,test="Chisq")
summary(dropmod)# --> CI PORTA ALLO STESSO IDENTICO MODELLO DI BACKWARD BIC(QUINDI DI TUTTI I BIC)

anova(dropmod, stepmod, test="Chisq") #ORA IL MIGLIORE E' DROPMOD
# we fail to reject H1 so the drop model and step model are not significantly different
# therefore we choose the smaller model 

anova(dropmod_653, stepmod_653, test = "Chisq")
# p-value: 0.05422
# we fail to reject H0 so we prefer the drop model (without 653) --> 
# dropmod_653 (valatile.acidiy, chlorides, alcohol, sulphates, bound.sulfur.dioxide)
#     

#Levarages 
lev<-sort(hatvalues(backward_bic), decreasing = T)
x<-2*length(coef(backward_bic))/nrow(db)
which(lev > x)# many observations are higher than 2p/n
id = rownames(db)
par(mfrow = c(1,1))
halfnorm(hatvalues(backward_bic), labs = id, ylab= "Lavarages")
abline(h = 2*sum(hatvalues(backward_bic)/nrow(db))) 

################################ CROSS VALIDATION ####################################
mse_bic = cv.glm(db, backward_bic, K = 10)
mse_bic$delta[1]

mse_aic = cv.glm(db, stepmod, K = 10)
mse_aic$delta[1]

mse = cv.glm(db, glmod, K = 10)
mse$delta[1]

####################################### PREDICTION ########################################################
##### Full model 
predicted=predict(glmod,test,type = "response")
alpha=0.5
yhat=ifelse(predicted>=alpha,1,0)
tb=table(test$good,yhat); tb   
 
overall_acc=sum(diag(tb))/sum(tb);overall_acc #0.7520 

# our miscalculation rate is around 25%
specificity=tb[1,1]/(tb[1,1]+tb[1,2]); specificity #0.7743
specificity(tb)
# there is a 77% probability that bad wine is classified as bad
sensitivity=tb[2,2]/(tb[2,1]+tb[2,2]);sensitivity#0.7322
sensitivity(tb)
##bha, quelle calcolate in automatico sono strane

#Roc curve
rocitb<-rocit(predicted, test$good); plot(rocitb)
AUC_step = rocitb$AUC; AUC_step #0.839715

J=which.max(Specificity+Sensitivity-1)
thresh[J]
#see what change with j as treshold
yhat_y=ifelse(predicted>=thresh[J],1,0)
tab=table(test$good,yhat_y)
tab
Sensitivity[J]#0.6929134
Specificity[J]#0.800885
# Specificity increases while Sensitivity decreases

# prediction on dropmod_653
predicted_drop=predict(dropmod_653,test,type = "response")
alpha=0.5
yhat=ifelse(predicted_drop>=alpha,1,0)
tb=table(test$good,yhat); tb   

overall_acc=sum(diag(tb))/sum(tb);overall_acc #0.7479 
# our miscalculation rate is around 25%
specificity=tb[1,1]/(tb[1,1]+tb[1,2]); specificity #0.7743
# there is a 77% probability that bad wine is classified as bad
sensitivity=tb[2,2]/(tb[2,1]+tb[2,2]);sensitivity #0.7244


#Roc curve
rocitb<-rocit(predicted_drop, test$good); plot(rocitb)
AUC_step = rocitb$AUC; AUC_step#0.835987

# Youden's index
J=which.max(Specificity+Sensitivity-1)
thresh[J]#0.54 ------------------> VIENE LO STESSO CHE NEL MODELLO TOTALE, STRANO ASSAI 
#see what change with j as treshold
yhat_y=ifelse(predicted>=thresh[J],1,0)
tab=table(test$good,yhat_y)
tab
Sensitivity[J] #0.7047
Specificity[J] #0.800885 ##MI SEMBRANO SBALLATI STI VALORI
# specificity increases while sensitivity decreases

# DISPERSION PARAMETERS
r=residuals(glmod, type="pearson")
x=sum(r^2)
sigma2=x/glmod$df.residual; sigma2
# sigma2=1.15 : no significant overdispersion on total model


#### on stepmod_653
r2=residuals(dropmod_653, type="pearson")
x=sum(r2^2)
sigma2step=x/dropmod_653$df.residualsigma2step
# sigma2step=0.9887 : no significant underdispersion on step model
#QUESTA COSA è CONFERMATA DAL FATTO è I DF SONO SIMILI ALLA DEVIANCE --> CONTROLLARE ANCHE NEGLI ALTRI MODELLI,
#MA IN NESSUONO DOVREBBERO ESSERCI PROBLEMI


hoslem.test(train_653$good,predict(dropmod_653,type="response"))
# Still fail to reject H0 --> model suits




#
cook<-head(sort(cooks.distance(dropmod_653), decreasing = T))
# nulla di che

exp(coef(stepmod_653))
# it shows how much the odds change by increasing each variable by 1

with653<-glm(good~.-fixed.acidity -citric.acid -residual.sugar -density 
             -pH -free.sulfur.dioxide , data=train, family="binomial")


predicted1=predict(with653,test,type = "response")

yhat_y1=ifelse(predicted1>=thresh[J],1,0)
tab1=table(test$good,yhat_y1)
tab1

pred2 <- predict(stepmod,test,type="response")
yhat_2=ifelse(pred2>=thresh[J],1,0)
tab2=table(test$good,yhat_2)
tab2





# ANOVA
# accuracy ecc
# MSE


