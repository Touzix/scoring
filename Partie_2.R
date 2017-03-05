library(xlsx)
#import Data
data_scoring=read.xlsx("c:/Users/imsd/Desktop/IMSD2016/Scoring/data_scoring.xlsm", 
               sheetName = "data")
#summar data
summary(data_scoring)
#???
data_scoring$Default=factor(data_scoring$Default)

pairs(data_scoring[3:8],col = data_scoring$Default)

##########PArtie B ###########
library(histogram)
par(mfrow=c(2,3))
hist(data_scoring$WCTA,main ="WCTA" )
  abline(v=quantile(data_scoring$WCTA),col="red")

hist(data_scoring$RETA,main ="RETA")
abline(v=quantile(data_scoring$RETA),col="red")

hist(data_scoring$EBIT_TA,main ="EBIT_TA")
abline(v=quantile(data_scoring$EBIT_TA),col="red")

hist(data_scoring$METL,main ="METL")
abline(v=quantile(data_scoring$METL),col="red")

hist(data_scoring$STA,main ="STA")
abline(v=quantile(data_scoring$STA),col="red")

#discretisation
##avec les distribution ###
BreaksWCTA=c(min(data_scoring$WCTA),0,0.4,max(data_scoring$WCTA))
data_scoring$WCTA.d = cut(data_scoring$WCTA, breaks = BreaksWCTA, include.lowest = TRUE)
summary(data_scoring$WCTA.d)


BreaksRETA=c(min(data_scoring$RETA),0,0.5,max(data_scoring$RETA))
data_scoring$RETA.d = cut(data_scoring$RETA, breaks = BreaksRETA, include.lowest = TRUE)
summary(data_scoring$RETA.d)

BreaksEBIT_TA=c(min(data_scoring$EBIT_TA),0,0.1,max(data_scoring$EBIT_TA))
data_scoring$EBIT_TA.d = cut(data_scoring$EBIT_TA, breaks = BreaksEBIT_TA, include.lowest = TRUE)
summary(data_scoring$EBIT_TA.d)

BreaksMETL=c(min(data_scoring$METL),5,max(data_scoring$METL))
data_scoring$METL.d = cut(data_scoring$METL, breaks = BreaksMETL, include.lowest = TRUE)
summary(data_scoring$METL.d)

BreaksSTA=c(min(data_scoring$STA),0.5,max(data_scoring$STA))
data_scoring$STA.d = cut(data_scoring$STA, breaks = BreaksSTA, include.lowest = TRUE)
summary(data_scoring$STA.d)
##avec arbre de decision ##
library(rpart)
arbre=rpart(Default~WCTA+RETA+EBIT_TA+METL+STA,data =data_scoring)
plot(arbre)
text(arbre)

arbre_WCTA=rpart(Default~WCTA,data =data_scoring)
plot(arbre_WCTA)
text(arbre_WCTA)
#2) WCTA< -0.6513918 7  1.714286 0.5714286 *
# 3) WCTA>=-0.6513918 3993 67.807660 0.9827198 *
#c'est pas interessant car une classe contient que 7 individu et l'autre 3993

##avec CAH ##

###avec WCTA ###
d_WCTA=dist(data_scoring$WCTA)
clust_WCTA=hclust(d_WCTA,method="ward.D2")
plot(clust_WCTA)
data_scoring$WCTA_CAH=cutree(clust_WCTA, 3)

###avec RETA ###
d_RETA=dist(data_scoring$RETA)
clust_RETA=hclust(d_RETA,method="ward.D2")
plot(clust_RETA)
data_scoring$RETA_CAH=cutree(clust_RETA, 3)

##avec EBIT_TA ##
d_EBIT_TA=dist(data_scoring$EBIT_TA)
clust_EBIT_TA=hclust(d_EBIT_TA,method="ward.D2")
plot(clust_EBIT_TA)
data_scoring$EBIT_TA_CAH=cutree(clust_EBIT_TA, 3)

##avec METL ##
d_METL=dist(data_scoring$METL)
clust_METL=hclust(d_METL,method="ward.D2")
plot(clust_METL)
data_scoring$METL_CAH=cutree(clust_METL, 3)

##avec STA ##
d_STA=dist(data_scoring$STA)
clust_STA=hclust(d_STA,method="ward.D2")
plot(clust_STA)
data_scoring$STA_CAH=cutree(clust_STA, 3)

####QUESTION 4 ####???
pairs(data_scoring[9:13],col = data_scoring$Default)
#biensur on peut rien conclure à partir du nuage des point 
Khi2_W_R = chisq.test(data_scoring$WCTA_CAH,data_scoring$RETA_CAH, correct=F)
Khi2_W_E = chisq.test(data_scoring$WCTA_CAH,data_scoring$EBIT_TA_CAH, correct=F)
Khi2_W_M = chisq.test(data_scoring$WCTA_CAH,data_scoring$METL_CAH, correct=F)
Khi2_W_S = chisq.test(data_scoring$WCTA_CAH,data_scoring$STA_CAH, correct=F)

Khi2_R_E = chisq.test(data_scoring$RETA_CAH,data_scoring$EBIT_TA_CAH, correct=F)
Khi2_R_M = chisq.test(data_scoring$RETA_CAH,data_scoring$METL_CAH, correct=F)
Khi2_R_S = chisq.test(data_scoring$RETA_CAH,data_scoring$STA_CAH, correct=F)

Khi2_E_M = chisq.test(data_scoring$EBIT_TA_CAH,data_scoring$METL_CAH, correct=F)
Khi2_E_S = chisq.test(data_scoring$EBIT_TA_CAH,data_scoring$STA_CAH, correct=F)

Khi2_M_S = chisq.test(data_scoring$METL_CAH,data_scoring$STA_CAH, correct=F)

Khi2_W_R$statistic
####QUESTION 5 ####???
data_scoring$WCTA_CAH=factor(data_scoring$WCTA_CAH)
data_scoring$RETA_CAH=factor(data_scoring$RETA_CAH)
data_scoring$METL_CAH=factor(data_scoring$METL_CAH)
data_scoring$EBIT_TA_CAH=factor(data_scoring$EBIT_TA_CAH)
data_scoring$STA_CAH=factor(data_scoring$STA_CAH)

frequencies(data_scoring$WCTA_CAH)
#on doit choisir la modalité de reference, celle qui est beaucoup présente dans notre data
"------------------------------------------------------------
  --                        Frequencies                     --
  --                                                        --
  Value # of Cases       % Cumulative %
1     1        793    19.8         19.8
2     2       2026    50.6         70.5
3     3       1181    29.5        100.0 "

data_scoring$WCTA_CAH <- relevel(data_scoring$WCTA_CAH, "2")

data_scoring$STA_CAH <- relevel(data_scoring$STA_CAH, "2")


fullmod=glm(formula = Default~WCTA_CAH+RETA_CAH+EBIT_TA_CAH+METL_CAH+STA_CAH, data = data_scoring, family=binomial(link="logit"))
nothing <- glm(Default ~ 1, data = data_scoring,family=binomial)

backwards=step(fullmod,direction = "backward")
forwards = step(nothing,
                scope=list(lower=formula(nothing),upper=formula(fullmod)), direction="forward")
bothways =step(nothing, list(lower=formula(nothing),upper=formula(fullmod)),
         direction="both")
formula(backwards)
formula(forwards)
formula(bothways)
AIC(backwards)
AIC(forwards)
AIC(bothways)



### ROC ####
library(Deducer)
par(mfrow=c(2,2))
rocplot(backwards)
rocplot(forwards)

iter1=glm(Default ~ WCTA_CAH + RETA_CAH + EBIT_TA_CAH + METL_CAH + STA_CAH,
          data = data_scoring,family=binomial)
iter2=glm(Default ~ RETA_CAH + EBIT_TA_CAH + METL_CAH + STA_CAH,
          data = data_scoring,family=binomial)
iter3=glm(Default ~ RETA_CAH + EBIT_TA_CAH + STA_CAH,
          data = data_scoring,family=binomial)
par(mfrow=c(3,3))

rocplot(iter1)
rocplot(iter2)
rocplot(iter3)

#question 7
best_mod=backwards
best_mod$coefficients
