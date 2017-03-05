#importation des données:
data =read.csv("data.csv")  # read csv file 
data$Default<-as.factor(data$Default)

#part1: regression logistique:

#val manquantes..:
na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
na_count

#val extremes:
par(mfrow=c(1,5))
boxplot(data$WCTA)
boxplot(data$RETA)
boxplot(data$EBIT_TA)
boxplot(data$METL)
boxplot(data$STA)



library(nortest)
lillie.test(data$STA)

#teste de normalité 
qqnorm(data$WCTA)
qqnorm(data$RETA)
qqnorm(data$EBIT_TA)
qqnorm(data$METL)
qqnorm(data$STA)


qqplot(1:300,data$STA)
qqplot(1:300,data$RETA)



t.test(data$STA)

boxplot(Default ~ WCTA+RETA+EBIT_TA+METL+STA, data = data, col = "lightgray")


#stat descriptive:
summary(data)

#stat bivarié:
cor(as.matrix(data[4:8]), method = c("pearson", "kendall", "spearman"))

#qual-num:
#WCTA-DEFAULT:
X <- data$WCTA[data$Default==0]
Y <- data$WCTA[data$Default==1]
lillie.test(X)#p-val<5% => on rejette la normalité
lillie.test(Y)#p-val<5% => on rejette la normalité
#cl: nous pouvons pas appliquer le test de Student  t.test(X,Y)
wilcox.test(X,Y)#p-value>5% => on ne rejette pas H0 
#=> on ne rejette pas que les 2 echant suivent la mm loi
#conditions:
length(unique(X))==length(X)
length(unique(Y))==length(Y)

#RETA-DEFAULT
X <- data$RETA[data$Default==0]
Y <- data$RETA[data$Default==1]
lillie.test(X)#p-val<5% => on rejette la normalité
lillie.test(Y)#p-val<5% => on rejette la normalité
#cl: nous pouvons pas appliquer le test de Student  t.test(X,Y)
wilcox.test(X,Y)#p-value<5% => on rejette  H0 
#=> on ne rejette pas que les 2 echant suivent la mm loi
#conditions:
length(unique(X))==length(X)#false
length(unique(Y))==length(Y)#false

#EBITA_TA-DEFAULT
X <- data$EBIT_TA[data$Default==0]
Y <- data$EBIT_TA[data$Default==1]
lillie.test(X)#p-val<5% => on rejette la normalité
lillie.test(Y)#p-val<5% => on rejette la normalité
#cl: nous pouvons pas appliquer le test de Student  t.test(X,Y)
wilcox.test(X,Y)#p-value<5% => on rejette  H0 
#=> on ne rejette pas que les 2 echant suivent la mm loi
#conditions:
length(unique(X))==length(X)#false
length(unique(Y))==length(Y)#false


#STA-DEFAULT
X <- data$METL[data$Default==0]
Y <- data$METL[data$Default==1]
lillie.test(X)#p-val<5% => on rejette la normalité
lillie.test(Y)#p-val<5% => on rejette la normalité
#cl: nous pouvons pas appliquer le test de Student  t.test(X,Y)
wilcox.test(X,Y)#p-value<5% => on rejette  H0 
#=> on ne rejette pas que les 2 echant suivent la mm loi
#conditions:
length(unique(X))==length(X)#false
length(unique(Y))==length(Y)#false


#boxplot
boxplot(data$WCTA,data$Default)

boxplot(data$RETA,data$Default)






#construction des modeles:
library(boot)                          
set.seed(1234)
model1 <- glm(Default~WCTA+RETA,family=binomial(link='logit'), data=data)
summary(model1)
model2<- glm(Default~METL+EBIT_TA,family=binomial(link='logit'), data=data)
summary(model2)
model3<- glm(Default~WCTA+RETA+EBIT_TA+STA,family=binomial(link='logit'), data=data) 
summary(model3)
model4<- glm(Default~WCTA+RETA+EBIT_TA+METL+STA,family=binomial(link='logit'), data=data) 
summary(model4)

#Courbe de ROC:
library(Deducer)
par(mfrow=c(2,2))
rocplot(model1)
rocplot(model2)
rocplot(model3)
rocplot(model4)

summary(model1)
#AIC & BIC:
AIC(model1)
BIC(model1)

#predict :WCTA = 0.6, RETA= 0.25, EBIT_TA = 0.45, METL= 0.05, STA= 0.72s)
x=as.data.frame(t(c(0.6,.25,.45,0.05,0.72)))
colnames(x) <- c("WCTA","RETA","EBIT_TA","METL","STA")
predlog=predict(model4,x,type='response')
res <- ifelse(predlog > 0.5,1,0)
cat ("la proba de ne pas avoir un défaut est:",predlog ,"Donc notre prédiction est:",res)

