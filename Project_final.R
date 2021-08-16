library(naniar)
library(mice)
library(VIM)
library(readr)
library(ggpubr)
library(leaps)
library(lattice)
library(MASS)

IDEgroup <- read_csv("C:/Users/Nick/OneDrive/Documents/Spring 2021/AMS 578 Regr/Project/IDEgroup355429.csv")
IDGgroup <- read_csv("C:/Users/Nick/OneDrive/Documents/Spring 2021/AMS 578 Regr/Project/IDGgroup355429.csv")
IDYgroup <- read_csv("C:/Users/Nick/OneDrive/Documents/Spring 2021/AMS 578 Regr/Project/IDYgroup355429.csv")

IDE <- subset(IDEgroup[order(IDEgroup$ID),], select = -c(X1,ID))
IDG <- subset(IDGgroup[order(IDGgroup$ID),], select = -c(X1,ID))
IDY <- subset(IDYgroup[order(IDYgroup$ID),], select = -c(X1,ID))
dataset <- cbind(IDE,IDG,IDY)

sum.stat = summary(dataset)
sum.stat

#Missing Value Analysis
data.na =is.na(dataset)
row.sums = rep(NA,length(data.na[,1]))
for(i in 1:length(data.na[,1])){
 row.sums[i] = sum(data.na[i,])
}
col.sums = rep(NA,length(data.na[1,]))
col.sd = rep(NA,length(data.na[1,]))
for(i in 1:length(data.na[1,])){
  col.sums[i] = sum(data.na[,i])
  col.sd[i] = sd(dataset[,i],na.rm=TRUE)
}
col.sums = data.frame(col.sums,row.names = colnames(dataset))
col.sddf = data.frame(col.sd,  col.names = colnames(dataset))
col.sddf
col.sums

data.na.num = matrix(lapply(data.na, as.numeric),ncol= 32)

levelplot(t(data.na.num[1:250,]))

mat.cor = cor(dataset,use = "complete.obs")
mat.cor
mat.cor.e = cor(IDE,use = "complete.obs")
mat.cor.g = cor(IDG,use = "complete.obs")

vis_miss(dataset)
gg_miss_upset(subset(dataset,select=-c(R5,R6,R7,R8,R9,R10,R13,R14,R15,R16,R18,R19,R20,R22,R24)),nsets=40,nintersects=50)
gg_miss_fct(dataset,fct=R5)
gg_miss_fct(dataset,fct=R6)
gg_miss_fct(dataset,fct=R8)
gg_miss_fct(dataset,fct=R25)


#Imputation

cart.impute<- mice(dataset, m=6, maxit = 10, method = 'cart', seed = 500)
summary(cart.impute)
#This generates 16 different data sets using the cart method,
#we will take the 15th but return to pool them all after
#model building
data.cart <- complete(cart.impute,5)


#Inspection
summary(data.cart)
#boxplot(log(data.cart$Y))
xyplot(cart.impute,Y~ R1+R2+R3+R4+R5+R6+R7+R8+R9+R10+R11+R12+R13+R14+R15+R16+R17+R18+R19+R20+R21+R22+R23+R24+R25,pch=18,cex=1)
xyplot(cart.impute,Y~ E1+E2+E3+E4+E5+E6,pch=18,cex=1)
densityplot(cart.impute)


ggqqplot(data.cart$Y)

#model building

test = data.cart
M1 = lm( log(Y) ~(.)^3,data=data.cart)
leaps_int = regsubsets(model.matrix(M1)[,-1], I(log(data.cart$Y)),really.big = TRUE,
                    method = "forward", nbest = 1, intercept = TRUE, nvmax = 500)

leaps_noint = regsubsets(Y~.,data=data.cart,nbest=1,nvmax = 500,method = "exhaustive",intercept = TRUE)


final_result_int <- summary(leaps_int)


#No interactions plot
plot(leaps_noint,scale="bic")

plot(leaps_noint,scale="adjr2")

plot(leaps_int,scale="bic")

plot(leaps_int,scale="adjr2")

final_result_int$rsq
final_result_int$bic[1:20]
colnames(final_result_int$which)[final_result_int$which[10,]]

#look at models with 10 or less terms

var_chose <- colnames(final_result_int$which)[final_result_int$which[10,]]
formula_select  <- paste0('log(Y) ~ ', paste(var_chose[-1], collapse = '+') )
M <- lm(formula_select, data=data.cart)
summary(M)
anova(M)
#Keep terms with F > 16

#Hierarchical model
M2 = lm(log(Y) ~ 1 + E1+ E3 + E4 + E5 + E1:E3 + E4:E5 + R12 +R11 + R22 + R12:R11:R22 + R11:R22 + R22:R12 + R12:R11 ,data=data.cart)
summary(M2)
anova(M2)


M3 = lm(log(Y) ~ (1 + E1 + E3 + E4 + E5 + R8:R25:R3),data=data.cart)
summary(M3)
anova(M3)
plot(M3)

#non log model
M3 = lm(Y ~ 1 + E1 + E3 + E4 + E5 ,data=data.cart)
summary(M3)
anova(M3)
plot(M3)

#

#box cox
bc <- boxcox(Y ~., data=test, lambda = seq(-2, 10, 1/10))
(lambda <- bc$x[which.max(bc$y)])
new_model <- lm(((Y^lambda-1)/lambda) ~(.)^3,data=test)
leaps_bc = regsubsets(model.matrix(new_model)[,-1], I((data.cart$Y^lambda-1)/lambda),really.big = TRUE,
                       method = "forward", nbest = 1, intercept = TRUE, nvmax = 500)
final_result_bc <- summary(leaps_bc)

var_chose <- colnames(final_result_bc$which)[final_result_bc$which[10,]]
formula_select  <- paste0('(Y^2.2-1)/2.2 ~ ', paste(var_chose[-1], collapse = '+') )
M <- lm(formula_select, data=data.cart)
summary(M)
anova(M)
#comparable to the analysis before
plot(M)

#merging of the data
fit <- with(cart.impute, lm(log(Y) ~ E1 + E3 + E4 + E5 + 1))
combine <- pool(fit)
summary(combine)
#aov(combine)


#Here are some of the other methods i had tried
#
#null = lm(Y~1,data=data.cart)
#null
#full = lm(Y~.,data=data.cart)
#full
#step(null,scope=list(lower=null,upper=full),direction="forward")


#leaps
#summary.out <- summary(leaps)
#as.data.frame(summary.out$outmat)

#library(car)
#subsets(leaps,statistic="bic",max.size = 10,)


#library(MASS)
#fit.test = lm(Y~., data= data.cart)
#step <- stepAIC(fit.test,scope =list(upper=  . ~ .^2, lower= ~1),direction="both")
#step$anova

#library(rFSA)
#fsa.fit = FSA(Y~.,data=data.cart,fitfunc = lm,m=3,numrs = 50,criterion = BIC)
#print(fsa.fit)

#library(bestglm)
#bestglm(data.cart,IC="BIC",family=binomial)

#library(glmnet)
#f <- as.formula(Y ~ .^3)
#x <- model.matrix(f,data.cart)[,-1]
#x


#glmnet(x,data.cart$Y)
#M_LASS <- glmnet::cv.glmnet(x, data.cart$Y,nfolds = 5, alpha=1, grouped = TRUE)
#coef_select <- as.matrix(coef(M_LASS, s="lambda.1se"))
#cbind(rownames(coef_select)[coef_select > 0], coef_select[coef_select > 0])
#M_LASS$lambda.1se
#coef_select






