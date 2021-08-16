

# https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/

library(readr)
IDEgroup <- read_csv("C:/Users/Nick/OneDrive/Documents/Spring 2021/AMS 578 Regr/Project/IDEgroup355429.csv")
IDGgroup <- read_csv("C:/Users/Nick/OneDrive/Documents/Spring 2021/AMS 578 Regr/Project/IDGgroup355429.csv")
IDYgroup <- read_csv("C:/Users/Nick/OneDrive/Documents/Spring 2021/AMS 578 Regr/Project/IDYgroup355429.csv")

IDE <- subset(IDEgroup[order(IDEgroup$ID),], select = -c(X1,ID))
IDG <- subset(IDGgroup[order(IDGgroup$ID),], select = -c(X1,ID))
IDY <- subset(IDYgroup[order(IDYgroup$ID),], select = -c(X1,ID))
dataset <- cbind(IDE,IDG,IDY)
sd(dataset)
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

library(mice)
library(VIM)
md.pattern(dataset)
my.mice = function(data){
  mice_plot <- aggr(data, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(data), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))
}
md.pattern(IDE)
md.pattern(IDG)
md.pattern(IDY)
md.pattern(subset(IDG,select=-c(R5,R6,R7,R8,R9,R10,R13,R14,R15,R16,R18,R19,R20,R22,R24)))
my.mice(IDE)
my.mice(IDG)
my.mice(subset(IDG,select=-c(ID,R5,R6,R7,R8,R9,R10,R13,R14,R15,R16,R18,R19,R20,R22,R24)))
my.mice(IDY)

mat.cor = cor(dataset,use = "complete.obs")
mat.cor.e = cor(IDE,use = "complete.obs")
mat.cor.g = cor(IDG,use = "complete.obs")

library(naniar)
vis_miss(dataset)
gg_miss_upset(subset(dataset,select=-c(R5,R6,R7,R8,R9,R10,R13,R14,R15,R16,R18,R19,R20,R22,R24)),nsets=40,nintersects=50)
gg_miss_fct(dataset,fct=R5)
gg_miss_fct(dataset,fct=R6)
gg_miss_fct(dataset,fct=R8)
gg_miss_fct(dataset,fct=R25)

#Imputation

#Not used
#pmm.impute<- mice(dataset, m=16, maxit = 50, method = 'pmm', seed = 500)
summary(pmm.impute)
data.pmm <- complete(pmm.impute,15)
summary(data.pmm)
data.pmm

#rf.impute<- mice(dataset, m=16, maxit = 50, method = 'rf', seed = 500)
summary(rf.impute)
data.rf <- complete(rf.impute,15)
summary(data.rf)
data.rf

#Used
cart.impute<- mice(dataset, m=16, maxit = 50, method = 'cart', seed = 500)
summary(cart.impute)
data.cart <- complete(cart.impute,15)

#Inspection
summary(data.cart)
xyplot(cart.impute,Y ~ E1 + E3 + E4 + E5 + 1,pch=18,cex=1)
densityplot(cart.impute)
stripplot(cart.impute, pch = 20, cex = 1.2)


data.cart


#trying to join the different predictions
test_data = as.mira(imputed_Data)

fit <- with(cart.impute, lm(log(Y) ~ E1 + E3 + E4 + E5 + 1))
combine <- pool(fit)
summary(combine)

library(missForest)
mf.imput = missForest(dataset)
mf.impute1 = missForest(dataset,variablewise = TRUE, verbose = TRUE)
mf.impute2 = missForest(IDG,variablewise = TRUE, verbose = TRUE)

mf.imput$OOBerror

mat.cor.impute.cart = cor(data.cart)
err.cor.cart = mat.cor - mat.cor.impute.cart
max(err.cor.cart)
max(mat.cor.impute.cart-diag(32))

mat.cor.impute.pmm = cor(data.pmm)
err.cor.pmm = mat.cor - mat.cor.impute.pmm
max(err.cor.pmm)
max(mat.cor.impute.pmm-diag(32))

library(ggpubr)
library(leaps)
ggqqplot(log(data.cart$Y))

#model building

test = data.cart
M1 = lm( Y ~ (.)^3,data=data.cart)
leaps_int = regsubsets(model.matrix(M1)[,-1], I(log(data.cart$Y)),really.big = TRUE,
                    method = "forward", nbest = 1, intercept = TRUE, nvmax = 500)

leaps_noint = regsubsets(Y~.,data=data.cart,nbest=1,nvmax = 500,method = "forward",intercept = TRUE)


final_result_int <- summary(leaps_int)
#look at models with 8 or less terms
var_chose <- colnames(final_result_M$which)[final_result_M$which[8,]]
formula_select  <- paste0('I(log(Y)) ~ ', paste(var_chose[-1], collapse = '+') )
M <- lm(formula_select, data=data.cart)
summary(M)
anova(M)
#Keep terms with F > 16

M2 = lm(Y ~ 1 + E1 + E3 + E4 + E5,data=data.cart)
summary(M2)
anova(M2)


var_chose <- colnames(final_result_M$which)[final_result_M$which[4,]]
formula_select  <- paste0('I(log(Y)) ~ ', paste(var_chose[-1], collapse = '+') )
M <- lm(formula_select, data=data.cart)
summary(M)
anova(M)

var_chose <- colnames(final_result_L1$which)[final_result_L1$which[5,]]
formula_select  <- paste0('I(Y) ~ ', paste(var_chose[-1], collapse = '+') )
M <- lm(formula_select, data=data.cart)
summary(M)
anova(M)

var_chose <- colnames(final_result_L1$which)[final_result_L1$which[5,]]
formula_select  <- paste0('I(log(Y)) ~ ', paste(var_chose[-1], collapse = '+') )
M <- lm(formula_select, data=data.cart)
summary(M)
anova(M)

M2 = lm(Y ~ 1 + E1 + E3 + E4 + E5,data=data.cart)
summary(M2)
anova(M2)

M3 = lm(log(Y) ~ 1 + E1 + E3 + E4 + E5,data=data.cart)
summary(M3)
anova(M3)

#No interactions plot
plot(leaps_noint,scale="bic")

plot(leaps_noint,scale="adjr2")

plot(leaps_int,scale="bic")

plot(leaps_int,scale="adjr2")

plot(M_selected,scale="bic")

plot(M_selected,scale="adjr2")


fit <- with(data = data.cart, exp = M3)
combine <- pool(fit)
summary(combine)


null = lm(Y~1,data=data.cart)
null
full = lm(Y~.,data=data.cart)
full
step(null,scope=list(lower=null,upper=full),direction="forward")


leaps
summary.out <- summary(leaps)
as.data.frame(summary.out$outmat)

library(car)
subsets(leaps,statistic="bic",max.size = 10,)


library(MASS)
fit.test = lm(Y~., data= data.cart)
step <- stepAIC(fit.test,scope =list(upper=  . ~ .^2, lower= ~1),direction="both")
step$anova

library(rFSA)
fsa.fit = FSA(Y~.,data=data.cart,fitfunc = lm,m=3,numrs = 50,criterion = BIC)
print(fsa.fit)

library(bestglm)
bestglm(data.cart,IC="BIC",family=binomial)

library(glmnet)
f <- as.formula(Y ~ .^3)
x <- model.matrix(f,data.cart)[,-1]
x

M_selected <- regsubsets(model.matrix(M)[,-1], I(log(test$Y)), really.big = TRUE,
                         method="forward", nbest=1, intercept = TRUE, nvmax=500)


glmnet(x,data.cart$Y,family = 'mgaussian')
cvfit <- glmnet::cv.glmnet(x, data.cart$Y)
coef(cvfit, s = "lambda.1se")
cvfit
aov(cvfit)




