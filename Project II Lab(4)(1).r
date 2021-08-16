# I use data set 110 as an example to do this LAB
# This may not 100% give you the correct answer if you simply repeat the script on your data set
# You need to try different transformation and play with these functions


# Set your working directory below
dir <-  "D:\\Users\\zsongzhu\\Desktop"
setwd(dir)

# Read the data
test <- read.csv("Group_105.csv", header=TRUE)
test = data.cart
# How the data looks like
View(test)

# Get the regression formula
# If you have smarter method please let me know. And you're welcomed to share with your classmates

E_var <- regmatches(names(test), regexpr("(E[0-9])+", names(test)))
G_var <- regmatches(names(test), regexpr("(R[0-9]*)+", names(test)))

(E_eff <- E_var)
(E_vs_G   <- paste0(paste( 'I(', c(gsub(" ", "*", outer(E_var, G_var, paste))), ')'), collapse = "+"))
(G_eff_1  <- paste("I(", G_var, ")", collapse="+"))
(G_eff_2  <- paste("I(", apply( combn(G_var, 2), 2, paste, collapse="*"), ")", collapse="+"))
(G_eff_3  <- paste("I(", apply( combn(G_var, 3), 2, paste, collapse="*"), ")", collapse="+"))
(G_3_E_eff <- gsub(' ', '*', outer(E_eff, as.vector(apply( combn(G_var, 3), 2, paste, collapse="*")), paste)))
G_eff_1_2 <-  paste("I(", G_var,"*",G_var ")", collapse="+")

#  ! Note:-------------------------------------------------------------
#  If You want to do a transformation, say square, then you might do  |
(E_eff    <- paste0("I(", E_var,"^2", ")", collapse = "+"))
(G_2    <- paste0("I(", G_var,"^2", ")", collapse = "+"))   #|
# --------------------------------------------------------------------|

# Get the formular
(formula <- paste( "I(Y) ~" , paste(c(E_var,E_eff, G_eff_1, G_eff_2, G_eff_3, G_2, E_vs_G), collapse = "+")))
# Could do transformation below
(formula <- paste( "I(log(Y)) ~" , paste(c(E_var, E_eff, G_eff_1, G_eff_2, G_eff_3, E_vs_G, G_2), collapse = "+")))

library(MASS)
M <- lm(formula, data=test)
# If you want to do a boxcox transformation
boxcox( Y ~ ., data=test, lambda = seq(-2, 10, 1/10))
summary(M)$r.squared
bc <- boxcox(Y ~ 1 + E1 + E3 + E4 + E5, data=test, lambda = seq(-2, 10, 1/10))
(lambda <- bc$x[which.max(bc$y)])
new_model <- lm(((Y^lambda-1)/lambda) ~ 1 + E1 + E3 + E4+ E5,data=test)
summary(new_model)

# ----------------------------------------------  Model Selection Procedure -----------------------------------------------
# Install "leaps" package before if you don't have
# install.packages("leaps", dependencies = TRUE)

library(leaps)

# Varaible Selection with leaps
# "regsubsets()" is model selector with which you could get the best model out out different covariate size
# Transform your DV --------------------------- here right below -------------------------------
M_selected <- regsubsets(model.matrix(M)[,-1], I(log(test$Y)), really.big = TRUE,
                         method="forward", nbest=1, intercept = TRUE, nvmax=500)
final_result <- summary(M_selected)
final_result
# Best Model with one covariate
colnames(final_result$which)[final_result$which[1,]]
# We have three proposals, you could change it with nbest argument
colnames(final_result$which)[final_result$which[2,]]
colnames(final_result$which)[final_result$which[3,]]
colnames(final_result$which)[final_result$which[4,]]
colnames(final_result$which)[final_result$which[5,]]

# look at other size
colnames(final_result$which)[final_result$which[7,]]
colnames(final_result$which)[final_result$which[9,]]
colnames(final_result$which)[final_result$which[13,]]
colnames(final_result$which)[final_result$which[20,]]

# How to decide the model is the best
# There are a lot of criterion you could turn to

final_result$rsq[(length(final_result$rsq)-5):length(final_result$rsq)] # R^2
final_result$rss[(length(final_result$rsq)-5):length(final_result$rsq)] # sum of square of residual
final_result$bic[(length(final_result$rsq)-5):length(final_result$rsq)] # bayesian informaiton criterion

# Look at the BIC and R^2 and how they're changed
summary(final_result)
num_var <- apply(final_result$which, 1, sum)
r2  <- final_result$rsq
rss <- final_result$rss
bic <- final_result$bic
r2_diff <- c(0,diff(r2))
rss_diff <- c(0,diff(rss))
bic_diff <- c(0,diff(bic))
par(mfrow=c(2,2))
plot(r2[1:25] ~ num_var[1:25], type='h',
     ylab = expression(R^2), xlab='Num of Vars')
plot(bic[1:25] ~ num_var[1:25], type='h',
     ylab=expression(BIC), xlab='Num of Vars')
plot(r2_diff[1:25] ~ num_var[1:25], type='h',
     ylab = expression(paste(Delta,R^2)), xlab='Num of Vars')
abline(v=4.3, lty=2, col='blue', lwd=2)
plot(bic_diff[1:25] ~ num_var[1:25], type='h',
     ylab = expression(paste(Delta, BIC)), xlab='Num of Vars')
abline(v=4.3, lty=2, col='blue', lwd=2)
par(mfrow=c(1,1))
criter <- cbind(num_var, r2, rss, bic, r2_diff, rss_diff, bic_diff)
View(criter[c(1:20),])
# Pick out the best one based on your knowledge got from the lecture

# explore more by looking into final_result
# or ?regsubsets
var_chose <- colnames(final_result$which)[final_result$which[5,]]
formula_select  <- paste0('I(log(Y)) ~ ', paste(var_chose[-1], collapse = '+') )
M <- lm(formula_select, data=test)
summary(M)
anova(M)
# Want only keep terms with t value larger than 4 here

(var_select <- colnames(final_result$which)[final_result$which[5,]])
(var_select <- paste0(gsub('I|\\(|\\)|\\* ', ' ', var_select), collapse = ' '))
(var_select <- as.vector(unlist(strsplit(var_select, split = ' '))))
(var_select <- unique(var_select))
# Final vars that are kept
keept_var <- sort(var_select[-c(1:2)])
keept_var

# --------------------------- This Part Depends on Your Keept_var out put -------------------------------------------------------------------------
# Split these interaction and put all of the terms that has these vars into the model
(GG_eff <- paste( 'I(', apply( combn( c('R10', 'R16', 'R18'), 2 ), 2, paste, collapse='*'), ")" , collapse= '+'))
(EG_eff <- paste( 'I(', gsub(' ', '*', outer( c('E1','E3', 'E4', 'E5') , c('R10', 'R16', 'R18') , FUN=paste) ) , ')', collapse = '+'))
(GGG_eff <- paste( paste( 'I(' ,apply( combn( c('R10', 'R16', 'R18'), 3 ), 2, paste, collapse='*'), ')' ) , collapse='+') )
(formula <- paste( c('Y ~ .', GG_eff, EG_eff, GGG_eff), collapse = '+'))
formula
# ----------------------------- Change to your G and E --------------------------------------------------------------------------------------------

M_final_select <- lm(formula, data=test)
(M_summary <- summary(M_final_select))
M_summary$coefficients[ abs(M_summary$coefficients[,3]) > 4 , ]
anova(M_final_select)

# ----------------------------------------------------- LASSO regression ------------------------------------------------

# You could also try LASSO regression

# install the packages "glmnet()" if you don't have one
# install.packages('glmnet')

# Reference : https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
(formula <- paste( "I(Y) ~" , paste(c(E_eff, G_eff_1, G_eff_2, G_eff_3, E_vs_G), collapse = "+")))
M <- lm(formula, data=test)

library(glmnet)

# look ?glmnet for more detail
# it takes some time
M_LASS <- cv.glmnet(model.matrix(M)[,-1], test$Y, nfolds = 10, alpha=1)
coef_select <- as.matrix(coef(M_LASS, s="lambda.1se"))
cbind(rownames(coef_select)[coef_select > 0], coef_select[coef_select > 0])
# Choose significant coefficient

# A better way to do this is select by t value first and then do LASSO
var_chose <- colnames(final_result$which)[final_result$which[15,]] # <- I use 10th here
formula_select  <- paste0('log(I(Y)) ~ ', paste(var_chose[-1], collapse = '+') )
M <- lm(formula_select, data=test)
M_summary <- summary(M)
rownames( M_summary$coefficients[ M_summary$coefficients[,4] < 0.01, ] )

M_LASS <- cv.glmnet(model.matrix(M), test$Y, nfolds = 5, alpha=1, grouped = TRUE)
coef_select <- as.matrix(coef(M_LASS, s="lambda.min"))
cbind(rownames(coef_select)[coef_select > 0], coef_select[coef_select > 0])
M_LASS$lambda.1se
coef_select


