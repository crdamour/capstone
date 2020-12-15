library(caret)
library(MASS)
path <- 'C:/Users/Chris_D/Documents/Github/levels'
setwd(path)
df <- read.csv(paste(path, '10_11_20_enriched.csv', sep = '/'))

#Grabbing only needed columns.
cols <- c(c('Years.of.Experience', 'Years.at.Company', 'CRMCYTOTC', 'DIVINDX_CY', 'X9001_I', 'HAI_CY'),
          #c('Gender', 'Race'),
          c('Company', 'Gender', 'Race'),
          colnames(df)[118:191],
          c('Total.Compensation'))

#Getting index of columns to derive from df.
ind <- numeric(length(cols))
counter <- 1
for (c in cols){
  ind[counter] <- grep(c, colnames(df))
  counter <- counter + 1
}

df <- df[, c(ind)]
#Log transforming target variable due to it's skew.
df$Total.Compensation <- log(df$Total.Compensation)

#detach(df)
attach(df)

#Stepwise regression. Eliminates many variables, retains some that are still highly correlated.
#fit_c <- lm(Total.Compensation ~ ., data = df)
#sfit_c <- step(fit_c, direction = 'both')
#summary(sfit_c)

#Removed categorical vars and evaluated corollaries. Eliminating cols with higher correlation than .7.
tmp <- cor(df[, -c(match('Total.Compensation', cols))])
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0
df.new <- df[,!apply(tmp,2,function(x) any(x > 0.7))]
cor(df.new)

#New cols:
new_cols <- c(colnames(df.new)[1:6], c('Company', 'Gender', 'Race'))

#Using amended columns this time.
fit_c <- lm(Total.Compensation ~ Years.of.Experience+Years.at.Company+CRMCYTOTC+HAI_CY+OCCFARM_CY_sqmi+RENTER_CY_sqmi+Company+Gender+Race, data = df)
sfit_c <- step(fit_c, direction = 'both')
summary(sfit_c)
coef_df <- as.data.frame(summary(sfit_c)$coefficients)
plot(sfit_c)

#Creating column for variables in lieu of rownames.
coef_df$Variables <- rownames(coef_df)
#Sets row names (index) back to numbers.
rownames(coef_df) <- c()
#Rearranging df.
coef_df <- coef_df[, c(5, 1:4)]
#Creating p value markers to understand degree of significance.
coef_df$significance <- ifelse(coef_df$`Pr(>|t|)` <= 0.001, '***',
                               ifelse(coef_df$`Pr(>|t|)` <= 0.01, '**',
                                      ifelse(coef_df$`Pr(>|t|)` <= 0.05, '*',
                                             ifelse(coef_df$`Pr(>|t|)` <= 0.1, '.', ''))))

#To compute the impact of independent variable on dependent variable since only
#log transformed the dependendent variable. This yields the percent increase/decrease in the
#response variable for every one unit increase in the independent variable.

coef_df$impact <- (exp(coef_df$Estimate) - 1) * 100#(exp(coef_df$Estimate) - 1) * 100

#Cross Validation.
train.control <- trainControl(method = "cv", number = 5)
sfit_comp <- train(Total.Compensation ~ Years.of.Experience+Years.at.Company+CRMCYTOTC+HAI_CY+OCCFARM_CY_sqmi+RENTER_CY_sqmi+Company+Gender+Race,
                   data = df,
                   method = 'lmStepAIC',
                   trControl = train.control,
                   trace = FALSE)


#Exporting to csv.
write.csv(coef_df, 'coefficient_reg_results2.csv', row.names = FALSE)