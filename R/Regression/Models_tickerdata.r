library(haven)
library(multcomp)
library(tidyverse)
library(purrr)

# Load the data
data<- read_dta("../../Assignements/assignment 2/capm4.dta")

# Define the dependent variable
x <- data$mkt - data$riskfree

# Create a list of independent variables

df1 <- data[,-c(1,8,9)] - data$riskfree
y_vars <- list(df1$dis,df1$ge,df1$gm,data$ibm,df1$msft,df1$xom)
# Function to run regression model
run_regression <- function(y){
  model <- lm(y ~ x)
  return(model)
}

# Run the regression models using the map function from purrr package
models <- map(y_vars, run_regression)

# View the results
models

# run summaries for each model
summary <- map(models,summary)

# View the results
summary

# Outputs for all the models

1. Model DIS 
# Call:
# lm(formula = y ~ x)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.216340 -0.036961 -0.006176  0.033851  0.256670 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.001548   0.006550  -0.236    0.814    
# x            0.937986   0.135951   6.899 2.06e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.07525 on 130 degrees of freedom
# Multiple R-squared:  0.268,	Adjusted R-squared:  0.2624 
# F-statistic:  47.6 on 1 and 130 DF,  p-value: 2.059e-10


2.Model GE
 
# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.14820 -0.02945 -0.00501  0.03516  0.24402 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.002594   0.005379   0.482     0.63    
# x           0.936000   0.111646   8.384 7.32e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.0618 on 130 degrees of freedom
# Multiple R-squared:  0.3509,	Adjusted R-squared:  0.3459 
# F-statistic: 70.29 on 1 and 130 DF,  p-value: 7.323e-14
# 
# 
3. model GM
# 
# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.40624 -0.06862 -0.00414  0.06907  0.30505 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.01482    0.01016  -1.458    0.147    
# x            1.24977    0.21094   5.925 2.64e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1168 on 130 degrees of freedom
# Multiple R-squared:  0.2126,	Adjusted R-squared:  0.2066 
# F-statistic:  35.1 on 1 and 130 DF,  p-value: 2.637e-08
# 
# 
4. Model IBM
# 
# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.261043 -0.049185 -0.001385  0.046172  0.286288 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.008116   0.006862   1.183    0.239    
# x           1.089978   0.142424   7.653 3.91e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.07884 on 130 degrees of freedom
# Multiple R-squared:  0.3106,	Adjusted R-squared:  0.3053 
# F-statistic: 58.57 on 1 and 130 DF,  p-value: 3.906e-12
# 
# 
5. MSFT
# 
# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.26727 -0.04982 -0.00699  0.05214  0.34424 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.001913   0.007822   0.245    0.807    
# x           1.430611   0.162354   8.812  6.8e-15 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.08987 on 130 degrees of freedom
# Multiple R-squared:  0.3739,	Adjusted R-squared:  0.3691 
# F-statistic: 77.65 on 1 and 130 DF,  p-value: 6.803e-15
# 
# 
6. Model XOM
# 
# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.14779 -0.04010 -0.00335  0.03426  0.23473 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.010370   0.005115   2.027 0.044675 *  
#   x           0.406108   0.106170   3.825 0.000202 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.05877 on 130 degrees of freedom
# Multiple R-squared:  0.1012,	Adjusted R-squared:  0.09425 
# F-statistic: 14.63 on 1 and 130 DF,  p-value: 0.000202


# part c

# Fit the regression model
model <- models[[5]]

# Predict the values of y based on the regression model
df1$predicted <- predict(model)

# Plot the scatter plot of data
ggplot(data = df1, aes(y = df1$msft, x = x)) + 
  geom_point() + 
  geom_line(aes(x = x, y = predicted), color = "red") + 
  labs(x = "Independent Variable", y = "Dependent Variable")

# partd

run_regression_nointercept <- function(y){
  model <- lm(y ~ x-1)
  return(model)
}

models_nointercept <- map(y_vars, run_regression_nointercept)

# View the results
models_nointercept

# run summaries for each model
summary_nointercept <-   models_nointercept 

# View the results
summary_nointercept

# [[1]]
# 
# Call:
#   lm(formula = y ~ x - 1)
# 
# Coefficients:
#   x  
# 0.9381  
# 
# 
# [[2]]
# 
# Call:
#   lm(formula = y ~ x - 1)
# 
# Coefficients:
#   x  
# 0.9358  
# 
# 
# [[3]]
# 
# Call:
#   lm(formula = y ~ x - 1)
# 
# Coefficients:
#   x  
# 1.251  
# 
# 
# [[4]]
# 
# Call:
#   lm(formula = y ~ x - 1)
# 
# Coefficients:
#   x  
# 1.089  
# 
# 
# [[5]]
# 
# Call:
#   lm(formula = y ~ x - 1)
# 
# Coefficients:
#   x  
# 1.43  
# 
# 
# [[6]]
# 
# Call:
#   lm(formula = y ~ x - 1)
# 
# Coefficients:
#   x  
# 0.4054  

# Load the data
df_star<- read_dta("../../Assignements/assignment 2/fair4.dta")

# create the scatter plot with vote on x-axis and growth on y-axis

ggplot(df_star, aes(y = vote, x = growth)) +
  geom_point() + # add points to the plot
  ylab("Vote") + # x-axis label
  xlab("Growth") + # y-axis label
  ggtitle("Vote vs Growth") # plot title

model_vote_growth <- lm(data = df_star, vote ~ growth)
model_vote_growth %>% summary()
# Call:
#   lm(formula = vote ~ growth, data = df_star)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -9.310 -3.058 -1.582  3.313  9.602 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  51.6083     0.8712  59.241  < 2e-16 ***
#   growth        0.7150     0.1602   4.463 9.93e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.98 on 31 degrees of freedom
# Multiple R-squared:  0.3912,	Adjusted R-squared:  0.3716 
# F-statistic: 19.92 on 1 and 31 DF,  p-value: 9.934e-05
ggplot(df_star, aes(y = vote, x = growth)) +
  geom_point() + # add points to the plot
  ylab("Vote") + # x-axis label
  xlab("Growth") + # y-axis label+ 
  geom_smooth(method = "lm",formula =  y~x)+
  ggtitle("Vote vs Growth") # plot title

# part d
df_star1 <- df_star[df_star$year != 2008,]



model_vote_growth1 <- lm(data = df_star1, vote ~ growth)
model_vote_growth1 %>% summary()
# Call:
#   lm(formula = vote ~ growth, data = df_star1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -9.4922 -2.8681 -0.9679  3.2225  9.4338 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  51.7699     0.8832   58.61  < 2e-16 ***
#   growth        0.7133     0.1600    4.46 0.000107 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.972 on 30 degrees of freedom
# Multiple R-squared:  0.3986,	Adjusted R-squared:  0.3786 
# F-statistic: 19.89 on 1 and 30 DF,  p-value: 0.0001066
ggplot(df_star1, aes(y = vote, x = growth)) +
  geom_point() + # add points to the plot
  ylab("Vote") + # x-axis label
  xlab("Growth") + # y-axis label+ 
  geom_smooth(method = "lm",formula =  y~x)+
  ggtitle("Vote vs Growth") # plot title


# part d
ggplot(df_star, aes(y = vote, x = inflation )) +
  geom_point() + # add points to the plot
  ylab("Vote") + # x-axis label
  xlab("inflation ") + # y-axis label+ 
  # geom_smooth(method = "lm",formula =  y~x)+
  ggtitle("Vote vs inflation ") # plot title

model_vote_inflation <- lm(data = df_star, vote ~ inflation)
model_vote_inflation %>% summary()
