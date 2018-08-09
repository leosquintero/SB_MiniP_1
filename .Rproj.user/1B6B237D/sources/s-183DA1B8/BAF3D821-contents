#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

setwd("F:/Carpetas/R/SB_MiniP_1/linear_regression/dataSets/")

##   You might also start by listing the files in your working directory

getwd() # where am I?
list.files("F:/Carpetas/R/SB_MiniP_1/linear_regression/dataSets/") # files in the dataSets folder

## Load the states data
## ────────────────────────

# read the states data
states.data <- readRDS("F:/Carpetas/R/SB_MiniP_1/linear_regression/dataSets/states.rds")
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat.mod)
# hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to

states <- readRDS("states.rds")

##   1. Examine/plot the data before fitting the model
states_en_me <- subset(states, select = c("energy", "metro"))
summary(states_en_me)
cor(states_en_me, use = "pairwise")
plot(states_en_me)

##   2. Print and interpret the model `summary'

model1_en_me <-  lm(energy ~ metro, data = states)
summary(model1_en_me)

##   3. `plot' the model to look for deviations from modeling assumptions
plot(model1_en_me)

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. 
st_mod_test3 <- subset(states, select = c("energy", "metro", "income", 
                                          "percent"))
summary(st_mod_test3)
cor(st_mod_test3, use = "pairwise")
plot(st_mod_test3)

st_mod_11 <- lm(energy ~ metro + income + percent, data = states.data)
summary(st_mod_11)

#Below I'm exoerimenting with other variations
st_mod_test <- subset(states, select = c("pop", "area", "density", "metro", 
                                         "waste", "energy", "miles", "toxic", 
  "green", "house", "senate", "csat", "vsat", "msat", "percent", "expense", 
  "income", "high", "college"))
summary(st_mod_test)
cor(st_mod_test, use = "pairwise")
plot(st_mod_test)

st_mod_2 <- lm(metro ~ waste + energy + house + miles + green + income + 
                 percent + expense + toxic + density + senate + college,
               data = states)
summary(st_mod_2)

st_mod_3 <- lm(income ~ density + house + percent + metro + senate + energy 
               + miles, data = states.data)
summary(st_mod_3)

st_mod_4 <- lm(income ~ percent + metro + energy, data = states.data)
summary(st_mod_4)

st_mod_5 <- lm(income ~ percent + metro, data = states.data)
summary(st_mod_5)

st_mod_6 <- lm(income ~ percent + metro + energy, data = states.data)
summary(st_mod_6)

st_mod_7 <- lm(income ~ percent + metro + miles, data = states.data)
summary(st_mod_7)

st_mod_8 <- lm(income ~ percent + metro + energy + miles, data = states.data)
summary(st_mod_8)

st_mod_9 <- lm(income ~ percent + metro + energy + csat + vsat, 
               data = states.data)
summary(st_mod_9)

# Model 10 shows the biggest correlation among variables
st_mod_test2 <- subset(states, select = c("percent", "metro", "energy", 
                                          "income", "csat", "vsat"))
summary(st_mod_test2)
cor(st_mod_test2, use = "pairwise")
plot(st_mod_test2)

st_mod_10 <- lm(percent ~ metro + income + energy + csat + vsat, 
                data = states.data)
summary(st_mod_10)

#Is this model significantly better than the model
##   with /metro/ as the only predictor?

# Answer = The model st_mod_11 is better but not necesarilly the best one 
#since the difference in Multiple R-squared and	Adjusted R-squared among both 
#models is only of 0.20 and 0.14 respectivelly.
#In the other hand, st_mod_10 shows a bigger corelation among variables, 
#resulting in Multiple R-squared equal to 0.8859 and an	Adjusted R-squared
# equal to  0.8729.
# 

## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

#Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.
st_mod_interac <- lm(energy ~ metro + income + percent + expense*csat, 
                     data = states)
coef(summary(st_mod_11))
anova(st_mod_interac)

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

states$region <- factor(states$region)
st_mod_interac_reg <- lm(energy ~ metro + income + percent + expense*csat + 
                           region, data = states)
summary(st_mod_interac_reg)
coef(summary(st_mod_interac_reg))

