# important information ####

# please create a parent folder (name does not matter) with the following folder structure:

# data_tidy <- save csv data files here, raw files were already cleaned with excel
# output <- descriptive and result tables will be saved here

# the directory containing the two folders must be set as the current working directory or contain a here() file

# not all econometric results are saved in the output folder
# if you are interested in robustness and statistical assumption checks please run the code from chunk to chunk and read the commented results

# change settings #####

# set error message language to english
Sys.setenv(LANG = "en")


# install packages ####
# packages have to be installed only once - remove #'s in RStudio with -> Code -> Comment/Uncomment Lines

# install.packages("tidyverse")
# install.packages("here")
# install.packages("readxl")
# install.packages("ggplot2")
# install.packages("data.table")
# install.packages("magrittr")
# install.packages("countrycode")
# install.packages("plm")
# install.packages("stargazer")
# install.packages("lmtest")
# install.packages("sandwich")
# install.packages("tseries")
# install.packages("systemfit")
# install.packages("ggeffects")

# load packages #####

# packages need to be loaded for every new session
library(tidyverse)
library(here)
library(readxl)
library(ggplot2)
library(data.table)
library(magrittr)
library(countrycode)
library(plm)
library(stargazer)
library(lmtest)
library(sandwich)
library(tseries)
library(systemfit)
library(ggeffects)

# test if here.package works getwd() = here() -> path should link to parent folders containing the three mentioned folders

here()

# import and merge data sets into a tidy panel format ####

Data_Wages<-fread("Data_Tidy/Data_Wages.csv", colClasses = "double")
Data_Wages[, c(5:9):= NULL]
Data_Wages<-Data_Wages[!222]
Data_Wages = Data_Wages %<>%  
  mutate(Year = as.numeric(Year)) %>%
  mutate(Gross_Hourly_Wages = as.numeric(Gross_Hourly_Wages))

Data_Employees<-fread("Data_Tidy/Data_Employees.csv", colClasses = "double")

Data_Parttime_Share<-fread("Data_Tidy/Data_Part-time_Share.csv", colClasses = "double")
Data_Parttime_Share<-Data_Parttime_Share[, !6]
Data_Parttime_Share<-setnames(Data_Parttime_Share, "Econimic_Sector", "Economic_Sector")

Data_Union_Membership<-fread("Data_Tidy/Data_Union_Membership.csv", colClasses = "double")




Data = Reduce(function(y,x) merge(x = x, y = y, by = c("Year", "Economic_Sector"), all = TRUE),
                list(Data_Wages, Data_Employees, Data_Parttime_Share, Data_Union_Membership))

Data[, idcode := .GRP, by=.(Economic_Sector)]
Data = setcolorder(Data, c(14,1:13))
Data<-setnames(Data, 7, "Share_of_Parttime_Employees")




Data_Wages_EURO<-fread("Data_Tidy/Data_Wages_EURO.csv", colClasses = "double")
Data_Wages_EURO[,c(9:37):=NULL]
Data_Wages_EURO_df=setDF(Data_Wages_EURO)
Data_Wages_EURO_df[,3:4] = sapply(Data_Wages_EURO_df[,3:4],as.numeric)
Data_Wages_EURO=as.data.table(Data_Wages_EURO_df)

Data_test = Reduce(function(y,x) merge(x = x, y = y, by = c("Year", "Economic_Sector"), all = TRUE),
              list(Data, Data_Wages_EURO))



Data = Reduce(function(y,x) merge(x = x, y = y, by = c("Year", "Economic_Sector"), all = TRUE),
              list(Data_Wages, Data_Employees, Data_Parttime_Share, Data_Union_Membership, Data_Wages_EURO))

Data[, idcode := .GRP, by=.(Economic_Sector)]
Data = setcolorder(Data, c(20,1:19))
Data<-setnames(Data, 13, "Share_of_Parttime_Employees")

# create/transform variables (growth rates, logs...)
Data$Delta_Gross_Hourly_Wages <- with(Data, ave(Gross_Hourly_Wages , idcode, FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))
Data$Delta_Share_of_Female_Employees <- with(Data, ave(Share_of_Female_Employees , idcode, FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))
Data$Delta_Share_of_Parttime_Employees <- with(Data, ave(Share_of_Parttime_Employees , idcode, FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))
Data$Delta_Share_of_Collective_Agreement <- with(Data, ave(Share_of_Collective_Agreement , idcode, FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))
Data$Delta_Share_of_German_total_Employees <- with(Data, ave(Share_of_German_total_Employees , idcode, FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))
Data$Delta_Gross_Monthly_Wages <- with(Data, ave(Gross_Monthly_Wages , idcode, FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))
Data$Delta_EUR_Gross_Hourly_Wages <- with(Data, ave(EUR_Gross_Hourly_Wages , idcode, FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))
Data$Delta_Total_Employees <- with(Data, ave(Total_Employees , idcode, FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))
Data$Log_Gross_Hourly_Wages <- with(Data, ave(Gross_Hourly_Wages , idcode, FUN=function(x) c(NA, log(x)) ))
Data$Log_Share_of_Female_Employees <- with(Data, ave(Share_of_Female_Employees , idcode, FUN=function(x) c(NA, log(x)) ))
Data$Log_EUR_Gross_Monthly_Wages <- with(Data, ave(EUR_Gross_Monthly_Wages , idcode, FUN=function(x) c(NA, log(x)) ))
Data$Log_EUR_Gross_Hourly_Wages <- with(Data, ave(EUR_Gross_Hourly_Wages , idcode, FUN=function(x) c(NA, log(x)) ))

write.csv(Data, "data_tidy/Data_Tidy.csv", row.names = FALSE )

# descriptive statistics ####

# select variables
descriptive_data <- select(Data, "idcode", "Year", "EUR_Gross_Hourly_Wages", "EUR_Gross_Monthly_Wages","Male_Gross_Hourly_Wages",
                           "Female_Gross_Hourly_Wages", "Share_of_Female_Employees",   "Share_of_Parttime_Employees",
                           "Share_of_Collective_Agreement", "Total_Employees","Share_of_German_total_Employees")

# store selected variables
descriptive_data<-as.data.frame(descriptive_data)
# descriptive_data<-na.omit(descriptive_data)
descriptive_data[,1] = sapply(descriptive_data[,1],as.numeric)

# create and save descriptive statistics table in word format
stargazer(descriptive_data,
          digits=2,
          title="Descriptive Statistics",
          type = "html",
          out="output/descriptive.doc",
          
          
          covariate.labels = c("ID Code (Economic Sectors)", "Year", "Gross Hourly Wages in EUR", "Gross Monthly Wages in EUR", 
                               "Gross Male Hourly Wages in EUR", "Gross Female Wages in EUR", "Share of Female Employees in %", 
                               "Share of Part-Time Employees in %", "Share of Collective Agreement in %", "Total Employees", 
                               "Share of all German Employees in %") 
)

# show distribution
ggplot(Data, aes(x=Share_of_Female_Employees)) +
  geom_histogram()

ggplot(Data, aes(x=EUR_Gross_Hourly_Wages)) +
  geom_histogram()

ggplot(Data, aes(x=Log_EUR_Gross_Hourly_Wages)) +
  geom_histogram(bins=30)

# gauss-markov-/statistical assumption tests for panel and time series data ####

# tests for functional forms / prevention of model misspecification

# visualize possible functional forms
ggplot(Data, aes(x=Share_of_Female_Employees, y=EUR_Gross_Hourly_Wages)) + 
  geom_point() + 
  geom_smooth(aes(colour="linear", fill="linear"), 
              method="lm", 
              formula=y ~ x, ) + 
  geom_smooth(aes(colour="quadratic", fill="quadratic"), 
              method="lm", 
              formula=y ~ x + I(x^2)) + 
  geom_smooth(aes(colour="cubic", fill="cubic"), 
              method="lm", 
              formula=y ~ x + I(x^2) + I(x^3)) + 
  scale_fill_brewer(palette="Set1") + 
  scale_colour_brewer(palette="Set1") + 
  theme_classic() + 
  labs(colour="Functional Form", fill="Functional Form") +
  xlab("Share of Female Employees in %") + ylab("Gross Hourly Wages in EUR")

# tests for deterministic/stochastic trends, weak dependency, stationary process

# explanatory variable: Share of Female Employees in %

# show time trends
ggplot(Data, mapping = aes(x = Year, y = Share_of_Female_Employees)) +
  geom_line(aes(linetype = as.factor(idcode))) + 
  scale_fill_brewer(palette="Set1") + 
  scale_colour_brewer(palette="Set1") + 
  theme_classic() +
  xlab("Year") + ylab("Share of Female Employees in %") +
  scale_x_continuous(limits = c(2009,2019), breaks = seq(2009, 2019, by= 3), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,80), expand = c(0, 0)) +
  theme(legend.position = 'none')


ggplot(Data, mapping = aes(x = Year, y = Share_of_Female_Employees, group = idcode)) +    
  geom_line(color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
  scale_fill_brewer(palette="Set1") + 
  scale_colour_brewer(palette="Set1") + 
  theme_classic() +
  xlab("Year") + ylab("Share of Female Employees in %") +
  scale_x_continuous(limits = c(2008,2020), breaks = seq(2009, 2019, by= 3), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,80), expand = c(0, 0)) +
  theme(legend.position = 'none')


# no clear time trend visible

# Levin-Lin-Chu Unit-Root Test
# test requires a dataset where the tested variable is stored in different lists for each group
Data_without_NA<-Data[-which(is.na(Data$Share_of_Female_Employees)),]
Data_without_NA[,1] = sapply(Data_without_NA[,1],as.numeric)
Data_without_NA_split = data.frame(split(Data_without_NA$Share_of_Female_Employees, Data_without_NA$idcode))
#HO: there is unit root in variable or variable is non stationary
#H1: there is no unit root and variable is stationary 
purtest(Data_without_NA_split, pmax=2, exo = "intercept", test = "levinlin")
# stationarity can be confirmed 

# Augmented Dickey-Fuller Test
# test requires dataset without NA's
#H0: no stationary process
#H1: stationary process
#k defines number of lags tested
adf.test(Data_without_NA$Share_of_Female_Employees, k = 6)
# stationarity can be confirmed

# explained variable: Gross Hourly Wages in EUR

# show time trends
ggplot(Data, mapping = aes(x = Year, y = EUR_Gross_Hourly_Wages)) +
  geom_line(aes(linetype = as.factor(idcode)))
ggplot(Data, mapping = aes(x = Year, y = Log_EUR_Gross_Hourly_Wages)) +
  geom_line(aes(linetype = as.factor(idcode)))

ggplot(Data, mapping = aes(x = Year, y = EUR_Gross_Hourly_Wages, group = idcode)) +    
  geom_line(color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
  scale_fill_brewer(palette="Set1") + 
  scale_colour_brewer(palette="Set1") + 
  theme_classic() +
  xlab("Year") + ylab("Gross Hourly Wages in EUR") +
  scale_x_continuous(limits = c(2009,2019), breaks = seq(2009, 2019, by= 3), expand = c(0, 1)) +
  scale_y_continuous(limits = c(10,35), expand = c(0, 0)) +
  theme(legend.position = 'none')

ggplot(Data, mapping = aes(x = Year, y = Delta_EUR_Gross_Hourly_Wages, group = idcode)) +    
  geom_line(color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
  scale_fill_brewer(palette="Set1") + 
  scale_colour_brewer(palette="Set1") + 
  theme_classic() +
  xlab("Year") + ylab("Growth Rate of Gross Hourly Wages") +
  scale_x_continuous(limits = c(2009,2019), breaks = seq(2009, 2019, by= 3), expand = c(0, 1)) +
  scale_y_continuous(limits = c(-0.1,0.1), expand = c(0, 0)) +
  theme(legend.position = 'none')

# the explanatory variable has a deterministic trend
# try Delta/Growth Rate transformation to fix deterministic trend
ggplot(Data, mapping = aes(x = Year, y = Delta_EUR_Gross_Hourly_Wages)) +
  geom_line(aes(linetype = as.factor(idcode)))
# growth rate transformation fixes the time trend and creates random walk like behavior
# alternatively we can control for the time trend with a Trend variable 

# Levin-Lin-Chu Unit-Root Test
# test requires a dataset where the tested variable is stored in different lists for each group
Data_without_NA<-Data[-which(is.na(Data$EUR_Gross_Hourly_Wages)),]
Data_without_NA[,1] = sapply(Data_without_NA[,1],as.numeric)
Data_without_NA_split = data.frame(split(Data_without_NA$EUR_Gross_Hourly_Wages, Data_without_NA$idcode))
#HO: there is unit root in variable or variable is non stationary
#H1: there is no unit root and variable is stationary 
purtest(Data_without_NA_split, pmax=2, exo = "intercept", test = "levinlin")
# stationarity can be confirmed for Household Savings
# test confirms unit root from graphical analysis

# repeat test for growth rate transformation
# Levin-Lin-Chu Unit-Root Test
# test requires a dataset where the tested variable is stored in different lists for each group
Data_without_NA<-Data[-which(is.na(Data$Delta_EUR_Gross_Hourly_Wages)),]
Data_without_NA[,1] = sapply(Data_without_NA[,1],as.numeric)
Data_without_NA_split = data.frame(split(Data_without_NA$Delta_EUR_Gross_Hourly_Wages, Data_without_NA$idcode))
#HO: there is unit root in variable or variable is non stationary
#H1: there is no unit root and variable is stationary 
purtest(Data_without_NA_split, pmax=2, exo = "intercept", test = "levinlin")
# stationarity can be confirmed for Household Savings
# transformation solves stationarity issues

# Augmented Dickey-Fuller Test
# test requires dataset without NA's
#H0: no stationary process
#H1: stationary process
#k defines number of lags tested
adf.test(Data_without_NA$Delta_EUR_Gross_Hourly_Wages, k = 6)
# stationarity can be confirmed

# tests for serial correlation / autocorrelation

#estimate model for test input
fe_linear <- plm(Delta_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
                   Share_of_Collective_Agreement  + Total_Employees, data=Data, model = "within")

fe_linear <- plm(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
                   Share_of_Collective_Agreement  + Total_Employees, data=Data, model = "within")

Trend=seq_along(Data$Log_EUR_Gross_Hourly_Wages)
fe_linear <- plm(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
                   Share_of_Collective_Agreement  + Total_Employees + Trend, data=Data, model = "within")

#Durbin Watson test
#H0: There is no autocorrelation / serial correlation in error term.
#H1: There is autocorrelation / serial correlation in error term.
pdwtest(Delta_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
          Share_of_Collective_Agreement  + Total_Employees, data=Data, model = "within")
# no autocorrelation in growth rate transformation 
pdwtest(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
          Share_of_Collective_Agreement  + Total_Employees, data=Data, model = "within")
pdwtest(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
          Share_of_Collective_Agreement  + Total_Employees + Trend, data=Data, model = "within")
# autocorrelation confirmed in log transformation
# no autocorrelation in growth rate transformation and Trend variable control model

#wooldridge test
#H0: There is no autocorrelation / serial correlation in error term.
#H1: There is autocorrelation / serial correlation in error term.
fe_linear <- plm(Delta_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
                   Share_of_Collective_Agreement  + Total_Employees, data=Data, model = "within")
pbgtest(fe_linear)
# no autocorrelation in growth rate transformation 
fe_linear <- plm(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
                   Share_of_Collective_Agreement  + Total_Employees, data=Data, model = "within")
pbgtest(fe_linear)
# autocorrelation confirmed in log transformation
fe_linear<-plm(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
          Share_of_Collective_Agreement  + Total_Employees + Trend, data=Data, model = "within")
pbgtest(fe_linear)

# both tests confirm autocorrelation in log transformation and reject autocorrelation in growth rate transformation and Time trend model

# tests for homoskedasticity / heteroskedasticity

#Breusch-Pagan Test
#HO: There is homoskedasticity
#H1: There is heteroskedasticity
bptest(Delta_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
         Share_of_Collective_Agreement  + Total_Employees, data=Data, studentize = F)
bptest(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
         Share_of_Collective_Agreement  + Total_Employees, data=Data, studentize = F)
bptest(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
         Share_of_Collective_Agreement  + Total_Employees + Trend, data=Data, studentize = F)
# test confirms heteroskedasticity for both models

# robust standard errors are necessary

#Robust Standard Erros for heteroskedasticity and autocorrelation
#if there is heteroscedasticity
coeftest(fe_linear, vcovHC)
#if there is heteroskedascticity and autocorrelation
coeftest(fe_linear, vcovHC(fe_linear, method = "arellano"))

# tests for multicollinearity 

# variance inflation factor function needs pooled ols model as input for panel data test
pooled_linear <- plm(Delta_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
                       Share_of_Collective_Agreement  + Total_Employees, data=Data, model = "pooling")
vif(pooled_linear)

pooled_linear <- plm(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
                       Share_of_Collective_Agreement  + Total_Employees, data=Data, model = "pooling")
vif(pooled_linear)

pooled_linear <- plm(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
                       Share_of_Collective_Agreement  + Total_Employees + Trend, data=Data, model = "pooling")
vif(pooled_linear)
# low variance inflation factor (no factor over 5) in growth rate and log transformation
# medium variance inflation factor (6,7) in time trend model

#Tests for cross sectional dependence in Panel data
# problem for macro panels with long time series

#Breusch-Pagan LM test for cross sectional dependence
#H0: there is no cross sectional dependence
#H1: there is cross sectional dependence
#pcdtest(fe_linear, test = c("lm"))

#Pesaran CD test for cross sectional dependence
#H0: there is no cross sectional dependence
#H1: there is cross sectional dependence
#pcdtest(fe_linear, test = c("cd"))

# test model with cross sectional dependence robust standard errors
# PCSE if N>T
#summary(fe_linear, vcov = function(x) vcovBK(x, type="HC1", cluster = c("group")))

# FGLS if T>N
#test<-pggls(Delta_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + Share_of_Collective_Agreement  + Total_Employees, data=Data, model = "within")
#summary(test)

# no cross sectional dependence issues expected due to short time series

# model comparison tests ####

# the following estimations are needed as inputs

Trend=seq_along(Data$Log_EUR_Gross_Hourly_Wages)
fe_linear <- plm(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
                   Share_of_Collective_Agreement  + Total_Employees + Trend, data=Data, model = "within")
re_linear <- plm(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
                   Share_of_Collective_Agreement  + Total_Employees + Trend, data=Data, model = "random")
pooled_linear <- plm(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
                       Share_of_Collective_Agreement  + Total_Employees + Trend, data=Data, model = "pooling")     


#Hausman test
#H0: random effect model is consistent
#H1: fixed effect model is consistent

phtest(fe_linear,re_linear)   

#pooled ols test
#HO: the same coefficients apply to each individual (pooled ols is stable)
#h1: the same coefficients do not apply to each individual (pooled ols is unstable)

pooltest(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
           Share_of_Collective_Agreement  + Total_Employees + Trend, data = Data,  model = "within")

#test on consistency between pooled ols and fixed effects model
#H0: pooled ols is consistent
#H1: fixed effect model is consistent
pFtest(fe_linear, pooled_linear)

#test for individual effects and time effects
#H0: no significant individual effects and time effects
#H1: significant individual effects and time effects

plmtest(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
          Share_of_Collective_Agreement  + Total_Employees + Trend, data=Data,
        effect = "twoways", type="ghm")

#test for individual effects
#H0: no significant individual effects
#H1: significant individual effects

plmtest(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
          Share_of_Collective_Agreement  + Total_Employees + Trend, data=Data,
        effect = "individual", type="honda")

#test for time effects
#H0: no significant time effects
#H1: significant time effects

plmtest(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
          Share_of_Collective_Agreement  + Total_Employees + Trend, data=Data,
        effect = "time", type="honda")

# all test results point towards a fixed effects model with group and time effects

fe<- plm(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + 
           Share_of_Collective_Agreement  + Total_Employees + Trend, data=Data, model = "within", effect = "twoways")
summary(fe)

fe<- plm(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
           Share_of_Collective_Agreement  + Total_Employees + Trend, data=Data, model = "within", effect = "individual")
summary(fe)


# Panel-Estimations ####

# coeftest(fe_quadratic, vcovHC(fe_quadratic,method = c("white1"),type = c("HC0"), cluster = c("group")))

#trend
Trend=seq_along(Data$Log_EUR_Gross_Hourly_Wages)

# 1 linear fixed effects estimation
fe_linear <- plm(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + Share_of_Parttime_Employees + 
                       Share_of_Collective_Agreement  + Total_Employees + Trend,
                 data = Data, model = "within")
# saving robust standard errors for output table
fe_linear_se <- vcovHC(fe_linear,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# 2 quadratic fixed effects estimation
fe_quadratic <- plm(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + I(Share_of_Female_Employees^2) + Share_of_Parttime_Employees + 
                       Share_of_Collective_Agreement  + Total_Employees + Trend,
                     data = Data, model = "within")
fe_quadratic_se <- vcovHC(fe_quadratic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# 3 fixed effects estimation with both individual and time fixed effects
fe_cubic <- plm(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + I(Share_of_Female_Employees^2) + I(Share_of_Female_Employees^3)+ Share_of_Parttime_Employees + 
                   Share_of_Collective_Agreement  + Total_Employees + Trend,
                data = Data, model = "within")
# saving robust standard errors for output table
fe_cubic_se <- vcovHC(fe_cubic,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# 4 quadratic lag fixed effects estimation
fe_quadratic_lag <- plm(Log_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + I(Share_of_Female_Employees^2) + lag(Share_of_Female_Employees)+ Share_of_Parttime_Employees + 
                      Share_of_Collective_Agreement  + Total_Employees + Trend,
                    data = Data, model = "within")
fe_quadratic_lag_se <- vcovHC(fe_quadratic_lag,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# 5 growth rate fixed effects estimation
fe_delta <- plm(Delta_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + I(Share_of_Female_Employees^2) + Share_of_Parttime_Employees + 
                  Share_of_Collective_Agreement  + Total_Employees,
                     data = Data, model = "within")
# saving robust standard errors for output table
fe_delta_se <- vcovHC(fe_delta,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# 6 dynamic growth rate fixed effects estimation
fe_delta_twoways <- plm(Delta_EUR_Gross_Hourly_Wages~Share_of_Female_Employees + I(Share_of_Female_Employees^2) + Share_of_Parttime_Employees + 
                  Share_of_Collective_Agreement  + Total_Employees,
                data = Data, model = "within", effect = "twoways")
# saving robust standard errors for output table
fe_delta_twoways_se <- vcovHC(fe_delta_twoways,method = c("white1"),type = c("HC0"), cluster = c("group")) %>% diag() %>% sqrt()

# create and save output table in word

stargazer(
  
  fe_linear, fe_quadratic, fe_cubic, fe_quadratic_lag, fe_delta, fe_delta_twoways, # choose models
  
  se = list(fe_linear_se, fe_quadratic_se, fe_cubic_se, 
            fe_quadratic_lag_se, fe_delta_se, fe_delta_twoways_se), # insert robust standard errors for all models
  
  type = 'html', # define table type
  
  title = "Fixed Effects Estimations", # name table
  
  out="output/regression_table.doc", # define save path for word document starting from r project directory
  
  single.row = TRUE, # to put coefficients and standard errors on same line
  
  no.space = TRUE, # to remove the spaces after each line of coefficients
  
  column.sep.width = "3pt", # to reduce column width
  
  font.size = "small", # to make font size smaller
  
  column.labels=c("Linear Individual FE", "Quadratic Individual FE", "Cubic Individual FE", "Lag Quadratic Individual FE", "Quadratic Individual FE", "Quadratic Time and Individual FE"), # name columns with model types
  
  covariate.labels = c("Share of Female Employees in %", "Share of Female Employees in % Quadratic", "Share of Female Employees in % Cubic", "Lag(Share of Female Employees in %)", "Share of Part-time Employees in %", "Share of Collective Agreement in %", "Total Employees", "Trend"), # name variables starting with  model containing the most variables
  
  dep.var.labels = c("Log of Gross Hourly Wages", "Growth Rate of Gross Hourly Wages"),
  
  notes = "Robust standard errors in parentheses: .p<0.1*p<0.05**p<0.01***p<0.001", # include note
  
  notes.append = FALSE, # turn off additional line break for notes
  
  notes.align = "l" # align notes in one row# name dependent variable    
)


# code end - check output folder for results ####

#tukey anscombe plot
ggplot(
  data.frame(
    GefitteteWerte= fitted.values(fe_linear),
    Residuen= residuals(fe_linear)
  ),
  aes(x=GefitteteWerte, y=Residuen)
) +
  ggtitle("Tukey-Anscombe-Plot") +
  geom_hline(yintercept = 0) + 
  geom_point()

# qqplot
ggplot(
  data.frame(
    GefitteteWerte= fitted.values(fe_linear),
    Residuen= residuals(fe_linear)
  ),
  aes(sample=Residuen)
) +
  stat_qq() + stat_qq_line()+
  labs(title="Q-Q-Plot", x = "Theoretical Quantiles", y="Residuals")

ggplot(fe_linear, aes(sample=residuals)) +
  stat_qq() + stat_qq_line()


coeftest(fe_individual, vcovHC(fe_individual,method = c("white1"),type = c("HC0"), cluster = c("group")))
