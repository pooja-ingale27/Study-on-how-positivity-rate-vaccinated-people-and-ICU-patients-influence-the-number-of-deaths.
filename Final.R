###Installing Packages###
install.packages('tidyr', repos = "http://cran.us.r-project.org")
install.packages("corrplot" , repos = "http://cran.us.r-project.org")


###Loading the library###
library(tidyverse)
library(tidyr)
library(dplyr)
library(corrplot)


###Read the Data set in variable###
dataset<-read.csv(file = "owid-covid-data.csv")
View(dataset)
count(dataset)


###Filtering  data for United Kingdom###
dataset<-filter(dataset,location =="United Kingdom")
View(dataset)
count(dataset)


###Selecting required variable columns###
uk_dataset<-dplyr::select(dataset,
                          continent,
                          location,
                          date,
                          new_deaths_smoothed,
                          positive_rate,
                          people_vaccinated,
                          icu_patients)
View(uk_dataset)


###Selecting columns with missing values###
list_na <- colnames(uk_dataset)[ apply(uk_dataset, 2, anyNA) ]
list_na


###Average value for columns with missing values###
options(scipen = 999)
average_missing <- apply(uk_dataset[,colnames(uk_dataset) %in% list_na],
                         2,
                         mean,
                         na.rm =  TRUE)
average_missing


###Filling up missing data by creating new column###
##New columns with filled missing data##
uk_dataset_replace <- uk_dataset %>%
  mutate(new_deaths_smoothed_replace  = ifelse(is.na(new_deaths_smoothed), average_missing[1], new_deaths_smoothed),
         positive_rate_replace = ifelse(is.na(positive_rate), average_missing[2], positive_rate),
         people_vaccinated_replace  = ifelse(is.na(people_vaccinated), average_missing[3], people_vaccinated),
         icu_patients_replace  = ifelse(is.na(icu_patients), average_missing[4], icu_patients))

View(uk_dataset_replace)


###Splitting of Data###
##Using 70% of data set as training set and remaining 30% as testing set##
set.seed(3)
sample <- sample(c(TRUE, FALSE), nrow(uk_dataset_replace), replace=TRUE, prob=c(0.7,0.3))
uk_dataset_replace_train  <- uk_dataset_replace[sample, ]
uk_dataset_replace_test   <- uk_dataset_replace[!sample, ]
count(uk_dataset_replace_train)
count(uk_dataset_replace_test)


###Finding Correlations Between Dependent variable and Indepdendent variables by point graph and cor test###
## For independent variable - positive_rate_replace##
#Point Graph#
ggplot(
  data=uk_dataset_replace_train,
  aes(x=positive_rate_replace, y=new_deaths_smoothed_replace)
) + geom_point()+
  labs(x = "Positivity rate", y ="New Deaths", title = 'Correlation of Deaths with Positivity Rate')

#Correlation test#
cor.test(
  uk_dataset_replace_train$positive_rate_replace,
  uk_dataset_replace_train$new_deaths_smoothed_replace
)


## For independent variable - people_vaccinated_replace##
#Point Graph#
ggplot(
  data=uk_dataset_replace_train,
  aes(x=people_vaccinated_replace, y=new_deaths_smoothed_replace)
) + geom_point()+
  labs(x = "Vaccinated People", y ="New Deaths" , title = 'Correlation of Deaths with Number of people Vaccinated ')

#Correlation test#
cor.test(
  uk_dataset_replace_train$people_vaccinated_replace,
  uk_dataset_replace_train$new_deaths_smoothed_replace
)


## For independent variable - icu_patients_replace##
#Point Graph#
ggplot(
  data=uk_dataset_replace_train,
  aes(x=icu_patients_replace, y=new_deaths_smoothed_replace)
) + geom_point()+
  labs(x = "ICU Patients", y ="New Deaths", title = 'Correlation of Deaths with Number of ICU Patients ' )

#Correlation test#
cor.test(
  uk_dataset_replace_train$icu_patients_replace,
  uk_dataset_replace_train$new_deaths_smoothed_replace
)


###Data Visualization###
##Selecting only numeric columns## 
uk_dataset_replace_train_new<-dplyr::select(uk_dataset_replace_train,
                                            new_deaths_smoothed_replace,
                                            positive_rate_replace,
                                            people_vaccinated_replace,
                                            icu_patients_replace)

##renaming variables##
rep1<-uk_dataset_replace_train_new %>% dplyr::rename( Deaths = new_deaths_smoothed_replace,
                            Positivity = positive_rate_replace,
                            Vaccine = people_vaccinated_replace,
                            ICU= icu_patients_replace,
)

## Plotting a correlation Matrix graph##
corrplot(cor(rep1),        
         method = "shade", 
         type = "lower",    
         tl.col = "black",
         bg = "white",     
         col = NULL)       


## Plotting a correlation Matrix Scatterplot##
library(PerformanceAnalytics)
chart.Correlation(rep1, histogram = TRUE, method = "pearson")

  ###Fitting Multivariate Regression###
mod_uk <- lm(
  formula=new_deaths_smoothed_replace~positive_rate_replace+people_vaccinated_replace+icu_patients_replace, 
  data=uk_dataset_replace_train)
options(scipen = 999)  
summary(mod_uk)


#coefs gives the coefficients of the model i.e b,a1,a2,a3.
coefs_uk<-coef(mod_uk)
coefs_uk

###Plotting regression line on the graph of the data points###
##For independent variable - Positivity rate##
ggplot(
  data=uk_dataset_replace_train,
  aes(x=positive_rate_replace, y=new_deaths_smoothed_replace)
) +
  geom_point() +
  geom_abline(mapping=aes(
    slope=coefs_uk["positive_rate_replace"],
    intercept=coefs_uk["(Intercept)"]
  ), color='red')+
  labs(x = "Positivity rate", y ="New Deaths", title = 'Correlation of Deaths with Positivity Rate')

##For independent variable - people_vaccinated_replace##
ggplot(
  data=uk_dataset_replace_train,
  aes(x=people_vaccinated_replace, y=new_deaths_smoothed_replace)
) +
  geom_point() +
  geom_abline(mapping=aes(
    slope=coefs_uk["people_vaccinated_replace"],
    intercept=coefs_uk["(Intercept)"]
  ), color='red')+
  labs(x = "Vaccinated People", y ="New Deaths" , title = 'Correlation of Deaths with Number of people Vaccinated ')


##For independent variable - icu_patients_replace##
ggplot(
  data=uk_dataset_replace_train,
  aes(x=icu_patients_replace, y=new_deaths_smoothed_replace)
) +
  geom_point() +
  geom_abline(mapping=aes(
    slope=coefs_uk["icu_patients_replace"],
    intercept=coefs_uk["(Intercept)"]
  ), color='red')+
  labs(x = "ICU Patients", y ="New Deaths" , title = 'Correlation of Deaths with Number of ICU Patients ')

###Calculate the predicted values and the residuals for each input sample using the predict() and 
#residuals() functions in R###
#Residuals.
uk_resid<-uk_dataset_replace_train
uk_resid$residuals<-residuals(mod_uk)
uk_resid$predicted<-predict(mod_uk)
View(uk_resid)

###Residuals on graph###.
##For Independent variable - people_vaccinated_replace##
ggplot(
  data=uk_resid,
  aes(x=people_vaccinated_replace,y=new_deaths_smoothed_replace)
) + labs(x= "Number of people Vaccinated" , y = "Number of Deaths")+
  geom_point(size=3) +
  geom_point(size=2, aes(y=predicted), shape=1) +
  geom_segment(aes(xend=people_vaccinated_replace, yend=predicted), alpha=0.9, color='red') +
  geom_abline(mapping=aes(
    slope=coefs_uk["people_vaccinated_replace"],
    intercept=coefs_uk["(Intercept)"]
  ), color='blue')

##For Independent variable - Positivity rates##
ggplot(
  data=uk_resid,
  aes(x=positive_rate_replace,y=new_deaths_smoothed_replace)
) + labs(x= "Positivity rates" , y = "Number of Deaths")+
  geom_point(size=3) +
  geom_point(size=2, aes(y=predicted), shape=1) +
  geom_segment(aes(xend=positive_rate_replace, yend=predicted), alpha=0.9, color='red') +
  geom_abline(mapping=aes(
    slope=coefs_uk["positive_rate_replace"],
    intercept=coefs_uk["(Intercept)"]
  ), color='blue')

##For Independent variable - ICU Patients##
ggplot(
  data=uk_resid,
  aes(x=icu_patients_replace,y=new_deaths_smoothed_replace)
) + labs(x= "ICU Patients" , y = "Number of Deaths")+
  geom_point(size=3) +
  geom_point(size=2, aes(y=predicted), shape=1) +
  geom_segment(aes(xend=icu_patients_replace, yend=predicted), alpha=0.9, color='red') +
  geom_abline(mapping=aes(
    slope=coefs_uk["icu_patients_replace"],
    intercept=coefs_uk["(Intercept)"]
  ), color='blue')

###Making prediction###
uk_predict<-predict(mod_uk, uk_dataset_replace_test)
uk_predict
View(uk_predict)
summary(uk_predict)

###Finding accuracy for test data###
##Root mean squared error##
rmse_val <- sqrt(mean(uk_predict-uk_dataset_replace_test$new_deaths_smoothed_replace)^2)
rmse_val

##Sum of Squares Error##
SSE = sum((uk_predict-uk_dataset_replace_test$new_deaths_smoothed_replace)^2)
print(SSE)

##Sum of Squares Regression##
SSR <- sum((uk_predict - mean(uk_dataset_replace_test$new_deaths_smoothed_replace))^2)
SSR

##Sum of Squares Total##
SST = SSR - SSE
SST

##R squared##
r2_test = 1 - SSE/SST
print(r2_test)

