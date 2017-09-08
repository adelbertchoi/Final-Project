# remove objects from global environment
rm(list = ls())

# set working directory
setwd("/Users/adelbertchoi/Google Drive/RMIT/2017 Semester2/MATH1307/Final Project")

# directory files
dir()

# load necessary packages
# install.packages("pacman")
pacman::p_load(readxl, dplyr, TSA)

# read the datasets
# using read_excel returns a tibble, function only works with data frames
data_year <- read_excel(path = "M3C_reduced.xlsx", sheet = 1) %>% as.data.frame()
data_quarter <- read_excel(path = "M3C_reduced.xlsx", sheet = 2) %>% as.data.frame()
data_month <- read_excel(path = "M3C_reduced.xlsx", sheet = 3) %>% as.data.frame()

##################################################################
# data reading functions
# returns a list consisting of all the data
read_data <- function(data = data, freq = 1) {
    N <- nrow(data)     # number of datasets
    data_list <- list() # create a list
    
    for(i in 1:N) {
        # time series specifications
        series_no <- data[i, 1]
        series_size <- data[i, 2]
        category <- data[i, 4]
        series_start <- data[i, 5]
        
        if (freq > 1)
            starting_season <- data[i, 6]
        
        # a vector for the time series
        series_data <- NULL
        for(j in 7:(7+series_size-1)) {
            # append data into the vector
            series_data <- c(series_data, data[i, j]) 
        }
        
        # get training (95%) and test data (5%)
        train <- series_data[1:floor((0.95*series_size))]
        test <- series_data[ceiling((0.95*series_size)):series_size]
        
        # data to ts object
        if (freq == 1){
            series_ts <- ts(series_data, start = series_start, frequency = freq) 
            train_data <- ts(train, start = series_start, frequency = freq) 
            series_name <- paste("Series", series_no, ":",  category, "from", series_start, 
                                 "to", series_start + series_size - 1, sep = " ")  
        }
        if (freq > 1) {
            series_ts <- ts(series_data, start = c(series_start, starting_season), frequency = freq) 
            train_data <- ts(train, start = c(series_start, starting_season), frequency = freq) 
            series_name <- paste("Series", series_no, ":",  category, "from", series_start, 
                                 as.vector(season(series_ts))[1], "to", floor(time(series_ts)[series_size]), 
                                 as.vector(season(series_ts))[series_size], sep = " ")  
        }
        
        # append data to list
        data_list[[series_name]] <- list(data = series_ts, 
                                         train_data = train_data, 
                                         test_data = test)
    }
    
    return(data_list) # return data list
}

# obtain yearly data
yearly_data <- read_data(data_year)
yearly_data$`Series 1 : MICRO from 1975 to 1994`$data # testing
yearly_data$`Series 1 : MICRO from 1975 to 1994`$train_data # testing

# obtain quarterly data
quarterly_data <- read_data(data_quarter, freq = 4)
quarterly_data$`Series 102 : MICRO from 1984 1Q to 1994 4Q`$train_data # testing

# obtain monthly data
monthly_data <- read_data(data_month, freq = 12)
monthly_data$`Series 203 : MICRO from 1990 January to 1995 August`$data # testing

##################################################################
# trial expert system

sample_data <- yearly_data$`Series 1 : MICRO from 1975 to 1994`

trend_models <- expand.grid(trends = c("AAN", "MAN", "AMN", "MMN"), 
                            damped = c(TRUE, FALSE))
model_list <- cbind(models = c("AAdN", "MAdN", "AMdN", "MMdN", "AAN", "MAN", "AMN", "MMN"), 
                    trend_models)

fit.aicc <- rep(NA, 8)
fit.bic <- rep(NA, 8)
fit.aic <- rep(NA, 8)
fit.mase <- rep(NA, 8)

for(i in 1:length(yearly_data)) {
    
}