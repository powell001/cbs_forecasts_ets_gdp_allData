library(tidyverse)
library(xts)
library(zoo)
library(svglite)
library(TSstudio)
library(zoo)
library(dlm)
library(forecast)
library(expsmooth)
library(ggplot2)
#library(ggfortify)
library(changepoint)
library(KFAS)
library(httpgd)
library(funtimes)
library(seastests)
library(car)
library(lmtest)

##############################
# Possible analyses
##############################

# unprocessed data
rawDataFile <- "data/cbs_basic_macro_allData_qt.csv"

###
# Combine all forecasts
###

files <- list.files("output/forecasts" , pattern = "final_forecasts.*\\.csv$", full.names = TRUE)
initial_df <- read.csv(files[1])

# Merge data frames using a loop
for (fl in files[2:length(files)]) {
  file_df <- read.csv(fl)
  initial_df <- rbind(initial_df, file_df)
}

# View the merged data frame
print(initial_df)
write.table(initial_df, file = "output/analyses/combined_final_forecasts.csv", sep =",",row.names = FALSE)

###
# Combine point forecast + entire series 
###

files_Raw <- list.files("output/forecasts" , pattern = "RawData.*\\.csv$", full.names = TRUE)
files_FinalForecasts <- list.files("output/forecasts" , pattern = "final_forecasts.*\\.csv$", full.names = TRUE)

###################################
###################################
###################################


######################
# Using biggest changers in Percentage Change, rank list of biggest changes last quarter, remove if missing
######################

fun_remove_Totaals <- function(rawDataFile){

    print("REMOVE TOTAALS")

    df1 <- read.csv(rawDataFile, stringsAsFactors=FALSE)
    # dont understand those Totaal columns yet, remove them
    df1 <- df1 %>% select(-contains('Totaal'))

    write.table(df1, file = "data/cbs_basic_macro_allData_qt.csv", sep =",",row.names = FALSE)
 }    

fun_remove_Totaals(rawDataFile)

fun_bigchanges_absolute_percent <- function(rawDataFile){
    
    bigchangers_df <- read.csv(rawDataFile, stringsAsFactors=FALSE)

    # create row names from list of variables, then drop variables so all columns numeric
    rownames(bigchangers_df) <- bigchangers_df[,1] 
    bigchangers_df <- bigchangers_df[,-1]

    # dont understand those Totaal columns yet, remove them
    bigchangers_df <- bigchangers_df %>% select(-contains('Totaal'))

    # these are the absolute differences from four years previous
    df1 <- data.frame(diff(as.matrix(bigchangers_df),4))
    absoluteDiff_lastRow <- t(tail(df1,1))

     # divide last column by column 4 periods before and divide by that same column
    percent_YearBefore <- t(100*(absoluteDiff_lastRow/df_yearBefore))

    # merge the two dataframes
    mrg1 <- merge(percent_YearBefore, absoluteDiff_lastRow, by = 'row.names', all = TRUE)

    # order, sort data
    lastCol <- tail(names(mrg1),1)
    o <- order(abs(mrg1[,lastCol]), decreasing = TRUE)
    output1 <- mrg1[o, ]#[c(1,ncol(mrg1))]

    write.table(output1, file = "output/analyses/real_changes_yearBefore.csv", sep =",",row.names = FALSE)
}    

fun_bigchanges_absolute_percent(rawDataFile)


fun_make_figures <- function(raw_data){
    index <- 0
    for(fl in raw_data) {

        index <- index + 1

        print(index)

        # get raw data
        firstfile <- raw_data[index]
        initial_df_raw <- read.csv(firstfile)
        tmp_raw <- initial_df_raw[,c(4,5)]

        series_name <- initial_df_raw[,c(1)][1]

        # get final forecasts
        firstfile <- files_FinalForecasts[index]
        initial_df_finalfore <- read.csv(firstfile)
        tmp_forecast <- initial_df_finalfore[,c(1,2)]

        # copy column names
        colnames(tmp_forecast) <- colnames(tmp_raw)
        combined <- rbind(tmp_raw, tmp_forecast)

        # colors for different parts of line
        combined$mycolors <- c(rep('hist', length(combined[,1])-2), rep(c('forecast'),2))
        combined$mycolors <- as.factor(combined$mycolors)
        combined$ObservationDate <- as.Date(as.yearqtr(combined$ObservationDate), frac = 0)

        png(filename=paste("output/figures/", series_name, "series_and_forecast.png", sep = "_"))
        print(ggplot(combined, aes(x = ObservationDate, y = RawData, col = mycolors, group = 1)) + geom_line())
        dev.off()

        ###
        # Combine point forecast + entire series (using data above)
        ###
        combined$seriesDifferenced <- combined['RawData'] - lag(combined['RawData'], 4)
        combined$seriesDifferenced <- unlist(combined$seriesDifferenced)

        png(filename=paste("output/figures/", series_name, "differenced_forecasts.png", sep = "_"))
        plot(ts(combined[,c(2,4)], frequency = 4, start=c(1995,1)), main=series_name)
        dev.off()
    }
}

fun_make_figures(files_Raw)

######################
# Type of model for each analysis
######################

fun_ETS_Used <- function(){

    files_Raw  <- list.files("output/forecasts" , pattern = "RawData.*\\.csv$", full.names = TRUE)
    initial_df <- read.csv(files_Raw[1])[1,c(3,1)]

    # Merge data frames using a loop

    for (fl in files_Raw[2:length(files_Raw)]) {
    file_df <- read.csv(fl)
    initial_df <- rbind(initial_df, file_df[1,c(3,1)])
    } 

    write.table(initial_df, file = "output/analyses/combined_model_used.csv", sep =",",row.names = FALSE)
}

fun_ETS_Used()

######################
# Biggest change since a year before, based on Forecasts
######################

fun_bigchanges <- function(){
    mylist <- list()
    index <- 0
    for(fl in files_Raw) {

        index <- index + 1

        print(index)

        # get raw data
        firstfile <- files_Raw[index]
        initial_df_raw <- read.csv(firstfile)
        tmp_raw <- initial_df_raw[,c(4,5)]

        series_name <- initial_df_raw[,c(1)][1]

        # get final forecasts
        firstfile <- files_FinalForecasts[index]
        initial_df_finalfore <- read.csv(firstfile)
        tmp_forecast <- initial_df_finalfore[,c(1,2)]

        # copy column names
        colnames(tmp_forecast) <- colnames(tmp_raw)
        combined <- rbind(tmp_raw, tmp_forecast)

        # colors for different parts of line
        combined$mycolors <- c(rep('hist', length(combined[,1])-2), rep(c('forecast'),2))
        combined$mycolors <- as.factor(combined$mycolors)
        combined$ObservationDate <- as.Date(as.yearqtr(combined$ObservationDate), frac = 0)

        ###
        # Combine point forecast + entire series (using data above)
        ###
        combined$seriesDifferenced <- combined['RawData'] - lag(combined['RawData'], 4)
        combined$seriesDifferenced <- unlist(combined$seriesDifferenced)

        df1 <- t(combined[,c(4)])
        colnames(df1) <- t(combined[c(1)])
        df2 <- as.data.frame(df1)
        df3 <- cbind(series_name, df2)

        mylist[[index]] <- as.data.frame(df3)

    }

    series_hist_forecast <- list_rbind(mylist)
    write.table(series_hist_forecast, file = "output/analyses/combined_series_hist_forecasts.csv", sep =",",row.names = FALSE)
}

fun_bigchanges()

######################
# Forecasts, using biggest changers in absolute value, rank list of biggest changes last quarter with forecast, remove if missing
######################

fun_bigchanges_absoluteValue_forecastedData <- function(){
    myfile <- "output/analyses/combined_series_hist_forecasts.csv"
    bigchangers_df <- read.csv(myfile, stringsAsFactors=FALSE)

    bigchangers_df <- bigchangers_df[!grepl("Totaal", bigchangers_df$series_name),]

    names(bigchangers_df) <- format(as.Date(paste0(names(bigchangers_df)), 'X%Y.%m.%d'))
    lastCol <- tail(names(bigchangers_df),1)
    o <- order(abs(bigchangers_df[,lastCol]), decreasing = TRUE)
    output1 <- bigchangers_df[o, ][c(1,ncol(bigchangers_df))]

    write.table(output1, file = "output/analyses/forecasts_changes_absoluteValue.csv", sep =",",row.names = FALSE)
}

fun_bigchanges_absoluteValue_forecastedData()


######################
# Forecasts, using biggest changers in percentage, rank list of biggest changes last quarter with forecast, remove if missing
######################

# get forecasts from combined_final_forecasts.csv
myfile <- "output/analyses/combined_series_hist_forecasts.csv"
df1 <- read.csv(myfile, stringsAsFactors=FALSE)
diff_forecast <- df1[, c(1,ncol(df1))]

# absolute value a year before
rawDataFile