library("dplyr")   
library("diffdf")

data1 <- read.csv("data/cbs_basic_macro_allData_qt_2024_11_15.csv", sep = ",")
data2 <- read.csv("data/cbs_basic_macro_allData_qt_2024_11_14.csv", sep = ",")

all.equal(data1, data2)  

diffdf(data1, data2)
