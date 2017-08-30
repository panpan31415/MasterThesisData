rm(list = ls())
library(dplyr)
library(ggplot2)
library(lubridate)
library(XML)

#load apple health export.xml file
xml <- xmlParse("/Users/panpan31415/Desktop/ThesisData/apple_health_export/export.xml", useInternalNodes=TRUE)

#transform xml file to data frame - select the Record rows from the xml file
df <- XML:::xmlAttrsToDataFrame(xml["//Record"],stringsAsFactors=FALSE)
str(df)

#make value variable numeric
# df$value <- as.numeric(as.character(df$value))
# str(df)