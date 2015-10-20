# Load required libraries
require(data.table)
library(dplyr)
library(DT)
library(rCharts)

# Read data
dataFile <- read.csv("./data/OutputFile.csv",header= FALSE);
df = dataFile[5:22, 1:55];
dt <- data.table(df)
mat <- t(dt)
data <- data.table(mat)
data[,row.names:=NULL]
data <- data[-1,]
setnames(data, "V1", "year")
setnames(data, "V2", "total_pop")
setnames(data, "V3", "resident_pop")
setnames(data, "V4", "citizen_pop")
setnames(data, "V5", "pr_pop")
setnames(data, "V6", "non_res_pop")
setnames(data, "V7", "total_pop_growth_percent")
setnames(data, "V8", "res_pop_growth_percent")
setnames(data, "V9", "pop_density")
setnames(data, "V10", "sex_ratio")
setnames(data, "V11", "median_age")
setnames(data, "V12", "supp_ratio_fifteen")
setnames(data, "V13", "supp_ratio_twenty")
setnames(data, "V14", "dep_ratio_fifteen")
setnames(data, "V15", "dep_ratio_twenty")
setnames(data, "V16", "old_age_dep_ratio")
setnames(data, "V17", "nat_increase")
setnames(data, "V18", "rate_nat_increase")

## Helper functions

#' Filter dataset by population variables & year (character dataset)
#' 
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @param popVariables
#' @return result data.table
#' 
filterByYear <- function(dt, minYear, maxYear, popVariables) {
  dt <- dt %>% filter(year >= minYear, year <=maxYear)
  popVariables <- as.numeric(popVariables)
  dt <- dt[, c(1,popVariables), with=FALSE]
  result <- datatable(dt, options = list(iDisplayLength = 50))
  return(result)
}

#' Filter dataset by population variables & year (numeric dataset)
#' 
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @param popVariables
#' @return data.table
#'
dataByPopVar <- function(dt, minYear, maxYear, popVariables) {
  dt <- dt %>% filter(year >= minYear, year <=maxYear)
  popVar <- as.numeric(popVariables); popVar <- c(1,popVar);
  popVar <- head(popVar,18)
  select_dt <- dt[,popVar,with=FALSE]
  select_dt <- apply(select_dt, 1, function(x) gsub(",","",x))
  result <- apply(select_dt, 1, function(x) as.numeric(x))
  return(result)
}

#' Filter dataset by population variables & year (numeric dataset)
#' 
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @param popVariables
#' @return data.table
#'
dataByPopVarPercent <- function(dt, minYear, maxYear, popVariables) {
  dt <- dt %>% filter(year >= minYear, year <=maxYear)
  popVar <- c(1,7,8);
  select_dt <- dt[,popVar,with=FALSE]
  select_dt <- apply(select_dt, 1, function(x) gsub(",","",x))
  result <- apply(select_dt, 1, function(x) as.numeric(x))
  return(result)
}

#' Plot total population (number)
#' 
#' @param dt data.table
#' @param dom
#' @param xAxisLabel year
#' @param yAxisLabel numbers
#' @return popByNumber plot
plotPopVar1 <- function(mat, dom = "popVar1", 
                            xAxisLabel = "Year",
                            yAxisLabel = "Numbers") {
  dt <- data.table(mat)
  popByNumber <- nPlot(
    total_pop ~ year,
    data = dt[,c(1,2),with=FALSE],
    dom = dom,  width = 650,
    type = "multiBarChart"
  )
  popByNumber$chart(margin = list(left = 100))
  popByNumber$yAxis(axisLabel = yAxisLabel, width = 80)
  popByNumber$xAxis(axisLabel = xAxisLabel, width = 70)
#   popByNumber$layer(y='total_pop_growth_percent', copy_layer=T, type='line', color=list(const='red'))
  popByNumber
}

#' Plot total population growth (per cent)
#' 
#' @param dt data.table
#' @param dom
#' @param xAxisLabel year
#' @param yAxisLabel percent
#' @return popGrowthPercent plot
plotPopVar2 <- function(mat, dom = "popVar2", 
                                xAxisLabel = "Year",
                                yAxisLabel = "Percent") {
#   return(Rickshaw$new())
  dt <- data.table(mat)
  popGrowthPercent <- nPlot(
     y = "total_pop_growth_percent", x="year",
#    total_pop_growth_percent ~ year,
    data = dt[,c(1,2),with=FALSE],
    type = "lineChart",
    dom = dom, width = 650
  )
  popGrowthPercent$chart(margin = list(left = 100))
  popGrowthPercent$yAxis(axisLabel = yAxisLabel, width = 80)
  popGrowthPercent$xAxis(axisLabel = xAxisLabel, width = 70)
  popGrowthPercent
}

#' Plot total population growth (per cent)
#' 
#' @param dt data.table
#' @param dom
#' @param xAxisLabel year
#' @param yAxisLabel percent
#' @return residentPopGrowthPercent plot
plotPopVar3 <- function(mat, dom = "popVar3", 
                        xAxisLabel = "Year",
                        yAxisLabel = "Percent") {
  dt <- data.table(mat)
  resPopGrowthPercent <- nPlot(
    res_pop_growth_percent ~ year,
    data = dt[,c(1,3),with=FALSE],
    type = "lineChart",
    dom = dom, width = 650
  )
  resPopGrowthPercent$chart(margin = list(left = 100))
  resPopGrowthPercent$yAxis(axisLabel = yAxisLabel, width = 80)
  resPopGrowthPercent$xAxis(axisLabel = xAxisLabel, width = 70)
  resPopGrowthPercent
}