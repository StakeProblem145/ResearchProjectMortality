library(tidyverse)
library(plotly)
library(viridis)


heatmapAgeYear <- function(dataSet, fillParameter, gender, limitFillParameter) {
  if(hasArg(limitFillParameter)) {
    dataSet <- dataSet %>%
      mutate(across(!!fillParameter, ~ ifelse(. <= limitFillParameter[1], limitFillParameter[1], .))) %>%
      mutate(across(!!fillParameter, ~ ifelse(. >= limitFillParameter[2], limitFillParameter[2], .)))
  }
  ggplot(filter(dataSet, Gender == gender), aes_string("Year", "Age", fill = fillParameter)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue") +
    ggtitle(paste(gender, fillParameter))
}


plane3dPlotAgeYearResDeaths <- function(dataSet, gender, limitFillParameter) {
  if(hasArg(limitFillParameter)) {
    dataSet <- dataSet %>%
      mutate(across(Res_Deaths, ~ ifelse(. <= limitFillParameter[1], limitFillParameter[1], .))) %>%
      mutate(across(Res_Deaths, ~ ifelse(. >= limitFillParameter[2], limitFillParameter[2], .)))
  }
  plot_ly(data = filter(dataSet, Gender == gender), x = ~Year, y = ~Age, z = ~Res_Deaths) %>%
    add_trace(type = "mesh3d", intensity = ~Res_Deaths, colors = colorRamp(c("blue", "lightblue", "chartreuse3", "yellow", "red")))
}


plotCrudeRatesPerAge <- function(dataSetNn, year, gender, xlim, ylim, moreDataSets) {
  plot <- ggplot(filter(dataSetNn, Gender == gender & Year == year))+
    geom_line(aes(x = Age, y = NN_log_mortality, color = "Neural Network")) +
    ggtitle(paste(gender, year))
  
  if("log_mortality" %in% names(dataSetNn)){
    plot <- plot +
      geom_line(aes(x = Age, y = log_mortality, color = "Observed"))
  }
  
  if(hasArg(xlim)) {
    plot <- plot +
      xlim(xlim)
  }
  
  if(hasArg(ylim)) {
    plot <- plot +
      ylim(ylim)
  }
  
  if(hasArg(moreDataSets)) {
    plot <- plot +
      geom_line(data = filter(moreDataSets, Gender == gender & Year == year), aes(x = Age, y = CLA_log_mortality, color = "Classic Model"))
  }
  return(plot)
} 


plotCrudeRatesPerAgeBothGender <- function(dataSetNn, year, xlim, ylim, moreDataSets) {
  plot <- ggplot(filter(dataSetNn, Year == year))+
    geom_line(data = ~filter(.x, Gender == "Female"), aes(x = Age, y = NN_log_mortality, color = "Female Neural Network")) +
    geom_line(data = ~filter(.x, Gender == "Male"), aes(x = Age, y = NN_log_mortality, color = "Male Neural Network")) +
    ggtitle(paste(year))
  
  if("log_mortality" %in% names(dataSetNn)){
    plot <- plot +
      geom_line(data = ~filter(.x, Gender == "Female"), aes(x = Age, y = log_mortality, color = "Female Observed")) +
      geom_line(data = ~filter(.x, Gender == "Male"), aes(x = Age, y = log_mortality, color = "Male Observed"))
  }
  
  if(hasArg(xlim)) {
    plot <- plot +
      xlim(xlim)
  }
  
  if(hasArg(ylim)) {
    plot <- plot +
      ylim(ylim)
  }
  
  if(hasArg(moreDataSets)) {
    plot <- plot +
      geom_line(data = filter(moreDataSets, Gender == "Female" & Year == year), aes(x = Age, y = CLA_log_mortality, color = "Female Classic Model"))
      #geom_line(data = filter(moreDataSets, Gender == "Male" & Year == year), aes(x = Age, y = CLA_log_mortality, color = "Male Classic Model"))
  }
  return(plot)
} 


plotCrudeRatesPerYear <- function(dataSetNn, age, gender, xlim, ylim, moreDataSets) {
  plot <- ggplot(filter(dataSetNn, Gender == gender & Age == age))+
    geom_line(aes(x = Year, y = NN_log_mortality, color = "Neural Network")) +
    ggtitle(paste(gender, age))
  
  if("log_mortality" %in% names(dataSetNn)){
    plot <- plot +
      geom_line(aes(x = Year, y = log_mortality, color = "Observed"))
  }
  
  if(hasArg(xlim)) {
    plot <- plot +
      xlim(xlim)
  }
  
  if(hasArg(ylim)) {
    plot <- plot +
      ylim(ylim)
  }
  
  if(hasArg(moreDataSets)) {
    plot <- plot +
      geom_line(data = filter(moreDataSets, Gender == gender & Age == age), aes(x = Year, y = CLA_log_mortality, color = "Classic Model"))
  }
  return(plot)
}


plotCrudeRatesPerYearBothGender <- function(dataSetNn, age, xlim, ylim, moreDataSets) {
  plot <- ggplot(filter(dataSetNn, Age == age))+
    geom_line(data = ~filter(.x, Gender == "Female"), aes(x = Year, y = NN_log_mortality, color = "Female Neural Network")) +
    geom_line(data = ~filter(.x, Gender == "Male"), aes(x = Year, y = NN_log_mortality, color = "Male Neural Network")) +
    ggtitle(paste(age))
  
  if("log_mortality" %in% names(dataSetNn)){
    plot <- plot +
      geom_line(data = ~filter(.x, Gender == "Female"), aes(x = Year, y = log_mortality, color = "Female Observed")) +
      geom_line(data = ~filter(.x, Gender == "Male"), aes(x = Year, y = log_mortality, color = "Male Observed"))
  }
  
  if(hasArg(xlim)) {
    plot <- plot +
      xlim(xlim)
  }
  
  if(hasArg(ylim)) {
    plot <- plot +
      ylim(ylim)
  }
  
  if(hasArg(moreDataSets)) {
    plot <- plot +
      geom_line(data = filter(moreDataSets, Gender == "Female" & Age == age), aes(x = Year, y = CLA_log_mortality, color = "Female Classic Model"))
      #geom_line(data = filter(moreDataSets, Gender == "Male" & Age == age), aes(x = Year, y = CLA_log_mortality, color = "Male Classic Model"))
  }
  return(plot)
}


plotCrudeRatesPerCohort <- function(dataSetNn, cohort, gender, xlim, ylim, moreDataSets) {
  dataSetNn <- dataSetNn %>%
    mutate(Cohort = Year - Age)
  plot <- ggplot(filter(dataSetNn, Gender == gender & Cohort == cohort))+
    geom_line(aes(x = Age, y = NN_log_mortality, color = "Neural Network")) +
    ggtitle(paste(gender, cohort))
  
  if("log_mortality" %in% names(dataSetNn)){
    plot <- plot +
      geom_line(aes(x = Age, y = log_mortality, color = "Observed"))
  }
  
  if(hasArg(xlim)) {
    plot <- plot +
      xlim(xlim)
  }
  
  if(hasArg(ylim)) {
    plot <- plot +
      ylim(ylim)
  }
  
  if(hasArg(moreDataSets)) {
    plot <- plot +
      geom_line(data = filter(moreDataSets, Gender == gender & Cohort == cohort), aes(x = Age, y = CLA_log_mortality, color = "Classic Model"))
  }
  return(plot)
}


plotParameterPerAge <- function(dataSet, parameter, year, gender, xlim, ylim, lineType = "Point"){
  plot <- ggplot(filter(dataSet, Gender == gender & Year == year), aes_string(x = "Age", y = parameter)) +
    ggtitle(paste(gender, year))
  
  if(hasArg(xlim)) {
    plot <- plot +
      xlim(xlim)
  }
  
  if(hasArg(ylim)) {
    plot <- plot +
      ylim(ylim)
  }
  
  if(lineType == "Line") {
    plot <- plot +
      geom_line()
  } else {
    plot <- plot +
      geom_point()
  }
  return(plot)
}


plotParameterPerYear <- function(dataSet, parameter, age, gender, xlim, ylim, lineType = "Point"){
  plot <- ggplot(filter(dataSet, Gender == gender & Age == age), aes_string(x = "Year", y = parameter)) +
    ggtitle(paste(gender, age))
  
  if(hasArg(xlim)) {
    plot <- plot +
      xlim(xlim)
  }
  
  if(hasArg(ylim)) {
    plot <- plot +
      ylim(ylim)
  }
  
  if(lineType == "Line") {
    plot <- plot +
      geom_line()
  } else {
    plot <- plot +
      geom_point()
  }
  return(plot)
}


### Heatmaps Res_Death
heatmapAgeYear(testTrainingData, "Res_Deaths", "Female", c(-10,10))
heatmapAgeYear(testTrainingData, "Res_Deaths", "Male", c(-10,10))

heatmapAgeYear(testForecastData, "Res_Deaths", "Female", c(-20,20))
heatmapAgeYear(testForecastData, "Res_Deaths", "Male", c(-20,20))


### 3d Plots Res_Death
plane3dPlotAgeYearResDeaths(testTrainingData, "Female")
plane3dPlotAgeYearResDeaths(testTrainingData, "Male")

plane3dPlotAgeYearResDeaths(testForecastData, "Female")
plane3dPlotAgeYearResDeaths(testForecastData, "Male")

testFullRangePeriod <- rbind(testTrainingData, testForecastData, fill=TRUE)

heatmapAgeYear(testFullRangePeriod, "Res_Deaths", "Female", c(-20,20))
heatmapAgeYear(testFullRangePeriod, "Res_Deaths", "Male", c(-20,20))

plane3dPlotAgeYearResDeaths(testFullRangePeriod, "Female")
plane3dPlotAgeYearResDeaths(testFullRangePeriod, "Male")



### Full Period
testFullRangeData <- rbind(testTrainingData, testForecastData, testInfinityData, fill=TRUE)

plotCrudeRatesPerAge(testFullRangeData, 2001, "Female")
plotCrudeRatesPerAge(testFullRangeData, 2001, "Male")

plotCrudeRatesPerYear(testFullRangeData, 75, "Female")
plotCrudeRatesPerYear(testFullRangeData, 75, "Male")

plotCrudeRatesPerCohort(testFullRangeData, 1930, "Female")
plotCrudeRatesPerCohort(testFullRangeData, 1930, "Male")

plotCrudeRatesPerAgeBothGender(testFullRangeData, 2001)
plotCrudeRatesPerYearBothGender(testFullRangeData, 75)

plotCrudeRatesPerAgeBothGender(testFullRangeData, 2016)
plotCrudeRatesPerYearBothGender(testFullRangeData, 75)

plotCrudeRatesPerYearBothGender(testFullRangeData, 85, moreDataSets = testClassicModelForcast)


### Training Period
# Function can be used like this:
# plotCrudeRatesPerAge(testTrainingData, 2001, "Female", xlim = c(50,90), ylim = c(-5,-1))
plotCrudeRatesPerAge(testTrainingData, 2001, "Female")
plotCrudeRatesPerAge(testTrainingData, 2001, "Male")

plotCrudeRatesPerYear(testTrainingData, 75, "Female")
plotCrudeRatesPerYear(testTrainingData, 75, "Male")

plotCrudeRatesPerCohort(testTrainingData, 1930, "Female")
plotCrudeRatesPerCohort(testTrainingData, 1930, "Male")

plotCrudeRatesPerAgeBothGender(testTrainingData, 2001)
plotCrudeRatesPerYearBothGender(testTrainingData, 75)

### Forecast Period
plotCrudeRatesPerAge(testForecastData, 2015, "Female")
plotCrudeRatesPerAge(testForecastData, 2015, "Male")

plotCrudeRatesPerYear(testForecastData, 75, "Female")
plotCrudeRatesPerYear(testForecastData, 75, "Male")

plotCrudeRatesPerCohort(testForecastData, 1930, "Female")
plotCrudeRatesPerCohort(testForecastData, 1930, "Male")

plotCrudeRatesPerAgeBothGender(testForecastData, 2015)
plotCrudeRatesPerYearBothGender(testForecastData, 75)


### Infinity Period
plotCrudeRatesPerAge(testInfinityData, 2030, "Female")
plotCrudeRatesPerAge(testInfinityData, 2030, "Male")

plotCrudeRatesPerYear(testInfinityData, 75, "Female")
plotCrudeRatesPerYear(testInfinityData, 75, "Male")

plotCrudeRatesPerCohort(testInfinityData, 1950, "Female")
plotCrudeRatesPerCohort(testInfinityData, 1950, "Male")

plotCrudeRatesPerAgeBothGender(testInfinityData, 2030)
plotCrudeRatesPerYearBothGender(testInfinityData, 75)


### Parameter Analysis
#plotParameterPerAge(testTrainingData, "Res_Deaths", 2001, "Female", ylim = c(-4,4))
plotParameterPerAge(testTrainingData, "Res_Deaths", 2001, "Female")
plotParameterPerAge(testTrainingData, "Res_Deaths", 2001, "Male")

plotParameterPerYear(testTrainingData, "Res_Deaths", 75, "Female")
plotParameterPerYear(testTrainingData, "Res_Deaths", 75, "Male")


plotParameterPerAge(testForecastData, "Res_Deaths", 2015, "Female")
plotParameterPerAge(testForecastData, "Res_Deaths", 2015, "Male")

plotParameterPerYear(testForecastData, "Res_Deaths", 75, "Female")
plotParameterPerYear(testForecastData, "Res_Deaths", 75, "Male")


