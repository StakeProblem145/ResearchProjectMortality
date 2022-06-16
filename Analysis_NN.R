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


plotCrudeRatesPerAge <- function(dataSetNn, year, gender, moreDataSets) {
  plot <- ggplot(filter(dataSetNn, Gender == gender & Year == year))+
    geom_line(aes(x = Age, y = log_mortality, color = "Observed")) +
    geom_line(aes(x = Age, y = NN_log_mortality, color = "Neural Network")) +
    ggtitle(paste(gender, year))
  if(hasArg(moreDataSets)) {
    for (model in moreDataSets) {
      plot <- plot +
        geom_line(data = model, aes(x = Age, y = log_mortality))
    }
  }
  return(plot)
} 

plotCrudeRatesPerYear <- function(dataSetNn, age, gender, moreDataSets) {
  plot <- ggplot(filter(dataSetNn, Gender == gender & Age == age))+
    geom_line(aes(x = Year, y = log_mortality, color = "Observed")) +
    geom_line(aes(x = Year, y = NN_log_mortality, color = "Neural Network")) +
    ggtitle(paste(gender, age))
  if(hasArg(moreDataSets)) {
    for (model in moreDataSets) {
      plot <- plot +
        geom_line(data = model, aes(x = Year, y = log_mortality))
    }
  }
  return(plot)
} 


plotParameterPerAge <- function(dataSet, parameter, year, gender, lineType = "Point"){
  plot <- ggplot(filter(dataSet, Gender == gender & Year == year), aes_string(x = "Age", y = parameter)) +
    ggtitle(paste(gender, year))
  if(lineType == "Line") {
    plot <- plot +
      geom_line()
  } else {
    plot <- plot +
      geom_point()
  }
  return(plot)
}

plotParameterPerYear <- function(dataSet, parameter, age, gender, lineType = "Point"){
  plot <- ggplot(filter(dataSet, Gender == gender & Age == age), aes_string(x = "Year", y = parameter)) +
    ggtitle(paste(gender, age))
  if(lineType == "Line") {
    plot <- plot +
      geom_line()
  } else {
    plot <- plot +
      geom_point()
  }
  return(plot)
}



heatmapAgeYear(testTrainingData, "Res_Deaths", "Female", c(-10,10))
heatmapAgeYear(testTrainingData, "Res_Deaths", "Male", c(-10,10))

heatmapAgeYear(testForecastData, "Res_Deaths", "Female", c(-20,20))
heatmapAgeYear(testForecastData, "Res_Deaths", "Male", c(-20,20))

plane3dPlotAgeYearResDeaths(testTrainingData, "Female")
plane3dPlotAgeYearResDeaths(testTrainingData, "Male")

plane3dPlotAgeYearResDeaths(testForecastData, "Female")
plane3dPlotAgeYearResDeaths(testForecastData, "Male")

plotCrudeRatesPerAge(testTrainingData, 2001, "Female")
plotCrudeRatesPerAge(testTrainingData, 2001, "Male")

plotCrudeRatesPerYear(testTrainingData, 75, "Female")
plotCrudeRatesPerYear(testTrainingData, 75, "Male")


plotCrudeRatesPerAge(testForecastData, 2015, "Female")
plotCrudeRatesPerAge(testForecastData, 2015, "Male")

plotCrudeRatesPerYear(testForecastData, 75, "Female")
plotCrudeRatesPerYear(testForecastData, 75, "Male")


plotParameterPerAge(testTrainingData, "Res_Deaths", 2001, "Female")
plotParameterPerAge(testTrainingData, "Res_Deaths", 2001, "Male")

plotParameterPerYear(testTrainingData, "Res_Deaths", 75, "Female")
plotParameterPerYear(testTrainingData, "Res_Deaths", 75, "Male")


plotParameterPerAge(testForecastData, "Res_Deaths", 2015, "Female")
plotParameterPerAge(testForecastData, "Res_Deaths", 2015, "Male")

plotParameterPerYear(testForecastData, "Res_Deaths", 75, "Female")
plotParameterPerYear(testForecastData, "Res_Deaths", 75, "Male")


