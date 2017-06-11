pollutantmean <- function(directory, pollutant, id = 1:332) {
  aggregate <- numeric()

  for(i in id) {
    path <- paste(getwd(), directory, sprintf("%03d.csv", i), sep='/')

    csv <- read.csv(path)

    completeValues <- !is.na(csv[pollutant])

    aggregate <- c(aggregate, csv[pollutant][completeValues])
  }

  mean(aggregate)
}
