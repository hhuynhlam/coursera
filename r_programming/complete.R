complete <- function(directory, id = 1:332) {
  completeCases <- numeric()
  ids <- numeric()

  for(i in id) {
    path <- paste(getwd(), directory, sprintf("%03d.csv", i), sep='/')

    csv <- read.csv(path)

    numberOfCompleteCases <- nrow(csv[complete.cases(csv),])

    completeCases <- c(completeCases, numberOfCompleteCases)
    ids <- c(ids, i)
  }

  data.frame(id = ids, nobs = completeCases)
}
