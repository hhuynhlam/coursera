corr <- function(directory, threshold = 0) {
  aggregate <- numeric()

  for(i in 1:322) {
    if (complete(directory, i)['nobs'] > threshold) {
      path <- paste(getwd(), directory, sprintf("%03d.csv", i), sep='/')

      csv <- read.csv(path)

      csv <- csv[complete.cases(csv),]

      nitrate <- as.numeric(unlist(csv['nitrate']))
      sulfate <- as.numeric(unlist(csv['sulfate']))

      correlation <- cor(nitrate, sulfate, use = 'pairwise.complete.obs')

      aggregate <- c(aggregate, correlation)
    }
  }

  aggregate
}
