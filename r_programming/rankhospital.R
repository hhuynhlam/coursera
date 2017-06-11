kBest <- 'best'
kHeartAttack <- 'heart attack'
kHeartAttackCol <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
kHeartFailure <- 'heart failure'
kHeartFailureCol <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
kHospitalNameCol <- 'Hospital.Name'
kPneumonia <- 'pneumonia'
kPneumoniaCol <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
kStateCol <- 'State'
kWorst <- 'worst'

check.validity <- function(state, outcome) {
  kOutcomes <- c(kHeartAttack, kHeartFailure, kPneumonia)
  kStates <- state.abb

  invalidOutcome <- !(outcome %in% kOutcomes)
  invalidState <- !(state %in% kStates)

  if (invalidOutcome) stop('invalid outcome')
  if (invalidState) stop('invalid state')
}

column.name <- function(outcome) {
  if (outcome == kHeartAttack) {
    return(kHeartAttackCol)
  } else if (outcome == kHeartFailure) {
    return(kHeartFailureCol)
  }

  kPneumoniaCol
}

read.outcome <- function(state, outcome) {
  columnName <- column.name(outcome)
  path <- paste(getwd(), 'hospitaldata/outcome-of-care-measures.csv', sep='/')

  columns <- c(kHospitalNameCol, kStateCol, columnName)

  csv <- read.csv(path)[columns]

  csv <- csv[csv['State'] == state,]

  csv[columnName] <- sapply(csv[columnName], as.character)
  suppressWarnings(csv[columnName] <- sapply(csv[columnName], as.numeric))

  csv[complete.cases(csv[columnName]),]
}

sort.outcome <- function(data, outcome) {
  columnName <- column.name(outcome)

  order <- order(data[columnName], data[kHospitalNameCol])

  data[order,]
}

rankhospital <- function(state, outcome, num = kBest) {
  check.validity(state, outcome)

  csv <- read.outcome(state, outcome)

  sortedOutcome <- sort.outcome(csv, outcome)[c(kHospitalNameCol, kStateCol)]

  if (num == kBest) {
    return(head(sortedOutcome, 1))
  } else if (num == kWorst) {
    return(tail(sortedOutcome, 1))
  }

  sortedOutcome[num,]
}
