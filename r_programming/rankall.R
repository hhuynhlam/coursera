kStates <- state.abb

rankall <- function(outcome, num = kBest) {
  lapply(kStates, function(state) {
    rankhospital(state, outcome, num)
  })
}
