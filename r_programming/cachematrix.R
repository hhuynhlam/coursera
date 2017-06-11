makeCacheMatrix <- function(x = matrix()) {
# Creates a closure for a supplied matrix that adds the ability to get and set a cached inverse.
#
# Args:
#   x: invertible matrix.
#
# Returns:
#   list(
#     get: gets the cached matrix,
#     set: sets the cached matrix,
#     getInverse: gets the cached inverse matrix,
#     setInverse: sets the cached inverse matrix,
#   )

  cachedInverse <- NULL
  cachedMatrix <- x                 # set the matrix on invocation so that we don't have to call
                                    # $set immediately after
  get <- function() {
    cachedMatrix
  }

  set <- function(matrix) {
    cachedInverse <<- NULL
    cachedMatrix <<- matrix
  }

  getInverse <- function() {
    cachedInverse
  }

  setInverse <- function(inverse) {
    cachedInverse <<- inverse
  }

  list(
    get = get,
    set = set,
    getInverse = getInverse,
    setInverse = setInverse
  )
}

cacheSolve <- function(x, ...) {
# Solve the inverse for a supplied matrix.
#
# Args:
#   x: invertible cached matrix
#
# Returns:
#   matrix()

  inverse <- x$getInverse()

  if(is.null(inverse)) {            # if a cached inverse does not exist for the matrix
    matrix <- x$get()               # get the original matrix

    inverse <- solve(matrix, ...)   # solve the inverse

    x$setInverse(inverse)           # set the inverse into cache

    return(inverse)
  }

  inverse
}
