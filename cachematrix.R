## Two functions in order to create and store
## a "matrix" object and also calculate and
## store its inverse

## This function creates a matrix object which can
## cache its inverse. Returns a list of 4 functions:
## "get" or "set" the matrix, and "get" or "set" the
## inverse of the stored matrix.

makeCacheMatrix <- function(x = matrix()) {
  matInverse <- NULL
  set <- function(y) {
    x <<- y
    matInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) matInverse <<- inverse
  getInverse <- function() matInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of a given matrix.
## First checks if it has been calculated in the input
## parameter. If so, returns the actual value, otherwise
## the function calculates the inverse and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matInverse <- x$getInverse()
  if(!is.null(matInverse)) {
    message("getting cached data")
    return(matInverse)
  }
  data <- x$get()
  matInverse <- solve(data, ...)
  x$setInverse(matInverse)
  matInverse
}
