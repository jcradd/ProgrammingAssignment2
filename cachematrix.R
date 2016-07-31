## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix:
## x is the matrix supplied by original call or set function
## m can hold cached copy of a matrix, if passed using setInverted
## m is intended to be called by cachesolve to hold inverted matrix
## the logic is all copied from makeVector and adapted for a matrix
## getInverted will pass back a null if setInverted hasn't been used since 
##   initialization or set

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverted <- function(inverted) m <<- inverted
  getInverted <- function() m
  list(set = set, get = get,
       setInverted = setInverted,
       getInverted = getInverted)
}


## cacheSolve should be passed an instance of makeCacheMatrix
## this will check if inverted matrix is already cached
## if cached version doesn't exist, solve and set cache
## and return the solved (inverted) matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverted()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverted(m)
  m
}



