# R_ProjectAssignment2
###### Overall description:
# Matrix inversion is usually a costly computation and there are benefits to caching 
# the inverse of a matrix rather than compute it repeatedly. 
# This script includes a pair of functions that cache the inverse of a matrix.


# The function makeCacheMatrix creates a special "matrix" object that 
# can cache its inverse.

makeCacheMatrix <- function(mx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mx <<- x
    inverse_mx <<- NULL
  }
  get <- function() mx
  setinv <- function(inv) inverse_mx <<- inv
  getinv <- function() inverse_mx
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# The CacheSolve function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the inverse 
# from the cache.

cacheSolve <- function(mx) {
  inverse_mx <- mx$getinv()
  if(!is.null(inverse_mx)) {
    message("Getting cached data...")
    return(inverse_mx)
  }
  data <- mx$get()
  inverse_mx <-inv(data)
  mx$setinv(inverse_mx)
  inverse_mx
}

