## Coursera - R Language _ Programming Assignment 2
## Goal of this assignment is to create a pair of function that cache the inverse of a matrix.
## For this assignment, assume that the matrix supplied is always invertible

## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing several functions:
##    1. function to set the value of the vector
##    2. function to get the value of the vector
##    3. function to set the value of the inverse
##    4. function to get the value of the inverse

makeCacheMatrix <- function(mx = matrix()) {
    inv_mx <- NULL
    set <- function(y) {
      mx <<- y
      inv_mx <<- NULL
    }
    get <- function() mx
    setinverse <- function(solve) inv_mx <<- solve
    getinverse <- function() inv_mx
    list(set = set, 
	     get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates
## the inverse of the data and sets the value of the inverse in the cache 
## via the setinverse function.
##
## "..." parameters are passed to the generic solve function.
## They are used only when there is no inverse in the cache.
## When you first call cacheSolve(matrix, param) the inverse matrix
## is computed using solve(matrix, param).
## When you call cacheSolve second time with different parameter, i.e.
## solve(mt, param_2)
## the function will return value of solve(matrix, param) 
## instead of solve(mt, param_2)
## because there is already a cached inverse matrix 
## for the mt matrix.

cacheSolve <- function(mx, ...) {
  inv_mx <- mx$getinverse()
  if(!is.null(inv_mx)) {
    message("Getting inverse matrix from cache")
    return(inv_mx)
  }
  data <- mx$get()
  inv_mx <- solve(data, ...)
  mx$setinverse(inv_mx)
  inv_mx
}
