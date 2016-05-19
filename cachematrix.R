
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. 
## The following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing the following functions:
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse of the matrix
##    4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  #Stores the result of the inversion & initializes to NULL
  inv <- NULL
  # create the matrix in the working environment
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # return the input matrix
   get <- function () x
  # set the inversed matrix
  setsolve <- function(solve) inv <<- solve
  # return the inversed matrix
  getsolve <- function () inv
  # return the created functions to the working environment
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
} 
    


## This function computes the inverse of the matrix returned by makeCacheMatrix above.
## First it checks if the inverse has already been calculated.  If so, it gets the results from the
## cache function and skips the calculation.
## If not, it computes the inverse and sets the value via the setsolve in the cache.

## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        #attempt to get the inverse of the matrix stored in cache
  inv <- x$getsolve()
  if(!is.null(inv)) { # if the inversion result is there
    message("getting cached data")
    return(inv) # return the calculated inversion
  }
  # if not, get the matrix object
  data <- x$get()
  # set and return inverse of matrix
  inv <- solve(data, ...)
  # set inverted matrix in cache
  x$setsolve(inv)
  # display matrix in console
  inv
}

## Test run:
##> x <- matrix(1:4, 2, 2)
##> m <- makeCacheMatrix(x)
##> cacheSolve(m)  ## First Run
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(m) ## Second Run
##getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
