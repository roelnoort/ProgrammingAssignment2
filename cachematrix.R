## These functions create a matrix object that contains
## caching for the inverse of the matrix.


## makeCacheMatrix contains a matrix with functions to set/get it.
## it also allows to store the calculated inverse of itself
## which would need to be set/retrieved via the setInverse/getInverse
## functions
makeCacheMatrix <- function(x = matrix()) {
  ## this variable stores the cached inverse. initialise to NULL.
  storedInverse <- NULL
  
  ## set(m) stores the matrix m
  set <- function (m) {
    x <<- m  ## store the matrix
    ## as we store a new matrix, we need to reset
    ## the storedInverse variable. We haven't yet
    ## calculated the inverse for this new matrix
    storedInverse <<- NULL
  }
  
  ## get() function returns the matrix stored
  get <- function() x
  
  ## setInverse(inverse) stores the inverse of this
  ## matrix
  setInverse <- function(inverse) storedInverse <<- inverse
  
  ## getInverse() returns the inverse of this matrix, or
  ## NULL if not previously calculated
  getInverse <- function() storedInverse
  
  ## return makeCacheMatrix as a list with 4 functions
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve calculates the inverse of matrix x (of type 
## makeCacheMatrix). It ensure optimal operation as caching
## is used to only calculate the inverse once for each matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Retrieve the cached inverse from the matrix
  inverse <- x$getInverse()
  
  ## When this value has been defined (ie not equal to NULL)
  ## simply return it.
  if (!is.null(inverse)) {
    ## display a message for debugging purpose
    message("using cached data")
    ## return the inverse as the result
    return(inverse)
  }
  
  ## Otherwise, we will need to calculate the inverse
  
  ## load the matrix to calculate the inverse on
  matrixToInverse <- x$get()
  
  ## do the actual inverse calculation. Assume that the
  ## matrix is inversible
  inverse <- solve(matrixToInverse, ...) 
  
  ## store the result to the cache, so that next time we
  ## can directly return this
  x$setInverse(inverse)
  
  ## return the inverse as the result
  inverse
}
