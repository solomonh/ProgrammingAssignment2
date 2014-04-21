# This R script contains functions to compute the inverse of a matrix and cache
# the result for faster retrieval on repetitive calls for the inverse of a matrix.

# the following function creates a special "matrix" that contains a list 
# of functions to:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x=matrix()) {
  y <- NULL
  set <- function(a){
    x <<- a
    y <<- NULL
  }
  
  get <- function() x
  setinverse <- function(i) y <<- i
  getinverse <- function() y
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

# This function computes the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the inverse
# has already been calculated. If so, it gets the inverse from the cache and
# skips the computation. Otherwise, it calculates the inverse of the matrix and
# sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...){
  ## Return a matrix that is the inverse of 'm'

  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data.")
    return (i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  return(i)
}
