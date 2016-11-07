## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  set(x)
  
  get <- function() {
    x
  }
  
  
  setInverse <- function(i) inv <<- i
  getInverse <- function() inv
  
  list(set = set, get = get, 
       setInverse = setInverse, getInverse = getInverse)
}


## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
