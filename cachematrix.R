makeCacheMatrix <- function(x = matrix()) {
  ## creates a list containing a function to:
  ## - set the value of the matrix
  ## - get the value of the matrix
  ## - set the value of the inverse
  ## - get the value of the inverse
  ## NOTE: the function assumes that the matrix supplied is always invertible!
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


cacheSolve <- function(x, ...) {
  ## Computes the inverse of the special "matrix" returned by makeCacheMatrix
  ## through the following steps:
  ## 1. Checks to see if the inverse has already been calculated. 
  ## 2. Gets the inverse from the cache (if available) or calculates 
  ## it from the data and sets the value of the inverse in the cache via the 
  ## setInverse function
  ## NOTE: the function assumes that the matrix supplied is always invertible!
  
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
