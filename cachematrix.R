## Function to cache inverse of matrix
## <<- searches through parent environment to find the
## definition of the variable, if any.

## makeCacheMatrix is called to set the matrix and its 
## matrix in an environment which can be retrieved later

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseValue) inverse <<- inverseValue
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}


## cacheSolve takes the input matrix, checks if inverse
## is already found, if found returns the inverse.
## If not found, calculates the inverse and then sets in the
## cache for later retrieval. Then returns the inverse to the console.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}