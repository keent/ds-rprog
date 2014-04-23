## functions for creating, caching and retrieving the 
## matrix and its inverse

## creates a Matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## retrieves or solves the inverse of a matrix x

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  if (!is.null(inverse)) {
    message("getting cached matrix inverse")
    return(inverse)
  }
  
  matrix <- x$get()
  inverse <- solve(matrix) #solve inverse of matrix
  x$setInverse(inverse) #save
  inverse
  
}
