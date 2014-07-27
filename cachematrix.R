## Matrix Inversion is usually a costly computation
## It may make sense to cache the inverse of a matrix.
## Rather than compute it repeatedly.

## makeCacheMatrix create a list of functions
## that can cache the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- null
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve computes the inverse of a matrix
## returned by makeCacheMartrix. If the inverse has 
## already been calculated the retrieve from cache

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
