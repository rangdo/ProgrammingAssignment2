## defined are two helper functions that will cache potentially
## expensive inverse operations on a matrix object

## Create a matrix object that is capable of storing a cached
## inverse solution

makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  set <- function(y) {
    x <<- y
    cached_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cached_inverse <<- inverse
  getinverse <- function() cached_inverse
  list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## For the given a cached matix object ( created using the above function )
## calculate the inverse of the matrix or use the store cached 
## version if already calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}