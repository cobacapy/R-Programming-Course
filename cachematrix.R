## R-programming Course Assignment #2: Caching the inverse of a matrix
## two functions one to generate the cache and a second to poll the cache and
##pull the values if they are present

## the function below returns a list containing four functions
## to define and retrieve the values of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function recieves the above function as an input.
## The cached values are polled, and if the result is present the cached values
## are pulled.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
      message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
