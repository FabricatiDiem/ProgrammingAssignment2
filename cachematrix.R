##The following functions create and manipulate a function vector that 
##represents a matrix and its cached inverse. Based on the examples by R. Peng.


## makeCacheMatrix creates a vector of named functions that allows the matrix 
## inverse to be cached. The get and set functions manipulate the stored matrix 
## itself and the getinv and setinv functions manipulate the stored matrix 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve uses the function vector created by the makeCacheMatrix 
## function to obtain the inverse of the given matrix, using the cached result 
## if it exists. If the inverse has not yet been calculated, it calculates it 
## and stores it in the cache for later use.

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
