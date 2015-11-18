# These functions provide a special "matrix" object that can cache 
# its inverse.

# The function makeCacheMatrix creates a special "matrix", 
# which is really a list containing functions to
# set - set the value of the matrix
# get - get the value of the matrix
# setinv - set the value of the inverse
# getinv - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# The cacheSolve function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not 
# changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
