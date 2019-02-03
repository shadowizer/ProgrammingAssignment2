## This function creates an object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invers <<- inverse
  getinverse <- function() invers
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Gets the inverse of the matrix created by makeCacheMatrix. If the inverse is already there, it gets it from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(invers)
  }
  mat <- x$get()
  invers <- solve(mat, ...)
  x$setinverse(invers)
  invers
}
