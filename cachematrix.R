## These two functions are used to compute the inverse of a square matrix.
## If the inverse of the matrix has been cached, the function will return the cache.

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix. If the inverse has already been calculated, the cache is retrieved.

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
