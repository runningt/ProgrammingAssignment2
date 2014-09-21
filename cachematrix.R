## Cache based counting inversion of matrices

## makeCacheMatrix:
## Creates a "cache compliant" version of matrix that can be used by cacheSolve function
## Parameters: x - input matrix (should be invertable)
## Returns: list of setters and getters of cache compliant matrix
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
##cacheSolve:
## This function computes the inverse of "cache compliant" matrix returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed),
## retrieves the inverse from the cache
## Parameters: x - "cache compliant" matrix returned by makeCacheMatrix
## Returns: inverse of x

cacheSolve <- function(x, ...) {
  res <- x$getsolve()
  if(!is.null(res)) {
    message("getting cached data")
    return(res)
  }
  matrix <- x$get()
  res <- solve(matrix, ...)
  x$setsolve(res)
  res
}
