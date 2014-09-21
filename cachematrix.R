## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix:
## 
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
