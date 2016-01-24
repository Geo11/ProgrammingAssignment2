## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 


## The function makeCacheMatrix is based in the function "makeVector".
## This function creates the same list that creates the function "makeVector" 

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) s <<- solve
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The function cacheSolve, based in the function cachemean, calculate the inverse a matrix creating for makeCacheMatrix.
## First, the function checks whether the inverse of the matrix created by "makeCacheMatrix" has been calculated. 
## If it exists in memory, it simply returns a message and the value s. 
## Else, calculates the inverse of the matrix and x$setinv(s) stores it in the object generated assigned with makeCacheMatrix.

cacheSolve <- function(x, ...) {
  s <- x$getinv()
  if(!is.null(s)) {
    message("Inverse it has been previously calculated")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s     
}
