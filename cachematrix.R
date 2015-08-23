## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
## this function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  setmatrix <- function(y) {
    x <<- y
    im <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(invmat) im <<- invmat
  getinverse <- function() im
  
  list(setmatrix = setmatrix, 
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$getmatrix()
  im <- solve(data, ...)
  x$setinverse(im)
  im
}
