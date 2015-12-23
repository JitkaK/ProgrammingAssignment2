## Put comments here that give an overall description of what your
## functions do

## These functions compute inversion of matrix with using of cashing. 

## Write a short comment describing this function


## makeCacheMatrix create a list of functions that load a matrix, 
## print the matrix, compute an inversion of the matrix and print
## the inversion

makeCacheMatrix <- function(x = matrix()) {
  xi <- NULL
  set <- function(y) {
    x <<- y
    xi <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) xi <<- solve
  getinverse <- function() xi
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve function check if an inversion of a matrix wasn't
## computed yet. In that case it print 'getting cached data' and 
## get data from cache. Otherwise it compute the inversion.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xi <- x$getinverse()
  if(!is.null(xi)) {
    message("getting cached data")
    return(xi)
  }
  data <- x$get()
  xi <- solve(data, ...)
  x$setinverse(xi)
  xi
}
