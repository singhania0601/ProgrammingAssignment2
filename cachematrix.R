## Put comments here that give an overall description of what your
## functions do

## this function creates a list containing four functions, the first one stores
## the value of the matrix while the others return the matrix value, calculates
## and inverse of the matrix and the fourth one returns the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## cacheSolve takes the special vector created by the makeCacheMatrix functions
## and uses to calculate and cache the inverse. It returns the inverse if it is
## cached. Also, it takes the original matrix object as a second argument, and
## compares it to the stored matrix in the special matrix object, if it is different
## it outputs an error and doesn't continue.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
