##These functions can be used to store the value of a matrix, calculate its inverse, and
## store the value of that inverse in memory. They also create functions for retrieving
## the matrix and inverse being stored. By storing these values in memory, the inverse 
## value only has to be calculated once.


## This function takes an argument of a matrix(x) and creates getter and setter
## methods for the value of the matrix, and the value of its inverse.
## The function returns the list of these methods.
## This function also stores the values of the matrix, and it's inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, 
    setinverse = setinverse,
    getinverse = getinverse)
}

## This function takes a argument of the list returned by the makeCacheMatrix function.
## This function checks if an inverse value has been stored be makeCacheMatrix
## This function will either return the stored inverse value, or if not available calculate
## the inverse of the matrix value stored in makeCacheMatrix, and set the value of the inverse.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
