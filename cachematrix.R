## Programming Assignment 2: Lexical Scoping
## The assignment is to write a pair of functions that cache the inverse of a matrix

## This function set the value of the matrix, get that, set value of inverse of matrix and get it 

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }                                               ######set the value of the matrix
  
  get <- function() x                             ######get the value of the matrix
  setinverse <- function(inverse) m <<- inverse   ######set value of inverse of matrix
  getinverse <- function() m                      ######get value of matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}                                                 ######store information in a list
}


## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips 
## the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }                                 ##Display a message if m is not null and returns the value of m
  data <- x$get()
  m <- solve(data, ...)             ##the solve function allows to invert a matrix
  x$setinverse(m)
  m
}
