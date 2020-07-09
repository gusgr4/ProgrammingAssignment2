## cachematrix.R is divided in two functions to receive a matrix, invert it
## and return its value.

## makeCacheMatrix returns a list of functions that defined within itself.
## These functions either give a value for the matrix or prepare the matrix
## for further calculation.

makeCacheMatrix <- function(x = matrix()) {
     im <- NULL
     get <- function() x
     set <- function(y) {
          x <<- y
          im <<- NULL
     }
     setinverse <- function(inverse) im <<- inverse
     getinverse <- function() im
     
     list(get = get, set = set, setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve checks if there is a matrix value already cached so it does
## not waste memory to calculate the inverse again. If no matrix is cached
## it proceeds to calculate the inverse of the given matrix and returns it.

cacheSolve <- function(x, ...) {
     im <- x$getinverse()
     
     if(!is.null(im)) {
          message("Getting cached data")
          return(im)
     }
     
     mat <- x$get()
     im <- solve(mat)
     x$setinverse(im)
     im
}