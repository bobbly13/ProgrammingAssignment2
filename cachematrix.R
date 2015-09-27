## Functions used for the calculation of the inverse of a matrix
## For improved performance they provide a caching strategy meaning
## that it is only necessary to perform the inverse matrix calculation
## on the intial execution

## Constructs a list that represents a cacheing matrix instance.
## The list contains a set of functions for the setting and retrieval of
## the original matrix and it's inverse if available

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinversematrix <- function(inverseMatrix) i <<- inverseMatrix
  
  getinversematrix <- function() i
  
  list(set = set, 
       get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## Calculates the inverse of the supplied matrix cache, using the 
## cached version if available.
## It does not check for whether the supplied matrix is invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinversematrix()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        
        data <- x$get()
        i <- solve(data, ...)
        x$setinversematrix(i)
        i
}
