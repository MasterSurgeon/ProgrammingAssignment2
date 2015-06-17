## First function (makeCacheMatrix)is to set and hold a square matrix. Second function 
## (cacheSolve) will find the inverse of the matrix stored in the first function.

## makeCacheMatrix: Function containing 4 sub-functions for setting/ storing a matrix.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y = matrix()) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inv <<- solve
      getinverse <- function() inv
     
      ##Storing the 4 sub-functions      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve: Function to find the inverse of the matrix loaded into function above

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            return(inv)
      }
      ## If Inv == NULL, continues below.
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
