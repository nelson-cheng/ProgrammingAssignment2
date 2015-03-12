## R Programming Exercise 2
##
## The following functions creates a 'special matrix' which caches the inverse matrix. This allows speedy return of
## the inverse if it has already been calculated. It assumes the underlying matrix is not singular.

## makecacheMatrix() creates a vector object that wraps an underlying matrix.
## The vector contains the functions to get/set the matirx and that for its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    myInv <- NULL
  
    set <- function(y) {
        x <<- y
        myInv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inv) myInv <<- inv
    
    getinv <- function() myInv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve() takes a special matrix and returns its inverse. It will perform
## the calculation if it is not cached.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    inv <- x$getinv()
    if (!is.null(inv)) {
         message("getting cached data")
         return(inv)
    }
    inv <- solve(x$get())
    x$setinv(inv)
    inv
}
