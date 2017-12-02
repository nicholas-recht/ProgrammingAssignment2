## The functions in this file are used to cache the inverse of a matrix
## to save from having to do this potentially expension operation multiple
## times. 


## This function creates a list from the given matrix
## with the get, set, getinv, and setinv functions defined.
## This list can then be used by the cacheSolve function defined
## below. 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



## This function computes the inverse of the given matrix and caches
## the result so future calls for the same matrix don't need to
## compute the inverse again. 
cacheSolve <- function(x, ...) {
    
    # check the cache first
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    # if we don't have a value then we
    # need to calculate and store it here
    m <- x$get()
    
    inv <- solve(m, ...)
    
    x$setinv(inv)
    inv
}
