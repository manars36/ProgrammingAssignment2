# Set of function, wich enables calculating and caching the inverse of a matrixes
# Functions assumes that matrixes are always positivly defined

## Function creating special matrix with cointains chached inverse of itself.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinvert <- function(invert) inv <<- invert
    getinvert <- function() inv
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)

}


## Function calculating or getting cached inverse matrix for matrixes created by 'makeCacheMatrix' function
## Warning: function works only with matrixes created by mentioned function.

cacheSolve <- function(x, ...) {
    inv <- x$getinvert()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinvert(inv)
    inv
}
