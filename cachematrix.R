## These two functions make use of lexical scoping to cache costly
## matrix inversion process. 
## The first function creates the cache object while the second retrieves
## the cached data from the first. 


## This function creates a "special matrix" that can cache the inverse matrix
## operation. More precisely, this function is metaphorical for a class that
## creates objects having x(matrix) and inv (its inverse) as attributes and
## getters and setters as methods

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function retrieves the cashed data made by makeCacheMatrix objects
## or sets it if the inverse wasn't calculated yet

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
