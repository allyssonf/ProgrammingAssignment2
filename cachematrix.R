## R Function to calculate an inverse of a square matrix
## and cache the result for performance improvements

## Function to create a special "matrix" object that
## can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Global cached inverse matrix
    cachedInvertedMatrix <- NULL
    set <- function(y) {
        x <<- y
        # If a new matrix was entered whe shall
        # remove previous cached data.
        cachedInvertedMatrix <<- NULL
    }
    get <- function() x
    # Set the inverse matrix of x
    setInverse <- function(inverse) cachedInvertedMatrix <<- inverse
    getInverse <- function() cachedInvertedMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse was already 
## calculated the value from cache is returned.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    # If the matrix was already inverted return
    # its cached value.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # If the matrix was not inverted, retrieve and
    # calculates its inverse.
    data <- x$get()
    m <- solve(data, ...)
    # Set matrix inversed value for caching. Next
    # time the cacheSolve is called for the same
    # matrix there will be no need to calculate 
    # its inverted value again.
    x$setInverse(m)
    m
}
