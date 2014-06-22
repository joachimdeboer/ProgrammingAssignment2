## This function creates a special Matrix object
## that caches the results so the next lookup
## of the same matrix will not "calculate" but use
## the cached value
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    #return the available functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function computes the inverse of a matrix
## only if it's not calculated before
## if calculated before; it returns it from cache
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        # compete the inverse
        m <- solve(data, ...)
        # cache the result
        x$setinv(m)
        m
}