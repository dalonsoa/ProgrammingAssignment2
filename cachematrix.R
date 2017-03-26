## These functions create a special matrix whose inverse can be chache as well
## as well as the function to calculate such inverse.

makeCacheMatrix <- function(x = matrix()) {
        # Makes a cache matrix whose invere can be store for future use.
        # The "set" function only updates the matrix and deletes the cache
        # inverse if it is different than the stored matrix.
        inverse <- NULL
        val <- x
        set <- function(y) {
                if(!identical(val, y)) {
                        val <<- y
                        inverse <<- NULL
                }
        }
        get <- function() val
        setinv <- function(inv) inverse <<- inv
        getinv <- function() inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


cacheSolve <- function(x, ...) {
        # Returns a matrix that is the inverse of 'x'. If there is a cache
        # version of the inverse, that one is returned. Otherwise, the inverse
        # is calculated and the result stored in the cache matrix.
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
}
