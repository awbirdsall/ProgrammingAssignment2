## cachematrix.R:
## Cache inverse of a matrix using a `cache.matrix`, a list of functions
## describing how to get and set the matrix and its inverse. Calling
## `cacheSolve` on a `cache.matrix` will calculate and set the inverse matrix
## ("solve" the matrix) if the inverse has not been calculate, or just return
## the inverse of the matrix if it has already been cached.

## Given matrix, make a list of functions describing a cache.matrix.

makeCacheMatrix <- function(cache.matrix = matrix()) {
        inv <- NULL
        set <- function(y) {
            cache.matrix <<- y
            inv <<- NULL
        }
        get <- function() cache.matrix
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Given cache.matrix, either return cached inverse, or calc and set inverse.

cacheSolve <- function(cache.matrix, ...) {
        inv <- cache.matrix$getinv()
        if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- cache.matrix$get()
        inv <- solve(data, ...)
        cache.matrix$setinv(inv)
        inv
}
