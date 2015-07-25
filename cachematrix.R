## makeCacheMatrix creates an object (a list)
## that represents a matrix. This object (list)
## is able to cache the inverse of the matrix.

## cacheSolve takes a makeCacheMatrix object and
## either returns the cached inverse (if present)
## or solves it and then returns the inverse.

makeCacheMatrix <- function(x = matrix()) {
## Return a special "matrix" object that can cache
## its inverse
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inv <<- inv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

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
