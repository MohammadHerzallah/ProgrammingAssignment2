## This is a pair of functions that cache the inverse of a matrix. Given that matrix inversion is usually a costly computation, there may be some benefit in caching the inverse of a matrix rather than computing it repeatedly.  Therefore, this pair of functions creates a matrix and caches its inverse, allowing the retrieval of the inverse from the cache.  If the inverse was not calcualted, then this will be done by the functions.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special matrix returned by the first function, `makeCacheMatrix`. If the inverse has already been calculated (with no changes to the matrix), then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
