## Put comments here that give an overall description of what your
## functions do

## This function creates a 'matrix' that is a list containing functions for caching inverse

makeCacheMatrix <- function(x = matrix()) {
    cachedinverse <- NULL
    set <- function(y) {
        x <<- y
        cachedinverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cachedinverse <<- inverse
    getinverse <- function() cachedinverse
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## This function either returns cached inverse matrix or calculates it and caches inverse, while returning the result

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(x)
    x$setinverse(inverse)
    inverse
}
