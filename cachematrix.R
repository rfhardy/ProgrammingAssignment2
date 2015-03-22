## R programming HW 2 submission
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
    ## initialize inverse value
    inv <- NULL

    ## set value of matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL # matrix has changed, reassign to NULL
    }

    ## get value of matrix
    get <- function() x

    ## set inverse of matrix
    setInverse <- function(inverse) inv <<- inverse

    ## get inverse of matrix
    getInverse <- function() inv

    ## return a list of all functions from above
    list(set = set, get = get,
        setInverse <- setInverse,
        getInverse <- getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## get inverse
    inv <- x$getInverse()
    
    ## if inverse exists, and is cached, return cached inverse
    if(!is.null(inv)) {
        message("Retrieving cached matrix.")
        return(inv)
    }

    ## otherwise, get matrix
    data <- x$get()

    ## compute inverse
    inv <- solve(data, ...)

    ## cache inverse of matrix
    x$setInverse(inv)

    ## return inverse
    inv
}
