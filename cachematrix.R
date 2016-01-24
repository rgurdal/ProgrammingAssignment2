## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly. Your 
## assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix(data=1:4,nrow=2)) {
        mcm <- makeCacheMatrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv<- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.
## (Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
