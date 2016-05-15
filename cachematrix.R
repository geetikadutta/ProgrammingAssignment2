##==================================================================
## Coursera R Programming : Programming Assignment 2 (caching)
## This script contains two functions makeCacheMatrix and cacheSolve 
## to compute and cache the inverse of a matrix
##==================================================================


## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Creates a "special matrix" object
        ##
        ## Args :
        ##      x: square invertible matrix
        ## Returns :
        ##      special "matrix" object which is a list of functions to 
        ##         1. set the value of the matrix
        ##         2. get the value of the matrix
        ##         3. set the value of the inverse
        ##         4. get the value of the inverse
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned
## by the makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve retrieves the
## inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## 
        ## Args :
        ##      x: special "matrix" object created using "makeCacheMatrix" function
        ##
        ## Returns :
        ##      The inverse of a matrix x
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
