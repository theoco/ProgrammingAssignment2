## makeCacheMatrix.R
##
## Author: Ted Randall
## Date: 1/24/2015

## makeCacheMatrix - creates a new matrix which allow some
## matrix operations to be cached and reused. Eg. cacheSolve
##
## Usage: > myMatrix <- makeCacheMatrix(matrix(1:4,2,2))
##        > myMatrix$get()
##             [,1] [,2]
##        [1,]    1    3
##        [2,]    2    4

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    get <- function() x
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    getinverse <- function() inverse
    setinverse <- function(y) inverse <<- y
    
    list(set = set,
         get = get,
         getinverse = getinverse,
         setinverse = setinverse)
}


## cacheSolve - Solves a matrix created using makeCacheMatrix
## and returns it's inverse. If the matrix has previously been 
## solved, it will return the results from it's cache.
##
## Usage: > cacheSolve(mymatrix)
##             [,1] [,2]
##        [1,]   -2  1.5
##        [2,]    1 -0.5
##        > cacheSolve(mymatrix)
##        getting cached data
##             [,1] [,2]
##        [1,]   -2  1.5
##        [2,]    1 -0.5
 
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
    }
    inverse <- solve(x$get())
    x$setinverse(inverse)
    inverse
}