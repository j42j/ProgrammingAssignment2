## Programming Assignment 2 for "R Programming"
## by J42J 06/10/14

## This fxn is able to 1) Set the value of a matrix; 2) Get the value of a 
## matrix; 3) Set the inverse of a matrix; 4) Get the inverse of matrix;
## Returns a list containing these functions

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This fxn calculates the inverse of the "matrix" created by
## the fxn makeCacheMatrix. If the inverse is already cached, that 
## value of the inverse is returned. Else, the inverse is calculated and
## the value cached and returned. 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
