## Matrix inversion is usually a costly computation. By using this function we can compute it simply.
## These following 2 functions allows you to cache the invese of matrix.

## The function named "makeCacheMatrix" creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invert <- NULL
    set <- function(y){
        x <<- y
        invert <<- NULL
    }
    get <- function()x
    setinverse <- function(inverse) invert <<- inverse
    getinverse <- function()invert
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This functin computes the inverse of the special matrix that returned by "makeCacheMatrix" function.

cacheSolve <- function(x, ...) {
    invert <- x$getinverse()
    if(!is.null(invert)){
        message("getting cached data")
        return(invert)
    }
    data <- x$get()
    invert <- solve(data, ...)
    x$setinverse(invert)
    invert
}
