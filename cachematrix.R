## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function, makeCacheInverse creates a special "vector", 
## which is really a list containing a function to
##  set the value of the matrix
##  get the value of the matrix
##  set the inverse of the matrix
##  get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inversex <- NULL
    set <- function(y) {
        x <<- y
        inversex <- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inversex <<- inverse
    getinverse <- function() inversex
    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## The following function calculates the inverse of the matrix in the special 
## "vector" created with the above function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
