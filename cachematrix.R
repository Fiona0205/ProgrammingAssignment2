## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## Set the value of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## Get the value of the matrix
    get <- function() x
    ## Set the value of the inverse matrix
    setinverse <- function(inverse) i <<- inverse
    ## Get the value of the inverse matrix
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## This function return a matrix which is the inverse of 'x'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    ## If the inverse has already been calculated, the cachesolve 
    ## retrieve the inverse from the cache
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## If not, this function computes the inverse of the special 
    ## "matrix" returned by makeCacheMatrix
    data <- x$get()
    ## Compute the inverse of matrix
    i <- solve(data, ...)
    ## Set the value of inverse matrix
    x$setinverse(i)
    i
}
