## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This creates list of functions required to get chached matrix
#inverse passed in as argument.
makeCacheMatrix <- function(x = matrix()){
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseOfX) m <<- inverseOfX
    getInverse <- function() m
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## Write a short comment describing this function
#This function gets the cached inverse value of the passed in matrix
#if found in the cache otherwise caches it for later use.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse();
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        matrixData <- x$get()
        m <- solve(matrixData, ...)
        x$setInverse(m)
        m
}
