## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get, 
                setInverse = setInverse, 
                getInverse = getInverse)
}

## Write a short comment describing this function
## cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if (!is.null(i)){
            message("getting cached inverse")
            ## Return a matrix that is the inverse of 'x'
            return(i)
        }
        i <- solve(x$get(), ...)
        x$setInverse(i)
        ## Return a matrix that is the inverse of 'x'
        i
}
