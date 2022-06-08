## The pair of functions below - 'makeCacheMatrix' and 'cacheSolve' are created 
## to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        set_Inverse <- function(inverse) Inv <<- inverse
        get_Inverse <- function() Inv
        list(set = set, get = get,
             set_Inverse = set_Inverse,
             get_Inverse = get_Inverse)

}


## The 'cacheSolve' function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix (above). If the inverse has already been calculated (and the 
## matrix has not changed), then 'cacheSolve' retrieves the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$get_Inverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        Inv_Mat <- x$get()
        Inv <- solve(Inv_Mat, ...)
        x$set_Inverse(Inv)
        Inv
        
}
