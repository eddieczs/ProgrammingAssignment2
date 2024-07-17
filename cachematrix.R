## This is a pair of functions that saves costly computing power by
## being able to cache the inverse of a matrix. 


## 'makeCacheMatrix' creates a special 'matrix' object that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the 'inverse_set' property
        inverse_set <- NULL 
        
        ## Method to set the matrix
        set <- function(y) {
                x <<- y
                
                ## reset the 'inverse_set' when the matrix is set
                inverse_set <<- NULL
        }
        
        ## Method to 'get' the matrix
        get <- function() x
        
        ## Method to 'set' the inverse
        setInverse <-function(inverse) inverse_set <<- inverse
        
        ## Method to 'get' the inverse
        getInverse <- function() inverse_set
        
        ## Return list of methods
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## `cacheSolve` computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_set <- x$getInverse()
        
        ## Return the cached inverse if it exists
        if (!is.null(inverse_set)) {
                message("getting cached data")
                return(inverse_set)
        }
        
        ## Get the matrix from th eobject
        mat <- x$get()
        
        ## compute the inverse
        inverse_set <- solve(mat,...)
        
        # Cache the inverse
        x$setInverse(inverse_set)
        
        ## Return the inverse
        inverse_set
}
