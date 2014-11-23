## Assignment 2 - Caching the Inverse of a Matrix

## Similar to the example function of "Caching the Mean of a Vector"
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function( mat = matrix() ) {
        
        inv <- NULL
        set <- function( matrix ) {
                mat <<- matrix
                inv <<- NULL
        }
        get <- function() {
                mat
        }
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        ## Get the inverse of the matrix
        getInverse <- function() {
                inv
        }
        ## Return a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Matrix of the inverse 'x'
        mat <- x$getInverse()
        if( !is.null(mat) ) {
                message("getting cached data")
                return(mat)
        }
        data <- x$get()
        ## Matrix Manipulation
        mat <- solve(data) %*% data
        ## Set the inverse to the object
        x$setInverse(mat)
        ## Now return the matrix
        mat
}
