## This pair of fucntions caches the inverse of a matrix
## as matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## initialize the value of the matrix to NULL
        i <- NULL
        ## initialize the inverse of the matrix (cached value) to NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        ## Set the value of inverse of matrix 
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
        
##  This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        ## Check if inverse of x already exists
        if(!is.null(i)) {
                message("getting cached data")
                ## Display a message
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        ## calculate the inverseof matrix
        x$setinverse(i)
        i

}
