## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

invrt <- NULL
        set <- function(y) {
                x <<- y
                invrt <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invrt <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
        invrt <- x$getInverse()
        if (!is.null(inv)) {
                message("Retrieving  cached Matrix")
                return(invrt)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(invrt)
        invrt
}
