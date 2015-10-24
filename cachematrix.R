## cachematrix.R is 2 functions.  
# The 1st, makeCacheMatrix returns a matrix that can cache its inverse.
# The 2nd, cacheSolve computes the inverse of the matrix from makeCacheMatrix, or if the
# inverse already exists, and the matrix hasn't changed, then it retrieves the inverse.


## This function creates a special "matrix" function that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    IM <- NULL
    set <- function(y) {
        x <<- y
        IM <<- NULL
    }
    
    get <- function() x
    setInverse <- function(InverseM) IM <<- InverseM
    getInverse <- function() IM
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
} 


## This function computes the inverse of the special "matrix" returned by the
# makeCacheMatrix above.  If the inverse matrix has already been 
# computed (and the matrix hasn't changed) then this (cacheSolve) function
# retrieves the inverse from the cache instead of computing the inverse.

cacheSolve <- function(x, ...) {
    IM <- x$getInverse()
    if(!is.null(IM)) {
        message("getting cached inverse matrix")
        return(IM)
    }
    OriginalMatrix <- x$get()
    IM <- solve(OriginalMatrix, ...)
    x$setInverse(IM)
    IM
}

