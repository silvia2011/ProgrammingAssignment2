## Caching the inverse of a matrix

# This function creates a special "matrix" object that can cache its inverse.
# Creates nested get/set functions
# Input: invertible matrix
# Output: list of get/set methods for the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setmatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getmatrix <- function() x
    setInv <- function(solve) inv <<- solve
    getInv <- function() inv
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setInv = setInv,
         getInv = getInv)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse is already calculated (and the matrix has not changed),
# then this function should retrieve the inverse from the cache.
# Input: 'special' matrix
# Output: Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    
    # Return inverse if already calculated
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # come here to calculate the inverse if not calculated 
    data <- x$getmatrix()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
