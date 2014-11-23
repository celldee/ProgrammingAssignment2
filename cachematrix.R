## cachematrix.R
## This file contains a pair of functions that cache the inverse of a matrix when used together.

## makeCacheMatrix: This function takes a matrix and returns a list object (l) containing the following functions:
## l$setmatrix - caches the matrix to be inverted
## l$getmatrix - returns the matrix to be inverted
## l$setinvmatrix - caches the inverted matrix
## l$getinvmatrix - returns the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                setmatrix <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                getmatrix <- function() x
                setinvmatrix <- function(inverse) m <<- inverse
                getinvmatrix <- function() m
                list(setmatrix = setmatrix, getmatrix = getmatrix,
                     setinvmatrix = setinvmatrix,
                     getinvmatrix = getinvmatrix)
}


## cacheSolve: This function takes a list object (x) returned by the makeCacheMatrix function above and either
## returns the cached inverted matrix stored in x, or calculates the inverse of the matrix cached in x and
## stores it in x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvmatrix()
        if (!is.null(m)) {
                return(m)
        }
        mat <- x$getmatrix()
        m <- solve(mat, ...)
        x$setinvmatrix(m)
        m
}
