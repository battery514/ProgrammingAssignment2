## Assignment 2 - These functions cache a matrix
## and then inverse the matrix.

## This function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## This function computes the inverse of the matrix returned 
## in the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<- x$getinv()
        if(!is.null(i)) {
                message("Getting Cached Data")
                return(i)
        }
        data <- x$get()
        i<- solve(data, ...)
        x$setinv(i)
        i
        
}
