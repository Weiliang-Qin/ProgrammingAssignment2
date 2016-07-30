## This pair of functions aims to cache the inverse of a matrix. The first function, i.e.
## makeCacheMatrix, creates a special "matrix" object that can cache its inverse. 
## The second function, i.e. cacheSolve, computes the inverse of the special "matrix"
## returned by the first function. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse from the cache. 

## the argument x should be a square and invertible matrix
## this function returns a list which contains four functions as
##      NO 1: set the matrix
##      NO 2: get the matrix
##      NO 3: set the inverse
##      NO 4: get the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                # the <<- operator is used to assign a value to an object in an 
                # environment that is different from the current environment
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) inv <<- inverse
        get_inverse <- function() inv
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## the argument x is the output of makeCacheMatrix
## this function returns the inverse of the original matrix we fed makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inverse()
        
        # if the inverse already has been calculted, we get it from the cache and 
        # skip the subsequent calcultions
        if(!is.null(inv)) {
                message("extracting cached data")
                return(inv)
        }
        
        # if not, we have to calculate the inverse
        data <- x$get()
        inv <- solve(data, ...)
        
        # we then want to set the inverse calculated to cache by set_inverse function
        x$set_inverse(inv)
        inv
}
