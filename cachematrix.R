## These functions will create a special matrix-like object that
## can store the cache of its inverse.

## The matrix-caching object creation function.  It takes a matrix
## and returns a list containing functions to manipulate the object
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Set the stored inverse to NULL on creation
    inv <- NULL  
    ## When we change the matrix, we also reset the cached inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## Get the matrix out
    get <- function() x  
    ## Set the inverse in the cache
    setinv <- function(matrixInverse) inv <<- matrixInverse
    ## Get the cached inverse
    getinv <- function() inv 
    ## Return a list of our manipulation functions
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function calculates the inverse of the matrix in our
## special object if it isn't in the cache

cacheSolve <- function(x, ...) {
    ## Try to pull out the inverse
    inv <- x$getinv()
    ## If we got it, we're done
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## Get our matrix out
    ourmatrix <- x$get()
    ## Solve for the inverse
    inv <- solve(ourmatrix)
    ## Put the inverse in the cache
    x$setinv(inv)
    ## Return the inverse of the matrix in our object
    inv
}
