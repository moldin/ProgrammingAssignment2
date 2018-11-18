## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix(x) provides a list of functions together with an environment where the 
## assigned matrix 'x' is stored. The function setinv(inv) allows the inverse 'inv' to 'x'
## to be stored in its environment. The function getinv() provides access to the 'inv' which is NULL
## if it has not yet been set. The original matrix 'x' is provided by get()
##
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() x
    
    setinv <- function(mInv) inv <<-mInv
    getinv <- function() inv
    
    list(get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## The method cacheSolve(x) calculates the inverse to the matrix 'x'. 'x' must be the
## output of the above function makeCacheMatrix. It checks if the inverse has already been cached
## and uses the cached value in that case.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$get()
    inv <- x$getinv()
    ## If 'inv' is not null it has previously been calculated and we can return the cached value.
    if(!is.null(inv)){
        return(inv)
    }
    inv <- solve(m)
    x$setinv(inv)
    inv
}
