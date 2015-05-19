## This file contains 2 functions that will allow to cache the result of a
##computally expensive operation: calculating the inverse of a matrix


##  makeCacheMatrix creates a list containing functions to
## - set and set the value of the matrix
## - get and set the value of the computed inverse
makeCacheMatrix <- function(x = matrix()) {
    #inv stores the result of the computation
    inv <- NULL
    #set changes the matrix and invalidates the cache if it exists
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    #get returns the matrix
    get <- function() x
    #setinv stores the result of the computation
    setinv <- function(res) inv <<- res
    #getinv retrives the inverse from the cache variable "inv"
    getinv <- function() inv
    #return the list with the 4 functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## 2) cacheSolve takes the cached matrix and returns the value of the inverse
## - from the cache if it has already been computed
## - from a calculation (the result being put in the cache for further use)
cacheSolve <- function(x, ...) {
    # check if the inverse exists in the cache and return it if it does
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # else compute the inverse, store and return the result
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

