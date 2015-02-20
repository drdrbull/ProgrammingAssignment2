## Coursera R Programming Module Week 2 Assignment
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
##
## These two functions allow for the inverse to be cached to save having to
## needlessly duplicate the calculation

## The macheCacheMatrix function encapsulate the cache: it keeps a copy of the raw data,
## and the solution (m) once it has been calculated.
## get() will return the raw data
## getsolve() will return the cached result
## setsolve() is used by the cacheSolve method to cache the result
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function returns the cached solution if it already exists, otherwise it
## calculates the inverse using solve, assigns the result to the cache and then
## returns it.
cacheSolve <- function(x, ...)  {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setsolve(m)
    m
}