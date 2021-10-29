# Coursera Course "R Programming"
# Solution for Week 3 > Programming Assignment 2: Lexical Scoping
#
# @author Andrey Legayev

# Example usage:
# > a <- matrix(rnorm(2000^2), nrow=2000)
# > x <- makeCacheMatrix()
# > x$set(a)
# > n <- cacheSolve(x) # not cached run
# > n <- cacheSolve(x) # cached data returned
# getting cached data


# Create object for cached matrix calucations
makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function() x
    setCache <- function(v) cache <<- v
    getCache <- function() cache
    list(set = set, get = get,
         setCache = setCache,
         getCache = getCache)
}


# Get inversed matix, cached value may be returned
cacheSolve <- function(x, ...) {
    cache <- x$getCache()
    if(!is.null(cache)) {
        message("getting cached data")
        return(cache)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setCache(s)
    s
}
