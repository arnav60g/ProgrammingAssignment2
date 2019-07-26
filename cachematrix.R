## The function makescacheMatrix() takes the matrix and inverse matrix
## from the global environment and caches it in function's environment.
## The cacheSolve function returns the inverse matrix and returns
## it if already stored in cache.

## The function returns a list of functions --> set and get 
## the argument(matrix) value to and from the cache.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        get <- function() x
        set.inv <- function(solve) s <<- solve
        get.inv <- function() s
        
        list(set = set, get = get,
             set.inv = set.inv,
             get.inv = get.inv)
}


## The function returns the value from cache if present or 
## calculates it and stores and returns it if on already present.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        s <- x$get.inv()
        
        if(!is.null(s)) {
                message("Returning Cached Data")
                return(s)
        }
        
        data <- x$get()
        s <- solve(data, ...)
        x$set.inv(s)
        s
        
}
