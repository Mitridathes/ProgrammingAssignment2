## The combination of these two functions is able to calculate and cache the inverse of a matrix. If the s
## inverse wa previously cached, it won't need to calculate it again.

## makeCacheMatrix function creates a "special matrix" which is really a list which contains metadata of 
## a matrix. This list stores somefunctions that may return either the matrix or its inverse if it was 
## calculated and cached before.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function (y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function (inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function either calculates the inverse of a matrix and caches it in the special matrix or
## just gets it from the special matrix if it was cached before

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)                        
        x$setinv(i)
        i
}
