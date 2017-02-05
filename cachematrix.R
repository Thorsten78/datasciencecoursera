##creating special vector, which is a list containing several functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculation and caching the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Check if the inverse is already calculated and cached
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if inverse not already calculated, get the matrix from special object x
        data <- x$get()
        ## creating the inverse
        m <- solve(data, ...)
        ## set the inverse to the special object
        x$setinverse(m)
        m
}
