

## This function creates a Matrix object which creates a list with 4 values in it.

makeCacheMatrix <- function(matrix = numeric()) {
        m <- NULL
        set <- function(y) {           ## This function will set the value of matrix when called
                matrix <<- y
                m <<- NULL
        }
        get <- function() matrix       ## This function will return the existing value of the matrix when called.
        setinverse <- function(inverse) m <<- inverse  ## This function will calculate the value of the inverse of the matrix.
        getinverse <- function() m     ## This function will return the inverse of the matrix when called.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



cachematrix <- function(x, ...) {       
        m <- x$getinverse()             
        if(!is.null(m)) {                  ## verifying if the inverse if already cached
                message("getting cached data")   
                return(m)                 ## returning the cache value if it already exists.
        }
        data <- x$get()
        m <- solve(data, ...)            ## generating the inverse of the matrix and caching it for future use.
        x$setinverse(m)
        m
}
