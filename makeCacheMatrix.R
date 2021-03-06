
   makeCacheMatrix <- function(matrix = numeric()) {
        m <- NULL
        set <- function(y) {
                matrix <<- y
                m <<- NULL
        }
        get <- function() matrix
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
