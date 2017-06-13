## This first function sets the value of a matrix, then 
## gets the value of the matrix, then calculates the inverse
## and then gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- matrix(nrow = 0, ncol = 0)
        set <- function(y) {
                x <<- y
                m <<- matrix(nrow = 0, ncol = 0)
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the matrix created in the previous function
## but it first checks to see if the inverse has already been calculated. If so, it takes
## the cached value instead of calculating it again. If it does need to calculate it does
## and then caches the value of the inverse in the cache via the setinverse function

cacheinverse <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
