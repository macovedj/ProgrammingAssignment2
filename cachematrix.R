## Inverting a matrix can be a time consuming process, especially
## when being done repeatedly.  Caching can quicken this process.
## The following functions will compute the inverse of a matrix 
## and cache it so that in the future it can access the cache 
## rather than repeat the computation

## makeCacheMatrix will create a special "matrix" that is actually
## a list of four functions that
## 1.) Set the value of the matrix
## 2.) Get the value of the matrix
## 3.) Set the value of the inverse
## 4.) Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve will compute the inverse of the special "matrix"
## unless it has already been computed and the matrix hasn't
## changed, in which case it will look up the inverse in the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting chached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
