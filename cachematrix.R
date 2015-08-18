## The following functions create a special object to store a matrix and cache
## it's inverse.

## makeCacheMatrix creates a special "matrix", which is really a list containing
## functions that...
## 1. get the matrix 
## 2. set the matrix 
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve calculates the inverse of the specia "matrix" created with 
## makeCacheMatrix. However, it first checks to see if the inverse has already 
## been calculated, in which case it simply gets the inverse from the cache and
## skips the calculation. Otherwise, it calculates the inverse of the matrix and
## sets the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
        
}
