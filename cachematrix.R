## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL

            # set the value of the matrix
            set <- function(y) {
                            x <<- y
                            inv <<- NULL
                        }

            # get the value of the matrix
            get <- function() x

            # set the inverse of the matrix
            setInverse <- function(inverse) inv <<- inverse

            # get the inverse of the matrix
            getInverse <- function() inv

            # return a list containing all functions
            list(set = set, get = get,
                          setInverse = setInverse,
                          getInverse = getInverse)
        }


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
            # Return a matrix that is the inverse of 'x'
            inv <- x$getInverse()

            # if the inverse is already cached, return it
            if (!is.null(inv)) {
                            message("getting cached data")
                            return(inv)
                        }

            # otherwise, compute the inverse
            data <- x$get()
            inv <- solve(data, ...)

            # cache the inverse
            x$setInverse(inv)

            # return the inverse
            inv
        }
