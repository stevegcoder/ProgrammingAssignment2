## Put comments here that give an overall description of what your
## functions do
## These functions compute the inverse of a matrix but also cache the result
## so subsequent calls will use the cached result

## Write a short comment describing this function
# This function will return a special matrix that has set and get for the original matrix,
# and setinverse and getinverse for the inverse of the original matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# This function computes the inverse of a matrix x.  If the cached result exists for x,
# then the cached result is returned.  Otherwise it is calculated and returned

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
