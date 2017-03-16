## cacheSolve and makeCacheMatrix allow for caching of matrix inverse
## computations.

## Creates a special matrix, which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 1. set the value of the inverse
## 2. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(mean) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setmean = setinv,
         getmean = getinv)
}


## Calculates the inverse of a matrix with cached inverse.
## If the inverse was already cached, it will return the already calculted
##  inverse. Otherwise, it will calculated the inverse, cache it, and
##  return it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message('getting cached data')
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
}
