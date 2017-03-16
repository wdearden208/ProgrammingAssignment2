## cacheSolve and makeCacheMatrix allow for caching of matrix inverse
## computations.

## Creates a special matrix, which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 1. set the value of the inverse
## 2. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set=set, get=get, 
         setinv=setinv, 
         getinv=getinv)
}


## Calculates the inverse of a matrix with cached inverse.
## If the inverse was already cached, it will return the already calculted
##  inverse. Otherwise, it will calculated the inverse, cache it, and
##  return it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data.")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
