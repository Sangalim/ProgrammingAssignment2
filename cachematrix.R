## These functions try to reduce the ammount of CPU used when inverting a matrix
## by storing the value of the solve() operation

## This function declares a 'extedend' matrix that also stores its inverse
## and calculates it 'on demand'

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverted <- function(inverted) i <<- inverted
    getinverted <- function() i
    list(set = set,  get = get, setinverted = setinverted, getinverted = getinverted)
}


## This function gets a matrix created by makeCacheMatrix
## and returns the inverted matrix. 

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverted()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    message("calculating data")
    data <- x$get()
    i <- solve(data)
    x$setinverted(i)
    i
}
