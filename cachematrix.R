## Combined these functions can be used to retrieve cached inverse or solve
## inverse of matrix if not already cached

## This function creates a list of four functions that can be used to get
## the matrix and set its inverse

makeCacheMatrix <- function(x = matrix()) {
    i = NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get, setinv = setinv,
         getinv = getinv)
}


## This function will check if the inverse is already assigned before
## calculating it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat, ...)
    x$setinv(i)
    i
}
