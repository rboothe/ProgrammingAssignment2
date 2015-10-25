## These functions enable the user to cache potentially time consuming calculations
## such as the inversion of a matrix

## The makeCacheMatrix function creates a list containing functions to do the
## following: 
# set the values of the matrix
# get the get the matrix
# set the values of the inverse of the matrix
# get the inverse matrix

makeCacheMatrix <- function(x = matrix(c(1,2,3,0,1,4,5,6,0), nrow = 3, ncol = 3, byrow = TRUE)
){
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the matrix created by the 
## makeCacheMatrix function above

cacheSolve <- function(x, ...){
    i <- x$getmean()
    if(!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
        ## Return a matrix that is the inverse of 'x'
}
