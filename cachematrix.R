## Contents:
##  makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
##  cacheSolve:      Computes the inverse of the special "matrix" returned by makeCacheMatrix.

################################################################################
##
##  Creates a special "matrix" object that can cache its inverse.
##
##  Given a matrix, this function wraps it with accessors:
##    1. set:    sets the value of the matrix
##    2. get:    gets the value of the matrix
##    3. setinv: sets the value of its inverse
##    4. getinv: gets the value of its inverse
##
##  Returns: a wapper for the matrix, implemented as a list of the above four
##  functions.
##
makeCacheMatrix <- function(x = matrix()) {
    cachedInv <- NULL
    set <- function(newX) {
        x <<- newX
        cachedInv <<- NULL
    }
    get <- function()
        x
    setinv <- function(inverse)
        cachedInv <<- inverse
    getinv <- function()
        cachedInv

    list(
        set = set,
        get = get,
        setinv = setinv,
        getinv = getinv
    )
}

################################################################################
##
##  Computes the inverse of the special "matrix" returned by makeCacheMatrix
##  above. If the inverse has already been calculated (and the matrix has not
##  changed), returns the inverse from the cache.
##
##  Returns: a matrix that is the inverse of the one wrapped by x.
##
cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}

## Test Matrices

# Rotation of 30 deg about positive z axis.
a <- matrix(
    c(
        0.86602540378443864676372317075294,
        0.5,
        0,
        -0.5,
        0.86602540378443864676372317075294,
        0,
        0,
        0,
        1
    ),
    nrow = 3,
    ncol = 3,
    byrow = TRUE
)

# Rotation of 60 deg about positive x axis.
b <- matrix(
    c(
        1,
        0,
        0,
        0,
        0.5,
        0.86602540378443864676372317075294,
        0,
        -0.86602540378443864676372317075294,
        0.5
    ),
    nrow = 3,
    ncol = 3,
    byrow = TRUE
)
