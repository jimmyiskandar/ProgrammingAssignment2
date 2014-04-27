## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeChaceMatrix returns a list of functions:
## a. set: cache the new matrix and set inverse to null
## b. get: get the cached matrix
## c. setinverse: cache the inverse
## d. getinverse: get the cached inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve returns an inverse of a matrix
## It tries to get the cached inverse 
## If successful, it returns the cached inverse
## If not, it solves the inverse, caches it, and returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## test 1: identity matrix
x <- makeCacheMatrix(diag(5));
cacheSolve(x);

## test 2: some matrix
x <- makeCacheMatrix(matrix(c(1,2,3,4,5,6,7,8,14),3,3));
invx <- cacheSolve(x);
## x multiplied by its inverse is close to the identity matrix
x$get() %*% invx;



