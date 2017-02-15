## Definition of a matrix like object which stores both
## a data matrix and also its inverse if it is already calculated. 

## makeCacheMatrix constructs an object that stores
## a data matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## set default values for the inverse
        inv <- NULL
        ## x setter definition
        setx <- function(y) { x <<- y }
        ## x getter definition
        getx <- function() x
        ## inv.x setter definition
        setinv <- function(val) inv <<- val
        ## inv.x getter definition
        getinv <- function() inv
        res <- list(set = setx, get = getx,
             setinv = setinv, getinv = getinv)
        ## return a list of setters and getters
        return(res)
}

## Testing functions
#matOb<-makeCacheMatrix(diag(1,3,3))
#matOb$get()
#matOb$set(diag(3,3,3))
#matOb$get()

## cacheSolve takes a makeCacheMatrix object as argument, extract
## the data matrix defined inside and calculates its inverse
## if it is not already stored in the object.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        ## checks if the inverse has been already calculated
        if ( ! is.null(inv) ) {
                message("Getting inverse from cache")
                res <- inv 
        } else {  
                data.m <- x$get()
                res <- solve(data.m, 
                             ...)
        }
        x$setinv(res)
        ## Return a matrix that is the inverse of 'x'
        return(res)
}


## Testing functions
matOb<-makeCacheMatrix(diag(1,3,3))
cacheSolve(matOb)
#cacheSolve(matOb)
## Testing A*X = I. Where A is a matrix, X its inverse and I the unitary diagonal
## matrix.
matOb$get()%*%matOb$getinv()

m1 <- matrix(c(1/2,-1/4,-1,3/4),2,2)
solve(m1)
m <- makeCacheMatrix(m1)
cacheSolve(m)
