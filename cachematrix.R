## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
## cacheSolve: This function computes the
## inverse of the special "matrix" returned by makeCacheMatrix above. If
## the inverse has already been calculated (and the matrix has not
## changed), then the cachesolve should retrieve the inverse from the
## cache.

## Can make an object with this Function that has "sub-functions" that
## can, set, get the matrix and also set and get the inverse of the
## matrix.

makeCacheMatrix <- function(x=matrix()) {

    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
        print("setting new matrix")
    }
    get <- function() x
    setinv <- function(invtbset) inv <<- invtbset
    getinv <- function() inv

    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Checks if object has an inverse cached, otherwise computes the inverse.

cacheSolve <- function(x,...){
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    message("computing the inverse")
    inv <- solve(x$get())
    x$setinv(inv)
    inv
}
