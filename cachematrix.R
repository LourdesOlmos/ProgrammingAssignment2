## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv  <-NULL
    set  <-function(y){
        x   <<-y
        inv <<- NULL
    }
    get  <-function()x
    setInverse <-function(inverse)inv <<- inverse
    getInverse <-function()inv
    list(set=set,
         get=get,
         setinverse=setInverse,
         getInverse=getInverse)

}


## Function wich computes the inverse of the special matrix created by
## makeCacheMatrix above. It calculates the inverse, and if the matrix doesn't
## change it'll retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv  <- x$getInverse()
    if (!is.null(inv)){
        message("GETTING CACHE DATA")
        return (inv)
    }
    mat  <-x$get()
    inv  <-solve(mat,...)
    x$setInverse(inv)
    inv
}

