
## Put comments here that give an overall description of what your
## functions do
##cache a matrix inverse

## Write a short comment describing this function
## "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
    inversion <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInversion <- function(inverse) inversion <<- inverse
    getInversion <- function() inversion
    list(set= set, get= get, setInversion= setInversion, getInversion= getInversion)
}


## Write a short comment describing this function
## calculate the mean of the vector created by the function above 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inversion <- x$getInversion()
    if(!is.null(inversion)){
        message("getting cached data")
        return(inversion)
    }
    m <- x$get()
    inversion <- solve(m, ...)
    x$setInversion(inversion)
    inversion
}
