## Put comments here that give an overall description of what your
## functions do

## OK, after reading forums for half a dozen hours,
## here is what I understand about what these functions must do

## makeCacheMatrix returns a list of functions applied to a provided matrix, 
## and 3 of 4 of them (get, setinv, getinv) are used in cacheSolve
## which stores an inverted provided matrix and returns it when necessary
## (set can be used to simply change 

## Write a short comment describing this function
 
## makeCacheMatrix returns list of functions in a different environment every call,
## so that when you call cacheSolve, it provide these helper functions
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## m is a matrix that you 
    set <- function(y) {
                x <<- y
                m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve   
    getinv <- function() m
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}
## Write a short comment describing this function

## cacheSolve checks (via x$getinv) if a matrix provided to makeCacheMatrix already
## has it's inverted matrix, and if yes, it returns it,
## and if not, it generates it via x$setinv

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("return cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
