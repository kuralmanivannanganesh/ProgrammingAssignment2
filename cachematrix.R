##Build a special matrix that will calculate and cache the inverse of a given matrix

## Returns a Special matrix list that has the following methods
## set() sets given matrix
## get() returns the matrix
## setinversematrix() sets the inverse of the matrix
## returns the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inve <- NULL
    set <- function(maty){
        x <<- maty
        inve <<- NULL
    }
    get <- function() x
    setinversematrix <- function(inver) inve <<- inver
    getinversematric <- function() inve
    list(set = set, get = get,
         setinversematrix = setinversematrix,
         getinversematric = getinversematric)
    
}


## Calculates the inverse of the given special matrix and caches the result

cacheSolve <- function(x, ...) {
    inve <- x$getinversematric()
    if(!is.null(inve)){
        return(inve)
    }
    originalmat <- x$get()
    inversemat <- solve(originalmat)
    x$setinversematrix(inversemat)
    inversemat
}
