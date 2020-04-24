## At first my makeCacheMatrix function creates a special "matrix", which is really a list containing a function to
## 1 . set the values of the matrix
## 2 . get the values of the matrix
## 3 . set the values of the inverse
## 4 . get the values of the inverse
## The cacheSolve function calculates the inverse of the special "matrix" created with the makeCacheMatrix function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the setInverse function.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <- NULL
    }
    get <- function(){x}
    setInverse <- function(inverse){i <<- inverse}
    getInverse <- function() i
    list(set=set , get=get , setInverse=setInverse , getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated,then the cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)){
            message("Getting inverse matrix")
            return(i)
        }
        mat <- x$get()
        i <- solve(mat , ...)
        x$setInverse(i)
        return(i)
}



