## A pair of functions that cache the inverse of a matrix is written below.


## This function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     invr <- NULL
     set <- function(y){
     x <<- y
     invr <<- NULL  #to initialize inverse as null
    }
     get <- function() x     #function to get matrix x
     setInverse <- function(inverse) invr <<- inverse
     getInverse <- function() invr            #to get inverse of the matrix
     list(set = set, get = get, 
                   setInverse = setInverse, 
                   getInverse = getInverse)
    }


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     invr <- x$getInverse()
     if(!is.null(invr)){                #to check whether inverse is null
         message("getting cached data")
         return(invr)                  #returns inverse value
       }
     dat <- x$get()
     invr <- solve(dat)
     x$setInverse(invr)
     invr
   }

