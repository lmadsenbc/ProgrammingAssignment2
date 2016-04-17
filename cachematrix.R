## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Completed by lmadsenbc for Assignment 2 

# Matrix inversion is usually a costly computation and there may be 
# some benefit to caching the inverse of a matrix rather than compute it 
# repeatedly (there are also alternatives to matrix inversion that we will 
# not discuss here). Your assignment is to write a pair of functions that 
# cache the inverse of a matrix.


# makeCacheMatrix creates a list containing a fuction to :
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y){
                x <<- y
                inver <<- NULL
        }
        get <-  function()x
        setInverse <- function(inverse)  inver <<- inverse
        getInverse <- function()inver
        list(set = set,get=get, setInverse=setInverse,getInverse=getInverse)
        
}


## Write a short comment describing this function
# cacheSolve returns the inverse of the provided matrix
# It will first check if the inverse of the matrix has already been computed
# If it has, the cached solution will be returned.  Otherwise it will compute
# using the setInverse fuction

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getInverse()
        
        # check for cached solution
        if(!is.null(inver)){
                message("returning previously cached solution")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data)
        x$setInverse(inver)
        inver
        
        

}
