## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The following few lines of code are few functions that will cache and compute
## the inverse of matrix by creating a special vector. The object is created as a
## list with help of get and set function and scoping.

makeCacheMatrix <- function(x = matrix()) {
    
     ##The inverse will be started as NULL
     
    inver <- NULL
    
    ## Now,I assign new argument which will be my stored value and then
    ## reset my calculation of inverse
    
    set <- function(i){
        x <<- i
        inver <<- NULL
}
    ## Returns the internal object
    
    get <- function() x
    
    ## Now, get and set function for inverse
    
    setInverse <- function(inv) inver <<- inv
    getInverse <- function() inver
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}

## Write a short comment describing this function

##In the next few lines of code, we will calculate the inverse of received matrix
## and then cache the result in the makeCacheMatrix object.
##If cache already exists, then it will return the cache result.

cacheSolve <- function(x, ...) {
    
        ## Here, we are returning a matrix which is the inverse of x.
        
        inver <- x$getInverse()
        if(!is.null(inver)) {
            message("getting cached data!!!")
            return(inver)
        }
        data <- x$get()
        
        ## Here, we are calculating the inverse
        
        inver <- solve(data)
        x$setInverse(inver)
        return (inver)
}
