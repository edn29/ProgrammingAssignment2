## Assignment is to write a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    q<-NULL
    set<-function(y) {
      x<<-y
      q<<-NULL
    }
    get<-function()x
    setinverse<-function(solve)q<<-solve
    getinverse<-function()q
    list(set=set,get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## This function computes the inverse of the special 
## makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    q<-x$getinverse()
    if(!is.null(q)){
      message("getting cached data")
      return(q)
    }
    data<-x$get()
    q<-source(data,...)
    x$setinverse(q)
    q
        ## Return a matrix that is the inverse of 'x'
}
