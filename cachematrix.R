##makeCacheMatrix is very similar to the makeVector given from the example, I only changed
## the names of getmean and setmean to getinverse and setinverse

##The first function, `makeCacheMatrix` creates a special "matrix", which is
##really a list containing a function to

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse
##4.  get the value of the inverse





## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { # x is the matrix you want to invert
      m <- NULL  
      set <- function(y) {
            x<<- y
            m<<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list( set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)

}


## this function takes in the matrix that we ran through makeCacheMatrix and computes it's inverse
## if the inverse has already been computed, it grabs the cached data and returns that

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m<-x$getinverse() ##if we have already computed the inverse, m will return NULL, otherwise we can grab the cached version
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data<-x$get()
      m<-solve(data, ...) ## this is the main change of the program, solve computes the inverse
      x$setinverse(m)
      m
      
}
