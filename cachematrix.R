## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned by 
##   makeCacheMatrix above. If the inverse has already been calculated (and the 
##   matrix has not changed), then the cachesolve should retrieve the inverse from 
##   the cache.
##
##
##  makeCacheMatrix creates a vector that is a list of functions. The functions are to: 
##   set substitutes y for x in the makeCacheMatrix vector
##   get returns the vector from the makeCacheMatrix function
##   setmatrix stores the value of the input variable m in the makeCacheMatrix function
##   getmatrix returns the value of m in the makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
     m<-NULL
     set<-function(y){
          x<<-y
          m<<-NULL
     }
     get<-function() x
     setmatrix<-function(solve) m<<- solve
     getmatrix<-function() m
     list(set=set, get=get,
          setmatrix=setmatrix,
          getmatrix=getmatrix)
}




## cacheSolve first verifies 'm' as already stored by getmean. 
##   If getmean is 'Null" then message "getting cached data" is returned and a new
##        matrix is solved
##   If getmean was cached from the previous function the return(m) prints the 
##        solved makeCacheMatrix function

cacheSolve <- function(x=matrix(), ...) {
     m<-x$getmatrix()
     if(!is.null(m)){
          message("getting cached data")
          return(m)
     }
     matrix<-x$get
     m<-solve(matrix, ...)
     x$setmatrix(m)
     m
}

