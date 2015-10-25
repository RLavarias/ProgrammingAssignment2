## These two functions allow for cacheing a large matrix computation
## to allow for saving time while working with larger data computations

## This function creates and sets the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## This function computes the inverse, then caches the data and returns the matrix inverse

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m        ## Return a matrix that is the inverse of 'x'
}
