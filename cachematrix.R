## Functions used to cache inverse of matrix

## makeCacheMatrix
# set/get the value of matrix
# set/get the value of inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
  }
  get <- function(){
    x
  }
  setinverse <- function(inverse){
    inv <<- inverse
  }
  getinverse <- function() {
    inv
  }
  list(
        set=set, 
        get=get, 
        setinverse=setinverse, 
        getinverse=getinverse
      )
}

## makeCacheMatrix
# this functions checks if inverse of matrix is already computed or not.
# if yes then gets the inverse and returns
# if no then compute and stores the result in cache for future

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}