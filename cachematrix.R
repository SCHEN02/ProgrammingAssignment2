## makeCacheMatrix creates a special matrix object
## cacheSolve calculates the inverse of the matrix created previously
## but it first checks to see if the inverse has already been found

## makeCacheMatrix creates a special matrix object and makes a 
## list to set the matrix values, get the matrix values,
## set the inverse of the matrix, and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function(){
    x
  }
  setInverse <- function(inverse){
    i <<- inverse
  }
  getInverse <- function(){
    i
  }
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## cacheSolve calculates the inverse of the matrix created previously
## but it first checks to see if the inverse has already been found
## if it has been found, the inverse value will be retrieved from cache
## otherwise, the inverse is calculated and set as the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    print("getting cached data.")
    return(i)
  }
  m <- x$get()
  i <- solve(m)
  x$setInverse(i)
  i
}
