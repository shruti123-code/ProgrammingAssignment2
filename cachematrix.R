makeCacheMatrix <- function(x = matrix()) {
  j <- NULL                            #initializing inverse as null
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function(){x}   #function to get matrix
  setInverse <- function(inverse) {j <<- inverse}
  getInverse <- function() {
    inver<-ginv(x)              #function to obtain inverse
    inver%*%x
  } 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){               #checking wheather inverse is null
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)    #calculating inverse value
  x$setInverse(j)
  j
}

