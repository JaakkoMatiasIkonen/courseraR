## The makeCacheMatrix function creates the matrix that is later cached

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL # first we initialize "inverse" and "x"
  set <- function(y){
    x <- y
    inverse <<- NULL
  }
  get <- function(){x} 
  set_inv <- functon(inv) {inverse <<- inv}
  get_inv <- function(){inverse}
  list(set=set, 
       get=get, 
       set_inv=set_inv, 
       get_inv=get_inv) ### a new object is returned to the parent environemnt 
}

## The cacheSolve function retrives the cached matrix 
## (as calculated by makeCacheMatrix earlier)
## and calculates its matrix

cacheSolve <- function(x, ...) { ##receiving cache data
  inverse <- x$get_inv()
  if(!is.null(inverse)){
    message("receiving cache data") ## checking whether the inverse is null
    return(inverse) 
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$set_inv(inverse)
  inverse ##returns the inverse of the matrix
}

##EXAMPLE##
gmatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2)) ##gen 2x2 matrix
gmatrix$get() 
gmatrix$get_inv()
cacheSolve(gmatrix) # inverse!
cacheSolve(gmatrix) # as the matrix remains the same, cacheSolve retrieves the inverse from the cache!
