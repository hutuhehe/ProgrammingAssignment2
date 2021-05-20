## makeCacheMatrix() creates a list containing a function to
## set the value of the matrix :setmatrix
## get the value of the matrix :getmatrix
## set the value of the inverse of matrix
## get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmatrix <- function(y) {
          x <<- y
          m <<- NULL
  }
  getmatrix <- function()x
  setinv <- function(inv) inv <<- inv
  getinv <- function()inv
  list(setmatrix = setmatrix,getmatrix = getmatrix,
       setinv = setinv,getinv = getinv)

}


## cacheSolve() calculate inverse of the special "vector"
# eg  exampleMatrix = makeCacheMatrix(x = marix(1:4,2,2))
#     cacheSolve(exampleMatrix) first calcuate the inverx
#     cacheSolve(exampleMatrix) second output the cached inverx

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                  message("getting cached data ")
                  return(inv)
        }
        data <- x$getmatrix()
      
        inv <- solve(data)
        x$setinv(inv)
        inv
}
