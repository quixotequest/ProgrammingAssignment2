## Homework assignment for inverting a matrix using caching

makeCacheMatrix <- function(x = matrix()) {
## first, create a value "newmatx" that will store the inverse of the function
  newmatx <- NULL
  setmatx <- function (y) {
      x <<- y
      newmatx <<- NULL}
  
## Then get the matrix
    getmatx <- function() x
## Then set and get the inverse lexicals of the matrix
    setinv <- function(solved) {newmatx <<- solved}
    getinv <- function() newmatx
    list(setmatx=setmatx, getmatx=getmatx, setinv=setinv, getinv=getinv)
}

## Computing the inverse of the square matrix with the "solve" function. 
## If X is a square invertible matrix, then solve(X) returns its inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## First, check for cache status
  newmatx <- x$getinv()
        ## This tests if a value has been stored prev. for use
  if (!is.null(newmatx)) {
        message("retrieved from cache")
        return(newmatx)
        }
## Otherwise solve for the inverse
solution <- x$getmatx()
newmatx <- solve(solution)
## Now put the new inverse calculation in the cache
x$setinv(newmatx)
## show the solution
print(newmatx)
}  

## Now run a test matrix
test <- matrix((sample(1:99,4,replace=F)),2,2)
print(test)
## Generate the inverse with f(makeCacheMatrix) 
inv <- makeCacheMatrix(test)
## Confirm inverse prints and then caches (via the f(cacheSolve)'s message)
testcache <- cacheSolve(inv)
testcache <- cacheSolve(inv)
testcache <- cacheSolve(inv)
