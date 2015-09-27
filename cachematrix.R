## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a square invertible matrix which creates 
##a special matrix, which has followinf functions.

## 1.set the matrix
## 2. get matrix
## 3. set inverse of the matrix
## 4. get inverse of the matrix

## '<<-' is used so as to assign a value to an object in an environment
## other than the current environment

makeCacheMatrix <- function(x = matrix()) {
  mat_inv<-NULL 
  set<-function(y){
    x<<-y
    mat_inv<<-NULL
  }
  get<-function() x
  setmat_inv<-function(solve) mat_inv <<- solve
  getmat_inv<-function() mat_inv
  list(set=set, get=get,
       setmat_inv=setmat_inv,
       getmat_inv=getmat_inv)

}


## Write a short comment describing this function

## this function returns the inverse of the special matrix created using the above function
## 'if' function checks if the inverse of the matrix is already calculated.
## and if already exists it skips the computation and gets the results from the cache.
## Otherwise computes the inverse and set the value of the inverse 
## via setmat_inv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat_inv<-x$getmat_inv()
  if(!is.null(mat_inv)){
    message("getting cached data")
    return(mat_inv)
  }
  matrix<-x$get()
  mat_inv<-solve(matrix, ...)
  x$setmat_inv(mat_inv)
  mat_inv
}
