

This assignment is about solving the inverse of a matrix by caching the 
# result within a lexical scope of a function:  "makeCacheMatrix" and 
# "cacheSolve". Caching is about using memory to avoid excess computation.
makeCacheMatrix <- function(x = matrix()) {
   inv<-NULL
   set<-function(y){
       x<<-y
       inv<<-NULL }
   get<-function() {x}
   setInverse<-function(inverse){inv<<-inverse}
   getInverse<-function(){inv}
   list(set=set,get=get,setInverse=setInverse, getInverse=getInverse)
}

## The function "cacheSolve" returns the inverse of the matrix that is 
# returned by makeCacheMatrix function, e.g. xMat$getmatrix()



cacheSolve <- function(x, ...) {
   inv<-x$getInverse()
   if(!is.null(inver){
        message("getting catched data")
        return(inv)}
   mat<-x$get()
   inv<-solve(mat,...)
   x$setInverse(inv)
   inv
