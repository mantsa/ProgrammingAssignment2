makeCacheMatrix<-function(a=matrix()){
    
#######################################################
###This function creates a matrix with the special 
###ability to cache their inverse. Following operations 
###are allowed: 
###
### a) m<-makeCacheMatrix() - creates an empty matrix
### b) m$set(y) - set the values of the matrix
### c) setinv() - calculates the inverse matrix, if it 
###     wasn't already calculated
### d) getinv() - returns the cached inverse matrix
#######################################################
    
    a_i<-NULL   #inititialize the inverse matrix as null
    #setter for the source matrix
    set<-function(b){
        #check first if the given matrix already exists
        if(!identical(a,b)){
            a<<-b
            a_i<<-NULL
        }
    }
    
    #getter for the source matrix 
    get <- function() a
    
    #setter for the inverse matrix
    setinv <- function(solve=matrix()) a_i<<-solve
    #getter of the inverse matrix
    getinv <- function() a_i
    #define the setter & getter option to the object
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(a,...){

#######################################################
###This function calculates the inverse matrix of a 
###given object from type 'makeCacheMatrix'. To create 
###such type of matrix please use the constructor
###function makeCacheMatrix().
###
##########################################################    
    
    ##Get the inverse matrix of the given one
    i<-a$getinv()
    #if the inverse matrix is already calculated (not equals null) get it from the cache
    if(!is.null(i)){ 
        message("Getting cached data")
        return(i)        
    }else{
        data<-a$get()
        #make a check if the matrix is 
        if(dim(data)[1]==dim(data)[2]){
            message("Calculating the inverse matrix")
            i<-solve(data,...)
            a$setinv(i)
            
        }else {
            print("For the given matrix it could not be calculated an inverse one.") 
            print("Please try with a squared matrix.")
        }
        return(i)
    }
}