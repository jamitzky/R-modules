# module omplet.R        
#                        
library(gsubfn)          

omplets=list()


func.mp=function(name,code,type,...){
        list(...) -> arg             
        names(arg) -> nn             
        arg.str=paste(nn,collapse=",")
        def.str=paste(sapply(1:length(arg), function(i)paste(arg[i],", intent(in) ::",nn[i])),collapse="\n\t")                                              
        code.str=gsub("\\^","**",chartr("[]","()",deparse(substitute(code)))) 
        name=deparse(substitute(name))                                        
        code.f=gsubfn(,,'                                                     
        `type` pure function `name`(`arg.str`)                                
        `def.str`                                                             
        `name` = `code.str`                                                   
        end function                                                          
        ')                                                                    
        om1=list()                                                            
        class(om1)="omplet"                                                   
        om1$name=name                                                         
        om1$type=type                                                         
        om1$code=code.str                                                     
        om1$f=code.f                                                          
        om1$def=def.str                                                       
        om1$arg=arg.str                                                       
        assign(name,om1,envir=parent.frame())                                 
        assign("omplets",c(omplets,list(name=get(name))),envir=.GlobalEnv)    
        }                                                                     


sum.mp=function(name,code,type,...){
        list(...) -> arg                                                      
        names(arg) -> nn                                                      
        nn[[1]] -> ii                                                         
        nn <- nn[-1]                                                          
        arg <- arg[-1]                                                        
        arg.str=paste(nn,collapse=",")                                        
        def.str=paste(sapply(1:length(arg), function(i)paste(arg[i],", intent(in) ::",nn[i])),collapse="\n\t")                                              
        def.str=ifelse( length(nn)==0, "", def.str)                           
        strsplit(deparse(substitute(...)),":")[[1]][2] -> mm                  
        code.str=gsub("\\^","**",chartr("[]","()",deparse(substitute(code)))) 
        name=deparse(substitute(name))                                        
        omp="!$OMP"                                                           
        code.f=gsubfn(,,'                                                     
        `type` pure function `name`(`arg.str`)                                
        `def.str`                                                             
        integer :: `ii`                                                       
        `name`=0                                                              
        `omp` PARALLEL DO REDUCTION(+: `name`)                                
        do `ii`=1,`mm`                                                        
        `name` = `name` + `code.str`                                          
        enddo                                                                 
        `omp` END PARALLEL DO                                                 
        end function                                                          
        ')                                                                    
        om1=list()                                                            
        class(om1)="omplet"                                                   
        om1$name=name                                                         
        om1$type=type                                                         
        om1$code=code.str                                                     
        om1$f=code.f                                                          
        om1$nn=nn                                                             
        om1$arg=arg                                                           
        assign(name,om1,envir=parent.frame())                                 
        assign("omplets",c(omplets,list(name=get(name))),envir=.GlobalEnv)    
        }                                                                     

apply.mp=function(name,code,type,...){
        strsplit(deparse(substitute(...)),":")[[1]][2] -> arg
        names(list(...)) -> nn                               
        code.str=gsub("\\^","**",chartr("[]","()",deparse(substitute(code))))
        name=deparse(substitute(name))                                       
        omp="!$OMP"                                                          
        code.f=gsubfn(,,'                                                    
        pure function `name`()                                               
        `type` :: `name`                                                     
        integer :: `nn`                                                      
        do `nn`=1,`arg`                                                      
        `name`(`nn`) = `code.str`                                            
        enddo                                                                
        end function                                                         
        ')                                                                   
        om1=list()                                                           
        class(om1)="omplet"                                                  
        om1$name=name                                                        
        om1$type=type                                                        
        om1$code=code.str                                                    
        om1$f=code.f                                                         
        om1$nn=nn                                                            
        om1$arg=arg                                                          
        assign(name,om1,envir=parent.frame())                                
        assign("omplets",c(omplets,list(name=get(name))),envir=.GlobalEnv)   
        }                                                                    

ifelse.str="
        integer pure function ifelse(cond,x,y)
                logical, intent(in) :: cond   
                integer, intent(in) :: x,y    
                if (cond) then                
                        ifelse=x              
                else                          
                        ifelse=y              
                endif                         
        end function                          

"

compile.mp=function(code,type,...){
        list(...) -> arg           
        names(arg) -> nn           
        arg.str=paste(nn,collapse=",")
        def.str=paste(sapply(1:length(arg), function(i)paste(arg[i],"::",nn[i])),collapse="\n\t")                                                           
        code.str=sub("\\^","**",chartr("[]","()",deparse(substitute(code))))  
        func.str=paste(sapply(omplets,function(x)x$f),collapse="\n")[[1]]     
        code.f=gsubfn(,,'                                                     
        subroutine main(res,`arg.str`)                                        
        `def.str`                                                             
        `type` :: res                                                         
        integer :: nth=`NTHREADS`                                             
        !call omp_set_num_threads( nth )                                      
        res=`code.str`                                                        
        contains                                                              
        `ifelse.str`                                                          
        `func.str`                                                            
        end
        ')
        cat(code.f,file="main.f90")
# gcc compiler macosx
        system("gfortran -dynamiclib -O3 main.f90 -o main.so")
# gcc compiler linux
#       system("gfortran -shared -O3 main.f90 -o main.so -fPIC")
# intel fortran compiler openmp
#       system("ifort -shared -openmp -O3 main.f90 -o main.so -fPIC")
# intel fortran compiler serial
#       system("ifort -shared -O3 main.f90 -o main.so -fPIC")
        dyn.load("main.so")
        function(...) .Fortran('main', ...)
        }

int=function(...){
        n=deparse(substitute(list(...)))
        n=sub("list","",n)
        ifelse(n=="()","integer",paste("integer, dimension",n,sep=""))
        }
dbl=function(...){
        n=deparse(substitute(list(...)))
        n=sub("list","",n)
        ifelse(n=="()","real*8",paste("real*8, dimension",n,sep=""))
        }

###################################
# example:
# set number of threads
NTHREADS=4

#######################################
# example: distance of 1/x and 1/y
#
# output on a macbook 2.1 GHz:
#
#[1] "times for pureR"
#   user  system elapsed
#  4.715   0.039   4.779
#[1] "times for Romp"
#   user  system elapsed
#  0.114   0.031   0.146


np=as.integer(1000000)
x = as.double(runif(np))
y = as.double(runif(np))
res.pure=as.double(0.)
res.omp =as.double(0.)

# print benchmark time
print("times for pureR")
print(system.time(for(i in 1:np) res.pure=res.pure+(1./x[i]-1./y[i])**2))

# define dosum as distance of 1/x and 1/y
sum.mp(dosum,(1./x[i]-1./y[i])**2, dbl(), i=1:np)

dosum.f = compile.mp(dosum(), dbl(),x=dbl(np),y=dbl(np), np=int())

# print benchmark time
print("times for Romp")
print(system.time(dd<-dosum.f(res=res.omp, x=x, y=y, np=np)))

