# webserver in R (http://localhost:8088/paste(Sys.info(),collapse=''))
library(gsubfn)
http.serve=function(){
        ss=make.socket("localhost",8088,server=T)
        gg=unlist(strapply(read.socket(ss),"GET /(.*?) HTTP",c))[[1]]
        res=eval.parent(parse(text=gg))
        reply=
gsubfn(,,"HTTP/1.1 200 OK
Server: R 2.8.1
Content-Length: `nchar(res)`
Content-Type: text/html
Connection: close

`res`")
        write.socket(ss,reply)
        close.socket(ss)
        }
http.serve()
