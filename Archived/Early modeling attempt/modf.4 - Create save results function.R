SaveResults<-function(){
#1 - Determine file name to save by with goal of not replacing anything
    dir<-getwd()
    tryCatch(setwd(paste(dir,"/Dropbox files/",sep="")), error=function(e){})
    tryCatch({
    number<-suppressWarnings(max(as.numeric(gsub("[[:alpha:]]", "", list.files(path="./Results html",pattern='*\\.html', recursive=TRUE))),na.rm=T)+1)
    number<-ifelse(is.infinite(number),1,number)
    htmlfilename<-paste("./Results html/Model ",number,".html",sep="")
    pngfilename<-paste("./Results png/Model ",number,".png",sep="")
#2 - save and view results
    #a - save as html
    siena.table(Model, type="html", sig=TRUE, file=htmlfilename)
    #b - save important numbers as png
    #i - subset to important numbers
            a<-readHTMLTable(htmlfilename)[[1]]
            b<-a[!grepl("^Rate|^rate",a$Effect),]
            b<-b[!is.na(b$`(s.e.)`),]
        #ii - insert a space between behaviors
            ind<-which(b$Effect=="DV linear shape")
            b[,]<-lapply(b[,], as.character)
            b2<-b[1,]
            b2[1,]<-""
            b<-rbind(b[1:(ind-1),],b2,b[ind:nrow(b),])
        #iii - save
            {png(pngfilename,height = 100+32*nrow(b), width = 1000)
                g<-tableGrob(b,theme = ttheme_minimal(base_size = 28,core = list(fg_params = list(hjust=0, x=0.1,fontsize=28))),rows=NULL)
                grid.draw(g)
            dev.off()}
        #iv - View
            browseURL(htmlfilename)
    setwd(dir)
    }, error=function(e){})
}