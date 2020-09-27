#1) read all files
    # library(openxlsx)
    library(xlsx)
    library(XML)
    #get current work directory
        wd <- getwd()
    #set directory of html files and excel workbook
        setwd("..")
        htmldir <- paste(getwd(),"/Results/Results html/",sep = "")
        excdir <- paste(getwd(),"/Results/",sep = "")
    #load most recent html file
        setwd(htmldir)
        a<-list.files()
        d<-sapply(a,file.mtime)
        b<-readHTMLTable(a[which(d==max(d))])
    #reset directory
        setwd(wd)

#4) remove uninteresting data from files and add interesting info
    b<-lapply(b,function(x) x[,2:ncol(x)]) #first column
    b<-lapply(b,function(x) sapply(x,as.character)) #factors
    b<-lapply(b, function(x){
      #converged well (calculation)
          est<-as.numeric(x[,2])
          conv<-est>25
          probs<-which(conv)
          worst<-est[which(est==max(est))]
      #number camps
          x[1,3]<-sum(grepl("constant friendship",x[,1]))
          x[1,2]<-"n networks = "
          x[1,c(1,4)]<-""
      #add rows if necessary
          nrows<-sum(grepl("constant friendship",x[,1]))
          if(nrows<6){
            rbind(x[rep(1,(6-nrows)),],x)
          }
      #number camps
          x[2,3]<-nrow(sumstats3)
          x[2,2]<-"n camps = "
          x[2,c(1,4)]<-""
      #Full friendship
          x[3,]<-""
          x[3,2]<-"Friendship = "
          x[3,3]<-ifelse(sum(grepl("ffriendship",x[,1]))>0,"Full","Close")
      #DV
          x[4,]<-""
          x[4,2]<-"DV = "
          if(class(options)!="function"){
            x[4,3]<-options$DVname
          }else{
            x[4,3]<-NA
          }
      #ward or camp
          x[5,]<-""
          x[5,2]<-"Network = "
          x[5,3]<-ifelse(nrow(sumstats3)==length(dataset),"Camp","Ward")
          x[5,4]<-paste("Worst Conv =", worst)
      #converged well (annotation)
          x[6,]<-""
          x[6,2]<-"Converged = "
          x[6,3]<-ifelse(any(conv),"No","Yes")
          x[6,4]<-paste("Problems =", ifelse(length(probs)==0,"None",probs))
          #remove rate effects
          x[7,]<-""
          x<-x[!(grepl("constant friendship",x[,1]) | grepl("rate DV",x[,1])),]
    })
        
#5) export as single excel document
    #if second time
        filename<-paste(excdir,"results.xlsx",sep="")
        b1<-as.data.frame(b[[1]])
        b1<-sapply(b1,as.character)
        b1[b1==""]<-" "
        b1[6:nrow(b1),3]<-paste("(",b1[6:nrow(b1),3],")",sep="")
        sheetname<-as.character(Sys.Date())
        wb <- loadWorkbook(filename)
        a<-getSheets(wb)
        if(sheetname %in% names(a)){
          a1<-Sys.time()
          a2<-substr(a1,nchar(sheetname)+2,nchar(a1))
          a2<-gsub(":","-",a2)
          sheetname<-paste(sheetname,a2)
        }
        write.xlsx(b1, file=filename, sheetName=sheetname, row.names=F,col.names = F,append = T)
    #format
        wb <- loadWorkbook(filename)
        sheets <- getSheets(wb)
        setColumnWidth(sheets[[length(sheets)]], colIndex=2, colWidth=11)
        setColumnWidth(sheets[[length(sheets)]], colIndex=3, colWidth=7)
        setColumnWidth(sheets[[length(sheets)]], colIndex=1, colWidth=30)
        # autoSizeColumn(sheets[[length(sheets)]],colIndex=1)
    #right align
        cs <- CellStyle(wb) + Alignment(horizontal = "ALIGN_RIGHT")
        all.rows <- getRows(sheets[[length(sheets)]], rowIndex = 1:nrow(b1))
        all.cells<-getCells(row=all.rows, colIndex = 2)
        invisible(lapply(all.cells, setCellStyle, cs))
        all.rows <- getRows(sheets[[length(sheets)]], rowIndex = 6:nrow(b1))
        all.cells<-getCells(row=all.rows, colIndex = 3)
        invisible(lapply(all.cells, setCellStyle, cs))
    #save
        saveWorkbook(wb, file = filename)
    #grid lines
        wb <- openxlsx::loadWorkbook(filename)
        openxlsx::showGridLines(wb, length(sheets), showGridLines = FALSE)
        openxlsx::saveWorkbook(wb, file = filename,overwrite = T)

    # 
    #         #determine sheet name
    #           tsheet<-paste(i,"-",vars[i],sep="")
    #         if(i==1){
    #           write.xlsx(tresult, file=fname, sheetName=tsheet, row.names=T)
    #         }else{
    #           write.xlsx(tresult, file=fname, sheetName=tsheet, append=TRUE, row.names=T)
    #         }
    
    # #convert to dataframe
    #     #prep data frame
    #         df<-data.frame(var=d)
    #         df$var<-as.character(df$var)
    #     #prep models
    #         a<-b[[1]]
    #         sapply(a,as.character)
    #         