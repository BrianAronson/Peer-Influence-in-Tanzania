#1) read all files
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
        b<-lapply(a,readHTMLTable)
        names(b)<-a
    #reset directory
        setwd(wd)
      
#2) remove files with different format
    b<-b[sapply(b,length)==1]

#3) cull files    
    #identify all unique variable names
        b1<-names(b)
        b<-lapply(b,function(x) x[[1]])
        d<-unlist(lapply(b,function(x) x[,2]))
        d<-unique(d)
        d<-as.character(d)
        d<-sort(d)
    #remove b where no convergence
        f<-sapply(b, function(x) max(as.numeric(as.character(x[,3]))))
        b<-b[f<50]
        # #find best predictors
        #     g<-sapply(b, function(x) x[,2][x[,5]!=""])
        #     a<-as.data.frame(table(as.character(unlist(g))))
        #     a<-a[order(a$Freq,decreasing=T),]
        #     a<-a[substr(a$Var1,1,2)!="DV",]
        #     a<-a[a$Freq>2,]
        #     View(a)
    #subset data to most recent files
        #shorten names
            names(b)<-gsub("Model ","",names(b)) 
            names(b)<-substr(names(b),1,3)
        #remove those not in 500s
            b<-b[!is.na(as.numeric(substr(names(b),3,3)))]
            
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
    
    
#4.5) more culling
    # #subset to sheets with average similarity
        # tm<-sapply(b,function(x) sum(grepl("DV average similarity",x[,1])))
        # b<-b[tm]
        # 
    #subset to sheets with average alter
        # tm1<-sapply(b,function(x) sum(grepl("DV av",x[,1])))
        # sum(tm)
        # a<-b[!tm & tm1]
        # a
        # b<-b[tm]
      
#5) export as single excel document
    #if first time
        b1<-as.data.frame(b[[length(b)]])
        b1<-sapply(b1,as.character)
        b1[b1==""]<-" "
        names(b1)<-gsub(" ","",names(b1))
        b1[6:nrow(b1),3]<-paste("(",b1[6:nrow(b1),3],")",sep="")
        sheetname<-as.character(Sys.Date())
        filename<-paste(excdir,"/results.xlsx",sep="")
        write.xlsx(b1, file=filename, sheetName=sheetname, row.names=F,col.names = F)
        wb <- loadWorkbook(filename)
        sheets <- getSheets(wb)
        for(i in length(sheets):length(sheets)){
            # autoSizeColumn(sheets[[i]],colIndex=1:3)
        }
        saveWorkbook(wb, file = filename)
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


    # #subset to just last sheet
    #     b<-b[length(b)]
    # file <- paste(excdir,"/results.xlsx", sep="")
    # wb <- createWorkbook()
    # datas <- b
    # sheetnames <- names(b) # or names(datas) if provided
    # sheets <- lapply(sheetnames, createSheet, wb = wb)
    # void <- Map(addDataFrame, x=datas, sheet=sheets,row.names=F)
    # for(i in 1:length(sheets)){
    #     autoSizeColumn(sheets[[i]],colIndex=1:4)
    # }
    # saveWorkbook(wb, file = file)
    # 
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