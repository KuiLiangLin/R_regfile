abcd <- t(t(read.csv("D:/test_3.txt", sep="\t")))
strsplit(abcd[1], "\\_")[[1]][3]
for(x in c(1:73)){
  cat(abcd[x], " = Reserved_in[",x-1,"];\n", sep ="")
}


a <- t(t(read.csv("D:/test.txt", sep="\t")))
b <- t(t(read.csv("D:/test_2.txt", sep="\t")))
c <- substring(strsplit(a[1,1], "x")[[1]][2], 1, 2)



sink("reg_file.v")

#cat("/*")
cat("//------------------------------------ For top calling---------")
cat("  
  reg_file U_reg_file (
  //---------------ALL were generated with register table by R Language-----------------
     .iTEST_EN  ( DI_APR_TEST_EN    )
    ,.RSTn      ( RSTn              )
    ,.srlat_clk1( srlat_clk1      )
    //i2c
    ,.i2c_SCL   ( DI_APR_TEST_EN    )
    ,.i2c_SDI   ( DI_APR_TEST_MODE  )
    ,.i2c_SDO   ( DO_APR_TESTOUT0   )
    ,.i2c_OEB   ( i2c_OEB )
    //scan
    ,.oREG_ATPG_ENB( oREG_ATPG_ENB )
")

cat("\n")
cat("  //----------------conf in ")
#for(y in c(1,3,5,7,9,11,13,15,17,19,21,23)){
#  for(x in c(3:10)){
#    #    if(a[y,x] == 'Reserved'){next;}
#    aas <- gsub('\\[', replacement = '\\_', a[y,x], perl=TRUE)
#    aas <- gsub('\\]', replacement = '', aas, perl=TRUE)
#    aass<- gsub('\\[', replacement = '\\_int\\[', a[y,x], perl=TRUE)
#    if(grepl("\\[",a[y,x])){
#      aass<- gsub('\\[', replacement = '\\_int\\[', a[y,x], perl=TRUE)
#    } else {
#      aass<- paste0(a[y,x],"_int")
#    }
#    cat("  ,.in_", aas,"(",aass ,")\n" , sep="")
#  }
#}
cccc <-1
sdf <- matrix(1:3500, nrow = 350, ncol = 10)
cat("\n")
for(y in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)){
  for(x in c(3:10)){
    #    if(a[y,x] == 'Reserved'){next;}
    cnt <- 0
    for(ff in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)){
      for(ee in c(3:10)){
        if (strsplit(a[y,x], "\\[")[[1]][1] == strsplit(a[ff,ee], "\\[")[[1]][1] ){
          cnt <- cnt+1
          sdf[ff,ee] <- strsplit(a[y,x], "\\[")[[1]][1]
        }
      }
    }
    sss <-  strsplit(a[y,x], "\\[")[[1]][1]
    if(grepl("Reserved",sss)){
      sdf[cccc,1] <- paste0("  ,.in_",sss,"(",sss,"_in)\n" , sep="")
    }else if(cnt>=2){
      sdf[cccc,1] <- paste0("  ,.in_",sss,"(",sss,"_in[",cnt-1,":0])\n" , sep="")
    }else{
      sdf[cccc,1] <- paste0("  ,.in_",sss,"(",sss,"_in)\n" , sep="")
    }
    cccc <- cccc+1
  }
}
asdf <- sdf[!duplicated(sdf[,1]),]
cat(asdf[grepl(",.in",asdf[,1]),1])


 cat("\n")
 cat("  //----------------conf out")
# for(y in c(1,3,5,7,9,11,13,15,17,19,21,23)){
#   for(x in c(3:10)){
#     #    if(a[y,x] == 'Reserved'){next;}
#     aas <- gsub('\\[', replacement = '\\_', a[y,x], perl=TRUE)
#     aas <- gsub('\\]', replacement = '', aas, perl=TRUE)
#     cat("  ,.", aas,"(/*",a[y,x] ,"*/)\n" , sep="")
#   }
# }

cccc <-1
sdf <- matrix(1:3500, nrow = 350, ncol = 10)
cat("\n")
for(y in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)){
  for(x in c(3:10)){
    #    if(a[y,x] == 'Reserved'){next;}
    cnt <- 0
    for(ff in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)){
      for(ee in c(3:10)){
        if (strsplit(a[y,x], "\\[")[[1]][1] == strsplit(a[ff,ee], "\\[")[[1]][1] ){
          cnt <- cnt+1
          sdf[ff,ee] <- strsplit(a[y,x], "\\[")[[1]][1]
        }
      }
    }
    sss <-  strsplit(a[y,x], "\\[")[[1]][1]
    if(grepl("Reserved",sss)){
      sdf[cccc,1] <- paste0("  ,.",sss,"(",sss,")\n" , sep="")
    }else if(cnt>=2){
      sdf[cccc,1] <- paste0("  ,.",sss,"(",sss,"[",cnt-1,":0])\n" , sep="")
    }else{
      sdf[cccc,1] <- paste0("  ,.",sss,"(",sss,")\n" , sep="")
    }
    cccc <- cccc+1
  }
}
asdf <- sdf[!duplicated(sdf[,1]),]
cat(asdf[grepl(",.",asdf[,1]),1])


 cat("\n")
 cat("//--------D2A / APR ----------------\n")
# #(y in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85)){
# for(y in c(1:dim(b)[1])){
#   if(y%%2 == 0){next}
#   for(x in c(3:10)){
#     if(b[y,x] == ""){next;}
#     bbs <- gsub('\\[', replacement = '\\_', b[y,x], perl=TRUE)
#     bbs <- gsub('\\]', replacement = '', bbs, perl=TRUE)
#     cat("  ,.", bbs,"(/*", b[y,x] ,"*/)\n" , sep="")
#   }
# }
# cat(");") 
# 

cccc <-1
sdf <- matrix(1:3500, nrow = 350, ncol = 10)
#(y in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85)){
for(y in c(1:dim(b)[1])){
  if(y%%2 == 0){next}
  for(x in c(3:10)){
    if(b[y,x] == ""){next;}
    
    cnt <- 0
    for(ff in c(1:dim(b)[1])){
      if(ff%%2 == 0){next}
      for(ee in c(3:10)){
        if(b[ff,ee] == ""){next;}
        if (strsplit(b[y,x], "\\[")[[1]][1] == strsplit(b[ff,ee], "\\[")[[1]][1] ){
          cnt <- cnt+1
          sdf[ff,ee] <- strsplit(b[y,x], "\\[")[[1]][1]
        }
      }
    }
    sss <- strsplit(b[y,x], "\\[")[[1]][1]
    if(cnt>=2){
      sdf[cccc,1] <- paste0("  ,.",sss,"(",sss,"[",cnt-1,":0])\n" , sep="")
    }else{
      sdf[cccc,1] <- paste0("  ,.",sss,"(",sss,")\n" , sep="")
    }    
    cccc <- cccc+1
  }
}
asdf <- sdf[!duplicated(sdf[,1]),]
cat(asdf[grepl(",.",asdf[,1]),1])
cat(");")    

#cat("*/")


cat("

`timescale 1ns/1ps
module reg_file #(
     parameter pTH   = 1.00
)
(    
//---------------ALL were generated with register table by R Language-----------------
     input  wire            iTEST_EN
    ,input  wire            RSTn
    ,input  wire            srlat_clk1
//i2c
    ,input  wire            i2c_SCL
    ,input  wire            i2c_SDI
    ,output wire            i2c_SDO
    ,output wire            i2c_OEB
//scan
    ,output wire            oREG_ATPG_ENB
")

cat("\n")
cat("  //----------------conf in ")
#---------------------------------------------------------------
cccc <-1
sdf <- matrix(1:3500, nrow = 350, ncol = 10)
cat("\n")
for(y in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)){
  for(x in c(3:10)){
#    if(a[y,x] == 'Reserved'){next;}
    cnt <- 0
    for(ff in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)){
      for(ee in c(3:10)){
        if (strsplit(a[y,x], "\\[")[[1]][1] == strsplit(a[ff,ee], "\\[")[[1]][1] ){
          cnt <- cnt+1
          sdf[ff,ee] <- strsplit(a[y,x], "\\[")[[1]][1]
        }
      }
    }
    sss <-  strsplit(a[y,x], "\\[")[[1]][1]
    if(cnt>=2){
      sdf[cccc,1] <- paste0("  ,input  wire [",cnt-1,":0]  in_",sss,"\n" , sep="")
    }else{
      sdf[cccc,1] <- paste0("  ,input  wire        in_",sss,"\n" , sep="")
    }
#    a[y,x] <- gsub('\\[', replacement = '\\_', a[y,x], perl=TRUE)
#    a[y,x] <- gsub('\\]', replacement = '', a[y,x], perl=TRUE)
#    cat("  ,input  wire   in_", a[y,x],"\n" , sep="")
    cccc <- cccc+1
  }
}
asdf <- sdf[!duplicated(sdf[,1]),]
cat(asdf[grepl("input",asdf[,1]),1])


cat("\n")
cat("  //----------------conf out")
#------------------------------------------------
cccc <-1
sdf <- matrix(1:3500, nrow = 350, ncol = 10)
cat("\n")
for(y in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)){
  for(x in c(3:10)){
#    if(a[y,x] == 'Reserved'){next;}
    cnt <- 0
    for(ff in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)){
      for(ee in c(3:10)){
        if (strsplit(a[y,x], "\\[")[[1]][1] == strsplit(a[ff,ee], "\\[")[[1]][1] ){
          cnt <- cnt+1
          sdf[ff,ee] <- strsplit(a[y,x], "\\[")[[1]][1]
        }
      }
    }
    sss <-  strsplit(a[y,x], "\\[")[[1]][1]
    if(cnt>=2){
      sdf[cccc,1] <- paste0("  ,output  wire [",cnt-1,":0]  ",sss,"\n" , sep="")
    }else{
      sdf[cccc,1] <- paste0("  ,output  wire        ",sss,"\n" , sep="")
    }    
#    a[y,x] <- gsub('\\[', replacement = '\\_', a[y,x], perl=TRUE)
#    a[y,x] <- gsub('\\]', replacement = '', a[y,x], perl=TRUE)
#    cat("  ,output  wire   ", a[y,x],"\n" , sep="")
    cccc <- cccc+1
  }
}
asdf <- sdf[!duplicated(sdf[,1]),]
cat(asdf[grepl("output",asdf[,1]),1])

#----------------------------------------------------------------------
cccc <-1
sdf <- matrix(1:3500, nrow = 350, ncol = 10)
cat("\n")
cat("//--------D2A / APR ----------------\n")
#(y in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85)){
for(y in c(1:dim(b)[1])){
  if(y%%2 == 0){next}
  for(x in c(3:10)){
    if(b[y,x] == ""){next;}
    
    cnt <- 0
    for(ff in c(1:dim(b)[1])){
      if(ff%%2 == 0){next}
      for(ee in c(3:10)){
        if(b[ff,ee] == ""){next;}
        if (strsplit(b[y,x], "\\[")[[1]][1] == strsplit(b[ff,ee], "\\[")[[1]][1] ){
          cnt <- cnt+1
          sdf[ff,ee] <- strsplit(b[y,x], "\\[")[[1]][1]
        }
      }
    }
    #b[y,x] <- gsub('\\[', replacement = '\\_', b[y,x], perl=TRUE)
    #b[y,x] <- gsub('\\]', replacement = '', b[y,x], perl=TRUE)
    #cat("  ,output  reg   ", b[y,x],"\n" , sep="")
    sss <-  strsplit(b[y,x], "\\[")[[1]][1]
    if(cnt>=2){
      if(grepl("reg_ts_data",sss) || grepl("reg_ts_out",sss) || grepl("OFFSETCAL_L",sss)){
        sdf[cccc,1] <- paste0("  ,input  wire [",cnt-1,":0]  ",sss,"\n" , sep="")
      }else{
        sdf[cccc,1] <- paste0("  ,output  reg [",cnt-1,":0]  ",sss,"\n" , sep="")
      }
    }else{
      sdf[cccc,1] <- paste0("  ,output  reg        ",sss,"\n" , sep="")
    }    
    cccc <- cccc+1
  }
}
asdf <- sdf[!duplicated(sdf[,1]),]
cat(asdf[( grepl(",",asdf[,1])),1])
cat(");")    
#--------------------------------------------------------
cat("    
    wire [7:0] reg_id;
    wire [7:0] reg_data_in;
    wire [7:0] reg_data_out;
    i2c U_i2c(
     .rstb          (RSTn)
    ,.scl           (i2c_SCL)
    ,.sdi           (i2c_SDI)
    ,.reg_data_out  (reg_data_out) // out to sdo
        
    ,.sdo           (i2c_SDO)
    ,.reg_rd_wrb    (reg_rd_wrb)
    ,.sdo_enb       (i2c_OEB)
    ,.reg_id        (reg_id)
    ,.reg_data_in   (reg_data_in)  //in to test mode  
);\n")
cat("\n")
for(y in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)){
  for(x in c(3:10)){
#    if(a[y,x] == 'Reserved'){next;}
    aas <- gsub('\\[', replacement = '\\_', a[y,x], perl=TRUE)
    aas <- gsub('\\]', replacement = '', aas, perl=TRUE)
    cat("  reg   reg_", aas,";\n" , sep="")
  }
}
cat("\n")
for(y in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)){
  for(x in c(3:10)){
#    if(a[y,x] == 'Reserved'){next;}
    aas <- gsub('\\[', replacement = '\\_', a[y,x], perl=TRUE)
    aas <- gsub('\\]', replacement = '', aas, perl=TRUE)
    cat("  reg   i_", aas,";\n" , sep="")
  }
}
# for(y in c(1:dim(b)[1])){
#   if(y%%2 == 0){next}
#   for(x in c(3:10)){
#     if(b[y,x] == ""){next;}
#     b[y,x] <- gsub('\\[', replacement = '\\_', b[y,x], perl=TRUE)
#     b[y,x] <- gsub('\\]', replacement = '', b[y,x], perl=TRUE)
#     cat("  reg   ", b[y,x],";\n" , sep="")
#   }
# }
cat("// WRITE ID SETTING
    // -----------------------------------------------------------------------------------------------------------
    assign srlat_clk1_inv = ~srlat_clk1;
//    reg[3:0] conf_pass;
//    reg[7:0] Scan_mode;
    wire conf_pass_en = (conf_pass == 4'hA); 
    
    assign oREG_ATPG_ENB = iTEST_EN & Scan_mode == 8'H5A ;\n
	\n")



for(y in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)){
  c <- substring(strsplit(a[y,1], "x")[[1]][2], 1, 2)
    cat("  wire R",c,"H_wr_en = (reg_id == 8'h",c,") & conf_pass_en;\n" , sep="")
}
cat("\n")
#for(y in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85)){
for(y in c(1:dim(b)[1])){
  if(y%%2 == 0){next}
  c <- substring(strsplit(b[y,1], "x")[[1]][2], 1, 2)
  cat("  wire R",c,"H_wr_en = (reg_id == 8'h",c,");\n" , sep="")
}

# cat("
#     assign oREG_ATPG_ENB = iTEST_EN & (Scan_mode_0 == 1'd1)
#                                     & (Scan_mode_1 == 1'd0)
#                                     & (Scan_mode_2 == 1'd1)
#                                     & (Scan_mode_3 == 1'd0)
#                                     & (Scan_mode_4 == 1'd0)
#                                     & (Scan_mode_5 == 1'd1)
#                                     & (Scan_mode_6 == 1'd0)
#                                     & (Scan_mode_7 == 1'd1);\n")

#cat("   
#    wire RDDH_wr_en = (reg_id == 8'hDD) ;
#    wire REEH_wr_en = (reg_id == 8'hEE) ;

#    always @(posedge reg_rd_wrb or negedge RSTn) begin
#        if (!RSTn)              conf_pass <= #(pTH) 4'b0;
#        else if (RDDH_wr_en)    conf_pass <= #(pTH) reg_data_in[3:0];
#        else                    conf_pass <= #(pTH) conf_pass;
#    end 
    
#    // SCAN REGISTER ENABLE
#    // -----------------------------------------------------------------------------------------------------------
#    reg[8:0]  scan_mode;
#    always @(posedge reg_rd_wrb or negedge RSTn) 
#    begin
#        if (!RSTn) scan_mode <= #(pTH) 8'b0;
#        else       scan_mode <= #(pTH) (REEH_wr_en) ? reg_data_in : scan_mode;
#    end     
#    assign oREG_ATPG_ENB = iTEST_EN & (scan_mode == 8'h5A);
    
#")

cat("//--------------------------conf reg_rd_wrb ------------------\n")
for(y in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)){
  c <- substring(strsplit(a[y,1], "x")[[1]][2], 1, 2)
  cat("\n    //----R",c,"H_wr_en----", sep="")
  for(x in c(3:10)){
#    if(a[y,x] == 'Reserved'){next;}
    aas <- gsub('\\[', replacement = '\\_', a[y,x], perl=TRUE)
    aas <- gsub('\\]', replacement = '', aas, perl=TRUE)

    cat("
    always @(posedge reg_rd_wrb or negedge RSTn) begin
    if (!RSTn)                reg_",aas," <= #(pTH) 1'b0;
      else if (R",c,"H_wr_en)    reg_",aas," <= #(pTH) reg_data_in[",10-x,"];
      else                    reg_",aas," <= #(pTH) reg_",aas,";
    end", sep="")

  }
  cat("\n")
}

cat("\n//--------------------------conf 2 srlat_clk_inv ------------------ \n")
for(y in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)){
  c <- substring(strsplit(a[y,1], "x")[[1]][2], 1, 2)
  cat("\n    //----R",c,"H_wr_en----", sep="")
  for(x in c(3:10)){
#    if(a[y,x] == 'Reserved'){next;}
    aas <- gsub('\\[', replacement = '\\_', a[y,x], perl=TRUE)
    aas <- gsub('\\]', replacement = '', aas, perl=TRUE)
    if(grepl('CSEL',a[y,x]) || grepl('DSCRST',a[y,x]) || grepl('DSCEN',a[y,x]) || grepl("PACMOD",a[y,x]) ){
      cat("    
      //always @(posedge srlat_clk1_inv or negedge RSTn) begin
      //    if (!RSTn)              i_",aas," <= #(pTH) 1'b0;
      //    else                    i_",aas," <= #(pTH) in_",a[y,x],";
      //end", sep="")
    }else{
      cat("    
      always @(posedge srlat_clk1_inv or negedge RSTn) begin
          if (!RSTn)              i_",aas," <= #(pTH) 1'b0;
          else                    i_",aas," <= #(pTH) in_",a[y,x],";
      end", sep="")
    }
  }
}

for(y in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)){
  c <- substring(strsplit(a[y,1], "x")[[1]][2], 1, 2)
  cat("\n    //----R",c,"H_wr_en----", sep="")
  for(x in c(3:10)){
#    if(a[y,x] == 'Reserved'){next;}
    aas <- gsub('\\[', replacement = '\\_', a[y,x], perl=TRUE)
    aas <- gsub('\\]', replacement = '', aas, perl=TRUE)
    if(grepl('CSEL',a[y,x]) || grepl('DSCRST',a[y,x]) || grepl('DSCEN',a[y,x]) || grepl("PACMOD",a[y,x]) ){
      cat("
      assign ",a[y,x]," = conf_pass_en ? reg_",aas," : in_",a[y,x],";", sep="")
    }else{
      cat("
      assign ",a[y,x]," = conf_pass_en ? reg_",aas," : i_",aas,";", sep="")
    }
  }
}



cat("\n\n\n//--------------------------register A2D APR reg_rd_wrb ------------------")
for(y in c(1:dim(b)[1])){
  if(y%%2 == 0){next}
  c <- substring(strsplit(b[y,1], "x")[[1]][2], 1, 2)
  cat("\n    //----R",c,"H_wr_en----", sep="")
  for(x in c(3:10)){
    if(b[y,x] == ""){next;}
    if(b[y+1,x] == "READ"){cat("read only ");next;}
    # b[y,x] <- gsub('\\[', replacement = '\\_', b[y,x], perl=TRUE)
    # b[y,x] <- gsub('\\]', replacement = '', b[y,x], perl=TRUE)
    if(b[y+1,x] == ""){b[y+1,x] <- 0}
    cat("
    always @(posedge reg_rd_wrb or negedge RSTn) begin
    if (!RSTn)                ",b[y,x]," <= #(pTH) 1'b",b[y+1,x],";
      else if (R",c,"H_wr_en)    ",b[y,x]," <= #(pTH) reg_data_in[",10-x,"];
      else                    ",b[y,x]," <= #(pTH) ",b[y,x],";
    end", sep="")
    
  }
  cat("\n")
}


cat("    
//-------------------------------read----------------------------------------
    assign reg_data_out =")
cat("
                          //--------------------------conf reg_rd_wrb ------------------")
aasmtrx <- matrix(1:22, nrow = 11, ncol = 2)
for(y in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)){
  c <- substring(strsplit(a[y,1], "x")[[1]][2], 1, 2)
  for(x in c(3:10)){
    #    if(a[y,x] == 'Reserved'){next;}
    aas <- gsub('\\[', replacement = '\\_', a[y,x], perl=TRUE)
    aasmtrx[x,1] <- paste0("reg_",gsub('\\]', replacement = '', aas, perl=TRUE))
  }
  cat("
                          {8{reg_id == 8'h",c,"}} & {",aasmtrx[3,1],", ",aasmtrx[4,1],", ",aasmtrx[5,1],", ",aasmtrx[6,1],", ",aasmtrx[7,1],", ",aasmtrx[8,1],", ",aasmtrx[9,1],", ",aasmtrx[10,1],"} |", sep="")
}
cat("
                          //--------------------------register A2D APR reg_rd_wrb ------------------")
for(y in c(1:dim(b)[1])){
  if(y%%2 == 0){next}
  c <- substring(strsplit(b[y,1], "x")[[1]][2], 1, 2)
  for(x in c(3:10)){    if(b[y,x] == ""){b[y,x] <- "1'd0"} }

  cat("
                          {8{reg_id == 8'h",c,"}} & {",b[y,3],", ",b[y,4],", ",b[y,5],", ",b[y,6],", ",b[y,7],", ",b[y,8],", ",b[y,9],", ",b[y,10],"} ", sep="")
  if(y != dim(b)[1]-1){cat("    |")}
}
cat("
                          ;")

cat("\n\nendmodule")
sink()