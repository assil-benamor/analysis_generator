score_mean <- function(x) {
  case_when(x >2/3 ~ 3,
            x > 1/3 ~ 2,
            x <= 1/3 ~ 1)
}

factors_and_character_to_numeric <- function(df){
  
  write.csv(x = df, file = "temp.csv", na = "",row.names = F)
  df <- read.csv("temp.csv",na.strings = c("","NA"," "),stringsAsFactors = F)
  file.remove("temp.csv")
  return(df)
}

export_table <- function(res,assessment_name,path) {
  
  
  res <- factors_and_character_to_numeric(res)
  
  
  hs1 <- openxlsx::createStyle(fgFill = "#FE615D", halign = "CENTER", textDecoration = "Bold",
                               border = "Bottom", fontColour = "white")
  
  posStyle <- openxlsx::createStyle(bgFill = "#FDE3E3")
  
  whitebg <- openxlsx::createStyle(bgFill = "white",wrapText = TRUE)
  
  wb = openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(wb, "results")
  
  openxlsx::writeDataTable(wb, 1, res, startRow = 1, startCol = 1, tableStyle = "TableStyleLight15",headerStyle = hs1, tableName = "results")
  
  openxlsx::conditionalFormatting(wb, "results", cols=1:ncol(res), rows=2:(nrow(res)+1), rule="!=766546", style = whitebg)
  
  openxlsx::conditionalFormatting(wb, "results", cols=1:ncol(res), rows=1:(nrow(res)+1), rule='$C1=""', style = posStyle)
  
  openxlsx::setColWidths(wb, "results", cols = 1:ncol(res), widths = c(70,rep(25,(ncol(res)-1))))
  
  openxlsx::saveWorkbook(wb, paste0(path,assessment_name,"_results_table_",gsub("-","_",gsub("(:| |-)","_",Sys.time())),".xlsx"), overwrite = TRUE)
  
}