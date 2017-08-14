#' @title Polish Publisher 
#' @description Polish publisher field separating for/by.
#' @param x Publisher vector
#' @return Polished vector
#' @export
#' @details Polish publisher field. 
#' @author Mika Koistinen \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # polish_publisher("printed and sold by R. Marchbank")
#' @keywords utilities
polish_publisher_forby <- function (x) {
  #x=df100$publisher
  x <- as.character(x)
  xorig <- x
  #x1="[s.n.]"
  x1 <- tolower(x)
  
  
  #reading data to gsub
  con <- file("data/replaces1.txt", open = "r")
  xreplace1lines <- readLines(con)
  xreplace1 <- strsplit(xreplace1lines, split="#")

  for(i in 1:length(xreplace1)){
    #print(i)
    #print(xreplace1[[i]][1])
    #print(xreplace1[[i]][2])
    x1 = gsub(xreplace1[[i]][1],xreplace1[[i]][2],x1)
    #x1 = gsub("\\[","",x1)
    #x1 = gsub("\\[sic\\]","",x1)
    #x1 = gsub("[sic]","",x1)
    
  }
  #xreplace1[[i]][1]=="\\[sic\\] "
  close(con)
  #if first words are for or by -> printed for or printed by  
  for(ind2 in 1:length(x1)){
      s1=strsplit(x1[ind2],' ')
      first_l=s1[[1]][1]
      if (grepl("by",first_l)==TRUE) {
          x1[ind2]=gsub("by ","printed by ",x1[ind2]) 
      }
  }
  for(ind2 in 1:length(x1)){
    s1=strsplit(x1[ind2],' ')
    first_l=s1[[1]][1]
    if (grepl("for",first_l)==TRUE) {
      x1[ind2]=gsub("for ","printed for ",x1[ind2]) 
    }
  }
  

  # Search for special fields: 
  #Splits every string in vector x1 by last column of c if found else second last then third last until found.
  #for example:
  #str="printed by wilson, spence, and mawman; sold by g. g. j. and j. robinson, t. cadell, b. white, j. robson, and j. murray, london; and by all the booksellers in york"   
  #pick_print_fields(str,c(" by ",", and ")) 
  #"mawman; sold by g. g. j. and j. robinson, t. cadell, b. white, j. robson"
  #pick_print_fields(str,c(" by ","not exist")) 
  #"wilson, spence, and mawman; sold ..."

  #x1 = gsub("printer,","printer ",x1) #argraphwyd
  
  for(ind22 in 1:length(x1)){
    x1[ind22] = gsub("printer,","printer ",x1[ind22]) #argraphwyd
    
    if(grepl('printer',x1[ind22],)==TRUE) {
      x1[ind22]=paste("printed by ",x1[ind22])     
    } 
  }
  
  
  x1 = gsub("sold at ","sold by at ",x1)
  x1 = gsub("sold in ","sold by at ",x1)
  x1 = gsub("sold only in ","sold by at ",x1)
  x1 = gsub("sold together with ","sold by at ",x1)
  
  x1 = gsub("printed ","printed by ",x1) #?
  
    
  xsoldby <- sapply(pick_print_fields(x1, c("sold by ","printed and sold by ","printed for and sold by ","printed for sold by ")), function (x) {x}) 
                    #"sold at the "
  xby <- sapply(pick_print_fields(x1, c("printed by ","printed and sold by ","printed by and for ","printed for and by")), function (x) {x})
  xfor <- sapply(pick_print_fields(x1, c("printed for ","printed for and sold by ","printed by and for ","printed for sold by ","printed for and by")), function (x) {x})
  xreprint <- sapply(pick_print_fields(x1, c("reprinted ")), function (x) {x})
  
  
  #Check for more specific rules:
  for(ind2 in 1:length(x1)){
    # Check if there is no sold by and printed by then by and for is accepted as [printed by]
    if(is.na(xsoldby[ind2]) & is.na(xby[ind2])){
      xby[ind2]=pick_print_fields(x1[ind2],' by ') 
      xfor[ind2]=pick_print_fields(x1[ind2],' for ')
      
      #if [printed by] is not empty and contains also "for"
    } 
  }
  
  for(ind2 in 1:length(x1)){
    if(!is.na(xby[ind2]) & is.na(xfor[ind2])) {
      #if next is number not accept this rule
      word1=strsplit(xfor[ind2],' ')[[1]][1]
      word1=gsub(",","",word1) 
      if(!is.na(suppressWarnings(as.numeric(word1)))==TRUE) {
          xfor[ind2]=NA
          next
      }
      text=xby[ind2]
      
      #the part before is only printed for; so the end part is removed
      xby[ind2]=strsplit(text,'for ')[[1]][1] #!grepl("printed for and sold by ",x1[ind2]) & 
    } 
  }
  
  for(ind2 in 1:length(x1)){
    if(!is.na(xfor[ind2]) & is.na(xsoldby[ind2])) {
      #if [for] is not empty and contains also " sold by"
      xfor[ind2]=pick_print_fields(x1[ind2],' for ')
      text=xfor[ind2]
      #the part before is only printed for; so the end part is removed
      xfor[ind2]=strsplit(text,' sold by ')[[1]][1]
    } 
  }
  
  
  #removes if last character is ","
  xfor=remove_ending_chars(xfor)
  xby=remove_ending_chars(xby)
  xsoldby=remove_ending_chars(xsoldby)
  
  #splitting xfor into xfor_1st and xfor_at
  output=split_doer_and_place(xfor)
  xfor_at=output$x_place
  xfor_1st=output$x_doer
  
  #splitting xby into xby_1st and xby_at
  output2=split_doer_and_place(xby)
  xby_at=output2$x_place
  xby_1st=output2$x_doer
  
  #splitting xfor into xby_1st and xby_at
  output3=split_doer_and_place(xsoldby)
  xsoldby_at=output3$x_place
  xsoldby_1st=output3$x_doer
  

  #remove ending "and" "," or ";"
  for(ind2 in 1:length(xfor)){
    xfor_1st[ind2]=remove_ending_chars(xfor_1st[ind2])  
  }
  
  for(ind2 in 1:length(xfor_1st)){
    xfor_1st[ind2]=remove_ending_chars(xfor_1st[ind2])  
  }
  
  for(ind2 in 1:length(xby)){
    xfor_1st[ind2]=remove_ending_chars(xfor_1st[ind2])  
  }
  for(ind2 in 1:length(xby_1st)){
    xfor_1st[ind2]=remove_ending_chars(xfor_1st[ind2])  
  }
  
  #create final dataframe
  xorig[xorig == ""] <- NA
  x1[x1 == ""] <- NA
  
  xrest <- xorig
  xrest2 <- x1
  
  #xrest2 <- tolower(xorig)
  
  xrest[which(!is.na(xfor) | !is.na(xby) | !is.na(xsoldby)| !is.na(xreprint))] <- NA
  xrest2[which(!is.na(xfor) | !is.na(xby) | !is.na(xsoldby)| !is.na(xreprint))] <- NA
  
  
  #for(ind2 in 1:length(rest2)){
   # if(!is.na(xrest2[ind2])) {
      #if [for] is not empty and contains also " sold by"
      
      #  xfor[ind2]=pick_print_fields(x1[ind2],' for ')
      #text=xfor[ind2]
      #the part before is only printed for; so the end part is removed
      #xfor[ind2]=strsplit(text,' sold by ')[[1]][1]
    #} 
  #}
  
  
  
  res <- list(original = xorig, original_mod=x1,printedfor = xfor,
              printedfor_doer = xfor_1st,printedfor_place = xfor_at, 
              printedby = xby, printedby_doer = xby_1st,
              printedby_place = xby_at, 
              soldby=xsoldby, soldby_doer = xsoldby_1st, 
              soldby_place = xsoldby_at, 
              reprinted=xreprint,
              rest = xrest,
              rest2 =xrest2)
  as.data.frame(res)

}





