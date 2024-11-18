#' Format data frame names
#'
#' Remove letter accents; replace spaces with periods; make all names lowercase; remove periods if they separate single characters
#' @param x Data frame
#' @param accents Should accents be removed? Default = TRUE
#' @param spaces Should spaces be removed? Default = TRUE
#' @param lowercase Should all letters all be lowercase? Default = TRUE
#' @param excess.periods Should excess periods be removed? Default = TRUE
#' @importFrom stringi stri_sub stri_trans_general
#' @importFrom stringr str_sub

#' @examples
#' fake<-as.data.frame(matrix(rep(1:4,7), ncol=7))
#' names(fake)<-c('    SOme DATA  here ','Données','n e b','A.S.O','.cfvn','ac.kg.tow','gh..kg.tow')
#' fake
#' format_df_names(fake)
#' format_df_names(fake,accents=FALSE)
#' format_df_names(fake,spaces=FALSE)
#' format_df_names(fake,lowercase=FALSE)
#' format_df_names(fake,excess.periods=FALSE)
#' @export
format_df_names<-function(x,accents=TRUE,spaces=TRUE,lowercase=TRUE,excess.periods=TRUE){

  if(accents==TRUE){names(x)<-stringi::stri_trans_general(str = names(x), id = "Latin-ASCII")}#remove accents

  if(spaces==TRUE){
    names(x)<-trimws(names(x))# remove leading and trailing white space
    names(x)<-gsub("\\s+"," ",names(x))# remove extra white space
    names(x)<-gsub("\\. | ",".",names(x),fixed = F)#replace spaces with "."
    }

  if(lowercase==TRUE){names(x)<-tolower(names(x))}#change to lower case


  if(excess.periods==TRUE){
    #remove repeating periods
    names(x)<-gsub("\\.+",".",names(x))

    #remove periods from strings that begin or end in periods
    index<-which(stringi::stri_sub(names(x),-1,-1)%in%'.'|stringi::stri_sub(names(x),1,1)%in%'.')
    if(length(index>0)){
      names(x)[index]<-gsub("\\.","",names(x)[index],fixed = F)
    }

    # remove periods from strings in which the 2nd and 4th character are periods
      # crude way of identifying strings in which every 2nd character is a period
    index2<-paste0(stringr::str_sub(names(x), 2, 2),stringr::str_sub(names(x), 4, 4))
    index3<-grep('\\..',index2)
    if(length(index3>0)){
      names(x)[index3]<-gsub("\\.","",names(x)[index3],fixed = F)
    }


  }
  return(x)
}
