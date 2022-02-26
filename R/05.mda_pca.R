# PCA   -----------------------------------------------------------


#' @title mda_PCA
#' @description Get kegg cid by CTS
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param mat 'Dataframe' or 'matrix'; with Compound_ID as 1st column,.
#' @param type "single','multiple';  For one compound or multiple compounds.
#' @return KEGGID InChikey 2 kegg cid.
#' @importFrom PCAtools pca biplot
#' @importFrom ggplotify as.ggplot
#' @export


mda_pca = function(mat,group,interactive=F,removeVar = 0.1,logTransfer = F,title = "test") {
  check_title = colnames(mat)[1]
  if(check_title != "Compound_ID") {
    message("Please set Compound_ID in 1st column, and set the 1st column name as 'Compoun_ID'. and try again")
    return()
  }
  mat %>% 
    column_to_rownames(Compound_ID) -> mat_x
  if(isTRUE(logTransfer)) {
    mat_x = log10(mat_x)
  } 
  
  pca_mat <- PCAtools::pca(mat = mat_x,metadata = group,scale = T,removeVar = removeVar)
  plt_bi = PCAtools::biplot(
    pcaobj = pca_mat,
    legendPosition = "top",
    colby = "group",
    title = "PCA biplot",
    subtitle = paste0("result of ", title),
    showLoadings = F,
    hline = 0,
    vline = 0
  )
  
  pca_out = list(
    pca_result = pca_mat,
    plt_bi = plt_bi
  )
  return(pca_out)
}


