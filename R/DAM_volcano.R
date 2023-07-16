# DAM_volcano -----------------------------------------------------------

#' @title DAM_volcano
#' @description volcano plot for DAM analysis result.
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param x 'dataframe' DAM analysis result of MDAtoolkits::DM_analysis.
#' @param title "character', title for volcano plot.
#' @param pval_cut 'numeric'; pvalue cutoff, default = 0.05 
#' @param qval_cut 'numeric'; FDR / adj.p or qvalue cutoff, default = 0.05 
#' @param log2fc_cut 'numeric'; log2fc cutoff, default = 1 
#' @param VIP_cut 'numeric'; VIP cutoff, default = 1 
#' @return pubchem_item An data frame about stored the pubchem information of input cids.
#' @import ggplot2
#' @importFrom dplyr mutate case_when
#' @importFrom magrittr %>%
#' @importFrom future plan
#' @importFrom furrr future_map_dfr
#' @importFrom crayon green bold italic red yellow
#' @references See webchem https://github.com/ropensci/webchem.
#' @export
#' @examples  
#' \dontrun{
#' # import file
#' mat = step7_result %>% select("CompoundID",starts_with("sample")) %>% as.matrix
#' left_index = c("sample_01","sample_02","sample_03")
#' right_index = c("sample_04","sample_05","sample_06")
#' left = "case"
#' right = "control"
#' res_dm <- DM_analysis(x = mat,left_index,right_index,left,right)
#' }
#' DAM_volcano(x = res_dm$Diff_result_clean)
DAM_volcano = function(x,title,pval_cut = 0.05,log2fc_cut = 1,qval_cut = 0.05, VIP_cut = 1){
  x %>%
    mutate(group = case_when(
      pvalue <= pval_cut & VIP >= VIP_cut & log2fc > log2fc_cut & FDR <= qval_cut ~ "up",
      pvalue <= pval_cut & VIP >= VIP_cut & log2fc < -log2fc_cut & FDR <= qval_cut~ "down",
      TRUE ~ "not significant"
    ),
    log10pval = -log10(pvalue)
    )-> vocdata
  if(qval_cut <= pval_cut) {yinter = qval_cut} else {yinter = pval_cut}
  if(max(vocdata$log2fc) == Inf) {x.max = 10} else {x.max = max(vocdata$log2fc) }
  p = ggplot(data = vocdata,aes(x = log2fc,y = log10pval))+
    geom_point(aes(color = group,size = VIP))+
    scale_color_manual(values = c("blue","grey","red"))+
    scale_size(range = c(0.05,1))+
    theme_bw()+
    ylab(expression(paste(-log10,("p-value"))))+
    xlab(expression(paste(log2,"(fold change)")))+
    xlim(-x.max-1,x.max+1)+
    ylim(0,max(vocdata$log10pval)+1)+
    ggtitle(title)+
    annotate("text",x = x.max-1,y = max(vocdata$log10pval)-1,
             label = paste0("up regular:",nrow(filter(vocdata,group == "up"))),
             color = "red",size = 3)+
    annotate("text",x = -x.max+1,y = max(vocdata$log10pval)-1,
             label = paste0("down regular:",nrow(filter(vocdata,group == "down"))),
             color = "blue",size = 3)+
    geom_hline(yintercept = -log10(yinter),lty=3,col="black",lwd=0.5)+
    geom_vline(xintercept = log2fc_cut,lty=3,col="black",lwd=0.5)+
    geom_vline(xintercept = -log2fc_cut,lty=3,col="black",lwd=0.5)+
    theme(
      axis.text = element_text(size = 12,color = 'black'),
      axis.title = element_text(size = 13,colour = "black"),
      panel.border = element_rect(linewidth = 1,color = "black")
    )
  return(p)
}