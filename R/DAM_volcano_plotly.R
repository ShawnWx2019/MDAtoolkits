# DAM_volcano -----------------------------------------------------------

#' @title DAM_volcano_plotly
#' @description Interactive volcano plot for DAM analysis result.
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param x 'dataframe' DAM analysis result of MDAtoolkits::DM_analysis.
#' @param title "character', title for volcano plot.
#' @param pval_cut 'numeric'; pvalue cutoff, default = 0.05 
#' @param qval_cut 'numeric'; FDR / adj.p or qvalue cutoff, default = 0.05 
#' @param log2fc_cut 'numeric'; log2fc cutoff, default = 1 
#' @param VIP_cut 'numeric'; VIP cutoff, default = 1 
#' @return ggplot2 object
#' @import ggplot2
#' @importFrom dplyr mutate case_when
#' @importFrom magrittr %>%
#' @importFrom plotly plot_ly layout add_lines
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
#' DAM_volcano_plotly(x = res_dm$Diff_result_clean)
DAM_volcano_plotly <- function(x, title, pval_cut = 0.05, log2fc_cut = 1, qval_cut = 0.05, VIP_cut = 1) {
  # 数据处理，增加分组和悬停文本
  x %>%
    mutate(
      group = case_when(
        pvalue <= pval_cut & VIP >= VIP_cut & log2fc > log2fc_cut & FDR <= qval_cut ~ "up",
        pvalue <= pval_cut & VIP >= VIP_cut & log2fc < -log2fc_cut & FDR <= qval_cut ~ "down",
        TRUE ~ "not significant"
      ),
      log10pval = -log10(pvalue),
      log10qval = -log10(FDR),
      hover_text = paste("Metabolite:", x[,1], "<br>Left:", x[,2], "<br>Right:", x[,3],
                         "<br>log2FC:", log2fc, "<br>P-value:", pvalue, "<br>FDR:", FDR, "<br>VIP:", VIP)
    ) %>%
    rowwise() %>%
    mutate(customdata = paste(across(!hover_text, ~ as.character(.x)), collapse = "|")) -> vocdata
  if(max(vocdata$log2fc,na.rm = T) == Inf) {x.max = 10} else {x.max = max(vocdata$log2fc,na.rm = T) }
  if(min(vocdata$log2fc,na.rm = T) == Inf) {x.min = -10} else {x.min = min(vocdata$log2fc,na.rm = T) }
  # 根据 pval_cut 和 qval_cut 选择 y 轴
  if (qval_cut <= pval_cut) {
    yaxis <- vocdata$log10qval
    ylab <- "log10(Q-value)"
  } else {
    yaxis <- vocdata$log10pval
    ylab <- "log10(P-value)"
  }
  
  # 使用 plotly 绘图
  p <- plot_ly(data = vocdata, x = ~log2fc, y = ~yaxis, type = 'scatter', mode = 'markers',
               text = ~hover_text, color = ~group, size = ~VIP,customdata = ~ customdata,
               colors = c("blue", "grey", "red"),
               sizes = c(1, 25),
               marker = list(line = list(color = 'rgba(0, 0, 0, 0.8)', width = 0.5))) %>%
    layout(title = title,
           xaxis = list(title = "log2(Fold Change)"),
           yaxis = list(title = ylab),
           hovermode = 'closest')
  
  # 添加线条
  p= p %>% add_lines(x = c(x.min,x.max),y = 3,inherit = F,line = list(color = "black", width = 1.5, dash = 'dash')) %>%
    add_lines(x = log2fc_cut,y = c(0,max(yaxis)),inherit = F,line = list(color = "black", width = 1.5, dash = 'dash')) %>%
    add_lines(x = -log2fc_cut,y = c(0,max(yaxis)),inherit = F,line = list(color = "black", width = 1.5, dash = 'dash'))
  
  return(p)
}