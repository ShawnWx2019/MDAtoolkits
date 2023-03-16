#' @title mrm_selection
#' @description Running mrm selection for compound discoverer result.
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param x 'dataframe', "A dataframe exported by CD software 
#' @return A dataframe, with MRM selection result.
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate  rowwise group_by summarise inner_join ungroup 
#' @importFrom tidyr pivot_longer
#' @importFrom progressr progressor handlers with_progress
#' @importFrom purrr map2_dfr
#' @importFrom crayon green bold italic red yellow
#' @references See 
#' @export
#' @examples  
#' \dontrun{
#' # demo
#' x = readxl::read_xlsx("~/GeekCloud/TTQC.xlsx",sheet = 2)
#' out <- mrm_selection_cd(x = x)
#' }


mrm_selection_cd = function(x) {
  msg_yes = green$bold$italic
  msg_no = red$bold$italic
  msg_run = blue$bold$italic$underline
  msg_warning = yellow$bold$italic
  ##> fragment selection
  pick_fragment = function(precursor,fragment) {
    i = 0 # set a index
    repeat{
      i = i+1
      gap = precursor - fragment[i]
      fragment_out = data.frame(product = fragment[i],
                                gap = gap)
      if(gap > 15) {
        message(msg_yes("The fragment ",i," were selected\nThe gap of precursor and fragment: ",gap))
        break
      }
    }
    return(fragment_out)
  }
  ##> 1st filter
  y = 
    x %>% 
    filter(MS2 != "No MS2") %>%  # remove no ms2
    filter(
      Adduct == "M+H" | Adduct == "M-H" | Adduct == "M+FA-H" 
    ) %>% # adduct filter
    filter(ChargeState == 1) %>% 
    rowwise() %>% 
    mutate(
      QC_mean = mean(c_across(starts_with("QC")))
    )
  ##> calculate rsd
  judge_rsd <- 
    y %>% 
    select(Sample,starts_with("QC"),-QC_mean) %>% 
    pivot_longer(contains("QC"),names_to = 'tag',values_to = 'value') %>% 
    select(-tag) %>% 
    group_by(Sample) %>% 
    summarise(rsd = (sd(value,na.rm = T)/mean(value,na.rm = T))*100) %>% 
    filter(rsd <= 30)
  ##> remove features with rsd > 30
  y1 <-
    inner_join(y,judge_rsd)
  ##> select fragment
  precursor_ = y1$ExtractedMass %>% as.numeric()
  tmp_df <- y1 %>% select(starts_with('Fragment'))
  product_ = list()
  for (j in 1:nrow(tmp_df)) {
    product_[[j]] = tmp_df[j,] %>% as.numeric()
  }
  tbl_fragment <- map2_dfr(
    .x = precursor_, .y = product_, .f = function(.x,.y) {
      pick_fragment(.x,.y)
    }
  )
  y1$product = tbl_fragment$product
  y1$gap = tbl_fragment$gap
  y1<-ungroup(y1)
  ##> output
  return(y1)
}
