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
    select(Compound_id,starts_with("QC"),-QC_mean) %>% 
    pivot_longer(contains("QC"),names_to = 'tag',values_to = 'value') %>% 
    select(-tag) %>% 
    group_by(Compound_id) %>% 
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

#' @title extract_fragment
#' @description Running mrm selection for tidymass workflow.
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param ms2_data 'list', ms2 information in mass_dataset object
#' @param vari_id 'character', variable_id of features have ms2 information
#' @param spec_id 'character', spectrum id
#' @return A dataframe, with MRM selection result.
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate relocate left_join group_by arrange filter slice_head rename
#' @importFrom purrr map2_dfr
#' @references 


extract_fragment <- function(ms2_data,vari_id,spec_id) {
  var2spec = data.frame(
    variable_id = vari_id,
    ms2_spectrum_id = spec_id
  )
  out = map2_dfr(.x = spec_id,.y = ms2_data,.f = function(.x,.y) {
    MS2_df <-
      .y %>% 
      as.data.frame() %>% 
      mutate(
        ms2_spectrum_id = .x
      ) %>% 
      relocate(ms2_spectrum_id,.before = mz)
  })
  mrm_out <- 
    out %>% 
    left_join(var2spec,by = 'ms2_spectrum_id') %>%
    relocate(variable_id,.before = ms2_spectrum_id) %>% 
    mutate(precursor = str_extract(ms2_spectrum_id,"(?<=mz).*(?=rt)") %>% as.numeric()) %>% 
    group_by(variable_id) %>% 
    arrange(variable_id,desc(intensity)) %>% ## order fragment by intensity
    mutate(fragment.order = 1:length(variable_id),
           gap = precursor - mz) %>% 
    filter(gap > 15) %>% # add tags to fragments which have mz gap with precursor ions larger than 15
    slice_head(n = 1) %>% # save the 1st fragment as the product ion
    rename('product' = "mz") %>% 
    select(variable_id,ms2_spectrum_id,precursor,product)
  return(mrm_out)
}

#' @title mrm_selection_tidymass
#' @description Running mrm selection for tidymass workflow.
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>.
#' \email{shawnwang2016@@126.com}
#' @param obj_ms2 'mass_dataset', a mass_dataset object with ms2 annotation
#' @return A dataframe, with MRM selection result.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter left_join
#' @importFrom massdataset activate_mass_dataset extract_ms2_data
#' @importFrom tidyr drop_na
#' @importFrom purrr map2_dfr
#' @references 
#' @export

oneStepMRMselection <- function(obj_ms2){
  x = extract_ms2_data(obj_ms2)
  x = x %>% setNames("MS2")
  ms2_data <- x$MS2@ms2_spectra
  vari_id <- x$MS2@variable_id
  spec_id <- x$MS2@ms2_spectrum_id
  res_mrm <- extract_fragment(ms2_data = ms2_data,vari_id = vari_id,spec_id = spec_id)
  
  ms2_tag <- data.frame(
    variable_id = vari_id,
    MS2_info = "yes"
  )
  object_new <- 
    obj_ms2 %>% activate_mass_dataset('variable_info') %>%
    left_join(ms2_tag,by = 'variable_id') %>% 
    filter(MS2_info == 'yes') %>% 
    left_join(res_mrm,by = 'variable_id') %>% 
    drop_na()
  return(object_new)
}