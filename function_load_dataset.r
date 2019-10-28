# Ez a f�ggv�ny import�lja az Excel filet az elemz�shez, az 
# NA �rt�kekt�l, �s a h�tv�g�kt�l megtiszt�tott adatb�zist adja �rt�k�l. 

# A bemeneti v�ltoz�k: 
# filename: az Excel-file el�r�si �tvonala stringk�nt

load_dataset <- function(filename = "WTI2.xlsx"){
  
  WTI_df <- readxl::read_excel(filename) %>% 
    dplyr::as_tibble() %>% na.omit() %>% 
    dplyr::mutate(day = as.POSIXlt(Date)$wday) %>%
    dplyr::filter(day != 6) %>% dplyr::filter(day != 0) %>%
    return()
  
}             

