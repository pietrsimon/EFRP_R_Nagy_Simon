# Ez a függvény importálja az Excel filet az elemzéshez, az 
# NA értékektõl, és a hétvégéktõl megtisztított adatbázist adja értékül. 

# A bemeneti változók: 
# filename: az Excel-file elérési útvonala stringként

load_dataset <- function(filename = "WTI2.xlsx"){
  
  WTI_df <- readxl::read_excel(filename) %>% 
    dplyr::as_tibble() %>% na.omit() %>% 
    dplyr::mutate(day = as.POSIXlt(Date)$wday) %>%
    dplyr::filter(day != 6) %>% dplyr::filter(day != 0) %>%
    return()
  
}             

