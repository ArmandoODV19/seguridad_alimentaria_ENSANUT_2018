security_df <- function(x = seguridad_alimentaria){
  x %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = freq/44151)
}
