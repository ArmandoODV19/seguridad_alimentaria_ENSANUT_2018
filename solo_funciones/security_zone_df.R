security_zone_df <- function(x = seguridad_alimentaria, zone){
  reg <- x %>%
    select(alimentaria, region) %>%
    filter(region == zone) %>%
    group_by(alimentaria, region) %>%
    count() %>%
    summarise(total = sum(freq))

  reg_total <- reg$total

  x %>%
    filter(region == zone) %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = freq/reg_total)
}
