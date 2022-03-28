security_domain_df <- function(x = seguridad_alimentaria){
  rur <- x %>%
    select(alimentaria, dominio) %>%
    group_by(alimentaria, dominio) %>%
    count() %>%
    filter(dominio == "rural") %>%
    summarise(total = sum(freq))

  rur_total <- rur$total

  urb <- x %>%
    select(alimentaria, dominio) %>%
    group_by(alimentaria, dominio) %>%
    count() %>%
    filter(dominio == "urbano") %>%
    summarise(total = sum(freq))

  urb_total <- urb$total

  x %>%
    select(alimentaria, dominio) %>%
    group_by(alimentaria, dominio) %>%
    count() %>%
    mutate(porcentaje = case_when(dominio == "urbano" ~ freq/urb_total,
                                  dominio == "rural" ~ freq/rur_total))
}
