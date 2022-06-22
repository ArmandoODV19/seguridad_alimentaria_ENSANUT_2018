### funciones para obtener data frames

security_df <- function(x = seguridad_alimentaria){
  x %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = n/44151)
}

security_df()




security_state_df <- function(x = seguridad_alimentaria, state){
  sta <- x %>%
    select(alimentaria, entidad) %>%
    filter(entidad == state) %>%
    group_by(alimentaria, entidad) %>%
    count() %>%
    summarise(total = sum(n)) %>%
    ungroup(alimentaria) %>%
    summarize(total = sum(total))

  sta_total <- sta$total

  x %>%
    filter(entidad == state) %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = n/sta_total)
}

security_state_df(state = "Tlaxcala")


security_domain_df <- function(x = seguridad_alimentaria){
  rur <- x %>%
    select(alimentaria, dominio) %>%
    group_by(alimentaria, dominio) %>%
    count() %>%
    filter(dominio == "rural") %>%
    summarise(total = sum(n)) %>%
    ungroup(alimentaria) %>%
    summarize(total = sum(total))

  rur_total <- rur$total

  urb <- x %>%
    select(alimentaria, dominio) %>%
    group_by(alimentaria, dominio) %>%
    count() %>%
    filter(dominio == "urbano") %>%
    summarise(total = sum(n)) %>%
    ungroup(alimentaria) %>%
    summarize(total = sum(total))

  urb_total <- urb$total

  x %>%
    select(alimentaria, dominio) %>%
    group_by(alimentaria, dominio) %>%
    count() %>%
    mutate(porcentaje = case_when(dominio == "urbano" ~ n/urb_total,
                                  dominio == "rural" ~ n/rur_total))
}

security_domain_df()


security_domain_state_df <- function(x = seguridad_alimentaria, state){
  rur <- x %>%
    select(alimentaria, dominio, entidad) %>%
    filter(entidad == state) %>%
    group_by(alimentaria, dominio) %>%
    count() %>%
    filter(dominio == "rural") %>%
    summarise(total = sum(n)) %>%
    ungroup(alimentaria) %>%
    summarize(total = sum(total))

  rur_total <- rur$total

  urb <- x %>%
    select(alimentaria, dominio, entidad) %>%
    filter(entidad == state) %>%
    group_by(alimentaria, dominio) %>%
    count() %>%
    filter(dominio == "urbano") %>%
    summarise(total = sum(n)) %>%
    ungroup(alimentaria) %>%
    summarize(total = sum(total))

  urb_total <- urb$total

  x %>%
    select(alimentaria, dominio, entidad) %>%
    filter(entidad == state) %>%
    group_by(alimentaria, dominio) %>%
    count() %>%
    mutate(porcentaje = case_when(dominio == "urbano" ~ n/urb_total,
                                  dominio == "rural" ~ n/rur_total))
}

security_domain_state_df(state = "Morelos")


security_zone_df <- function(x = seguridad_alimentaria, zone){
  reg <- x %>%
    select(alimentaria, region) %>%
    filter(region == zone) %>%
    group_by(alimentaria, region) %>%
    count() %>%
    summarise(total = sum(n)) %>%
    ungroup(alimentaria) %>%
    summarize(total = sum(total))

  reg_total <- reg$total

  x %>%
    filter(region == zone) %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = n/reg_total)
}

security_zone_df(zone = "centro")


security_domain_zone_df <- function(x = seguridad_alimentaria, zone){
  rur <- x %>%
    select(alimentaria, dominio, region) %>%
    filter(region == zone) %>%
    group_by(alimentaria, dominio) %>%
    count() %>%
    filter(dominio == "rural") %>%
    summarise(total = sum(n))

  rur_total <- rur$total

  urb <- x %>%
    select(alimentaria, dominio, region) %>%
    filter(region == zone) %>%
    group_by(alimentaria, dominio) %>%
    count() %>%
    filter(dominio == "urbano") %>%
    summarise(total = sum(n))

  urb_total <- urb$total

  x %>%
    select(alimentaria, dominio, region) %>%
    filter(region == zone) %>%
    group_by(alimentaria, dominio) %>%
    count() %>%
    mutate(porcentaje = case_when(dominio == "urbano" ~ n/urb_total,
                                  dominio == "rural" ~ n/rur_total))
}

security_domain_zone_df(zone = "sur")
