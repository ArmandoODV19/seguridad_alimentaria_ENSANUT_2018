### funciones para obtener data frames

security_df <- function(x = seguridad_alimentaria){
  x %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = freq/44151)
}

security_df()




security_state_df <- function(x = seguridad_alimentaria, state){
  sta <- x %>%
    select(alimentaria, entidad) %>%
    filter(entidad == state) %>%
    group_by(alimentaria, entidad) %>%
    count() %>%
    summarise(total = sum(freq))

  sta_total <- sta$total

  x %>%
    filter(entidad == state) %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = freq/sta_total)
}

security_state_df(state = "Morelos")


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

security_domain_df()


security_domain_state_df <- function(x = seguridad_alimentaria, state){
  rur <- x %>%
    select(alimentaria, dominio, entidad) %>%
    filter(entidad == state) %>%
    group_by(alimentaria, dominio) %>%
    count() %>%
    filter(dominio == "rural") %>%
    summarise(total = sum(freq))

  rur_total <- rur$total

  urb <- x %>%
    select(alimentaria, dominio, entidad) %>%
    filter(entidad == state) %>%
    group_by(alimentaria, dominio) %>%
    count() %>%
    filter(dominio == "urbano") %>%
    summarise(total = sum(freq))

  urb_total <- urb$total

  x %>%
    select(alimentaria, dominio, entidad) %>%
    filter(entidad == state) %>%
    group_by(alimentaria, dominio) %>%
    count() %>%
    mutate(porcentaje = case_when(dominio == "urbano" ~ freq/urb_total,
                                  dominio == "rural" ~ freq/rur_total))
}

security_domain_state_df(state = "Aguascalientes")
