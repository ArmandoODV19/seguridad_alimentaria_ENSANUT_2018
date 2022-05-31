#### generando funciones


# funcion para obtner seguridad alimentaria a nivel nacional

security_plot <- function(x = seguridad_alimentaria){
  x %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = n/44151) %>%
    ggplot(aes(x = alimentaria, y = porcentaje, fill = alimentaria))+
    scale_x_discrete(limits=c("seguridad_alimentaria","inseguridad_leve",
                              "inseguridad_moderada", "inseguridad_severa"))+
    geom_col()+
    ylab("")+
    xlab("")+
    ylim(0,1)+
    theme_grey()+
    theme_classic() +
    theme(legend.position="none")
}

security_plot()


# funcion para obtener seguridad alimentaria por estado

security_state_plot <- function(x = seguridad_alimentaria, state){
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
    mutate(porcentaje = freq/sta_total) %>%
    ggplot(aes(x = alimentaria, y = porcentaje, fill = alimentaria))+
    geom_col()+
    scale_x_discrete(limits=c("seguridad_alimentaria","inseguridad_leve",
                              "inseguridad_moderada", "inseguridad_severa"))+
    ylab("")+
    xlab("")+
    ylim(0,1)+
    theme_grey()+
    theme(legend.position="none")
}

security_state_plot(state = "Morelos")

### funcion para graficar seguridad alimentaria por dominio

security_domain_plot <- function(x = seguridad_alimentaria){
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
                                  dominio == "rural" ~ freq/rur_total)) %>%
    ggplot(aes(x = alimentaria, y = porcentaje, fill = alimentaria))+
    geom_col()+
    scale_x_discrete(limits=c("seguridad_alimentaria","inseguridad_leve",
                              "inseguridad_moderada", "inseguridad_severa"))+
    ylab("")+
    xlab("")+
    ylim(0,1)+
    theme_grey()+
    theme(legend.position="none")+
    facet_wrap(.~dominio)
}

security_domain_plot()


### funcion para graficar seguridad alimentaria por dominio y estado

security_domain_state_plot <- function(x = seguridad_alimentaria, state){
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
                                  dominio == "rural" ~ freq/rur_total)) %>%
    ggplot(aes(x = alimentaria, y = porcentaje, fill = alimentaria))+
    geom_col()+
    scale_x_discrete(limits=c("seguridad_alimentaria","inseguridad_leve",
                              "inseguridad_moderada", "inseguridad_severa"))+
    ylab("")+
    xlab("")+
    ylim(0,1)+
    theme_grey()+
    theme(legend.position="none")+
    facet_wrap(.~dominio)
}

security_domain_state_plot(state = "Morelos")


### funcion para graficar seguridad alimentaria por region


security_zone_plot <- function(x = seguridad_alimentaria, zone){
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
    mutate(porcentaje = freq/reg_total) %>%
    ggplot(aes(x = alimentaria, y = porcentaje, fill = alimentaria))+
    geom_col()+
    scale_x_discrete(limits=c("seguridad_alimentaria","inseguridad_leve",
                              "inseguridad_moderada", "inseguridad_severa"))+
    ylab("")+
    xlab("")+
    ylim(0,1)+
    theme_grey()+
    theme(legend.position="none")
}

security_zone_plot(zone = "centro")


### funcion para graficar seguridad alimentaria por region y dominio

security_domain_zone_plot <- function(x = seguridad_alimentaria, zone){
  rur <- x %>%
    select(alimentaria, dominio, region) %>%
    filter(region == zone) %>%
    group_by(alimentaria, dominio) %>%
    count() %>%
    filter(dominio == "rural") %>%
    summarise(total = sum(freq))

  rur_total <- rur$total

  urb <- x %>%
    select(alimentaria, dominio, region) %>%
    filter(region == zone) %>%
    group_by(alimentaria, dominio) %>%
    count() %>%
    filter(dominio == "urbano") %>%
    summarise(total = sum(freq))

  urb_total <- urb$total

  x %>%
    select(alimentaria, dominio, region) %>%
    filter(region == zone) %>%
    group_by(alimentaria, dominio) %>%
    count() %>%
    mutate(porcentaje = case_when(dominio == "urbano" ~ freq/urb_total,
                                  dominio == "rural" ~ freq/rur_total)) %>%
    ggplot(aes(x = alimentaria, y = porcentaje, fill = alimentaria))+
    geom_col()+
    scale_x_discrete(limits=c("seguridad_alimentaria","inseguridad_leve",
                              "inseguridad_moderada", "inseguridad_severa"))+
    ylab("")+
    xlab("")+
    ylim(0,1)+
    theme_grey()+
    theme(legend.position="none")+
    facet_wrap(.~dominio)
}

security_domain_zone_plot(zone = "sur")
