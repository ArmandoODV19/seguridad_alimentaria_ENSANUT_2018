#### generando funciones

security_plot <- function(x = seguridad_alimentaria){
  x %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = freq/44151) %>%
    ggplot(aes(x = alimentaria, y = porcentaje, fill = alimentaria))+
    scale_x_discrete(limits=c("seguridad_alimentaria","inseguridad_leve",
                              "inseguridad_moderada", "inseguridad_severa"))+
    geom_col()+
    ylab("")+
    xlab("")+
    ylim(0,1)+
    theme_grey()+
    theme(legend.position="none")
}

security_plot()

