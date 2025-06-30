
# Cargar paquetes ---------------------------------------------------------
# Instalar paquetes desde Github
# remotes::install_github("MathiasHarrer/dmetar")
# 
# remotes::install_github("daniel1noble/orchaRd")

# Cargar paquetes
pacman::p_load(metafor,
               meta,
               orchaRd,
               metaviz,
               dmetar,
               scico,
               tidyverse)


# Cargar datos ------------------------------------------------------------
datos <- dat.bornmann2007 |> 
  # Crear identificador único
  rowid_to_column(var = "id")



# Calcular estimadores ----------------------------------------------------
datos <- escalc(measure = "OR",
                ai = waward,
                n1i = wtotal,
                ci = maward,
                n2i = mtotal,   
                data = datos,
                slab = study)


# Ajustar modelo ----------------------------------------------------------
## Metafor
mod <- rma.mv(yi = yi,               
              V = vi,                
              random = ~ 1|study/id,
              mods = ~ type,
              data = datos)


## Meta
mod1 <- metabin(event.e = waward, 
                n.e = wtotal,
                event.c = maward,
                n.c = mtotal,
                data = datos,
                sm = "OR",
                common = FALSE,
                studlab = id,
                cluster = study,
                subgroup = type)



# Salida modelos ----------------------------------------------------------
## Metafor
mod_results(mod, group = "study", mod = "type")

## Meta
mod1


# Gráficos ----------------------------------------------------------------
## Orchard plot básico
orchard_plot(
  object = mod,
  mod = "1",
  group = "study",
  xlab = "log(OR)", 
  k = TRUE,
  g = TRUE)


## Orchard plot mejorado
orchard_plot(
  mod,
  mod = "type",
  group = "study",
  xlab = "log(OR)",
  k = TRUE,              
  g = TRUE) +
  
  # Escalas colorblind-friendly
  scale_color_scico_d(palette = "hawaii") + # color de borde
  scale_fill_scico_d(palette = "hawaii")    # color de relleno
