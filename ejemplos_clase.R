
# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  janitor,
  metafor,
  meta,
  orchaRd,
  tidyverse
)


# Cargar datos ------------------------------------------------------------
datos <- dat.konstantopoulos2011

datos_cor <- dat.molloy2014
# 
# datos_prev <- dat.crisafulli2020
# 
# datos_inc <- dat.nielweise2008
# 
# datos_md <- dat.furukawa2003
# 
datos_or <- dat.collins1985b



# Explorar datos ----------------------------------------------------------
glimpse(datos)

glimpse(datos_cor)

glimpse(datos_prev)

glimpse(datos_inc)

glimpse(datos_md)

glimpse(datos_or)


# Modelo para estimador precalculado --------------------------------------
mod <- metagen(TE = yi,
               seTE = vi,
               studlab = study,
               common = TRUE,    
               random = TRUE,    
               backtransf = TRUE, 
               data = datos)

summary(mod)

mod

# Quitar modelo efectos fijos y añadir intervalo de predicción
mod <- update(mod, common = FALSE, prediction = TRUE)

mod

# Gráfico
forest(mod, 
       study.results = FALSE,
       col.diamond.common = "green",
       col.diamond.random = "magenta", 
         )

## Modelo con metafor
res <- rma(yi, vi, data = datos)

# orchard plot
orchard_plot(res, group = "study", xlab = "diferencia de medias") +
  scale_fill_viridis_d()

caterpillars(res, group = "study", xlab = "x")

# Modelo para correlación -------------------------------------------------
mod_cor <- metacor(cor = ri,
                   n = ni,
                   #sm = "ZCOR",
                   data = datos_cor,
                   backtransf = FALSE,
                   prediction = TRUE)

mod_cor2 <- metacor(cor = ri,
                   n = ni,
                   sm = "COR",
                   data = datos_cor)

mod_cor

mod_cor2

forest(mod_cor)


# Modelo para prevalencia -------------------------------------------------
mod_prev <- metaprop(
  event = cases,      
  n = total,         
  studlab = study,   
  data = datos_prev, 
  sm = "PLOGIT",      
  prediction = TRUE,  
  pscale = 100       
)


# Modelo para tasa de incidencia ------------------------------------------
mod_inc <- metarate(
  event = x2i,         
  time = t2i,            
  studlab = authors,      
  data = datos_inc,       
  sm = "IRLN",            
  prediction = TRUE    
)


# Modelo para diferencia de medias ----------------------------------------
mod_md <- metacont(
  n.e = Ne,         
  mean.e = Me,      
  sd.e = Se,        
  n.c = Nc,         
  mean.c = Mc,      
  sd.c = Sc,        
  studlab = author, 
  data = datos_md, 
  sm = "SMD",       
  prediction = TRUE 
)



# Modelo para OR/RR -------------------------------------------------------
mod_or <- metabin(
  event.e = pre.xti,  
  n.e = pre.nti,      
  event.c = pre.xci,  
  n.c = pre.nci,      
  studlab = author,   
  data = datos_or,    
  sm = "OR",         
  prediction = TRUE   
)


# Modelo para IRR ---------------------------------------------------------
mod_irr <- metainc(
  event.e = x1i,         
  time.e = t1i,           
  event.c = x2i,         
  time.c = t2i,           
  studlab = authors,      
  data = datos_inc,       
  sm = "IRR",             
  prediction = TRUE       
)


# Calcular estimadores de efecto ------------------------------------------
### Buscar dataset
datsearch(pattern = "cor")

# Cargar dataset
datos_esc <- dat.aloe2013

# Explorar datos
glimpse(datos_esc)


# Calcular coeficiente de correlación
datos_esc <- escalc(measure = "ZPCOR", # correlación parcial
                    ti = tval,
                    ni = n,
                    mi = preds,
                    data = datos_esc)


## Ajustar modelo (meta)
mod_cor2 <- metacor(cor = yi,
                    n = n,
                    data = datos_esc,
                    prediction = TRUE)


summary(mod_cor2)


# Forest plot
forest(mod_cor2)


## Ajustar modelo (metafor)
mod_cor3 <- rma(yi = yi, vi = vi, data = datos_esc)

summary(mod_cor3)
