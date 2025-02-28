
# Integración -------------------------------------------------------------

# Funciones de enlace (join)

library(readxl)
library(dplyr)

read_xlsx('02 - integracion.xlsx', skip = 1,
          col_names = c("NOMBRE","APELLIDO1","APELLIDO2",
                        "DISTRITO","EDAD","CARRERA")) -> datos_estudiantes

read_excel('02 - integracion.xlsx', sheet = "ZONAS") -> datos_distritos

datos_estudiantes |> 
  inner_join(datos_distritos) -> datos_integrados

datos_estudiantes |> 
  left_join(datos_distritos) -> datos_left

datos_estudiantes |> 
  right_join(datos_distritos) -> datos_right

datos_estudiantes |> 
  full_join(datos_distritos) -> datos_full

datos_estudiantes[datos_estudiantes$DISTRITO=="CARMEN DE LA LEGUA REYNOSO",]$DISTRITO = "CARMEN DE LA LEGUA-REYNOSO"

datos_estudiantes |> 
  left_join(datos_distritos) -> datos_left

read_excel('02 - integracion.xlsx', range = "ZONAS2!B2:C45") -> datos_distritos2

datos_estudiantes |> 
  inner_join(datos_distritos2) -> datos_integrados2

datos_estudiantes |> 
  inner_join(datos_distritos2, 
             by = c("DISTRITO"="Distritos")) -> d_int2

datos_estudiantes |> 
  full_join(datos_distritos2, 
            by = c("DISTRITO"="Distritos")) -> d_full2

datos_estudiantes |> 
  left_join(datos_distritos2, 
            by = c("DISTRITO"="Distritos")) -> d_left2

datos_estudiantes |> 
  right_join(datos_distritos2,
             by = c("DISTRITO"="Distritos")) -> d_right2

# Resolucion de la pregunta

library(fs)
library(purrr)

dir_ls('02 - datos integracion', regexp = "\\.xlsx$") |> 
  purrr::map(read_xlsx) -> DATOS

DATOS[[1]] -> datos_carreras
DATOS[[2]] -> datos_estudiantes
DATOS[[3]] -> datos_zonas
datos_estudiantes |> 
  inner_join(datos_distritos) |> 
  inner_join(datos_carreras) -> datos_integrados

table(datos_integrados$FACULTAD,datos_integrados$ZONA)

chisq.test(datos_integrados$ZONA,datos_integrados$FACULTAD) -> prueba_chi
prueba_chi #H0: independencia #H1: No independencia (asociación)
prueba_chi$expected

datos_integrados |> sample_n(100) # muestra aleatoria

library(writexl)
write_xlsx(x = datos_integrados, path = "02 - transformacion.xlsx")

# Transformación de datos -------------------------------------------------

read_excel("02 - transformacion.xlsx") -> datos_integrados

read_excel("02 - notas.xlsx") -> datos_notas

datos_integrados |> inner_join(datos_notas, 
                               by = c("NOMBRE"="NOMBRE",
                                      "APELLIDO1"="APELLIDOP",
                                      "APELLIDO2"="APELLIDOM")) -> datos_transformacion

datos_transformacion |> str()

library(lubridate)
library(readr)

datos_transformacion |> 
  mutate('NOMBRE COMPLETO'=paste(NOMBRE,APELLIDO1,APELLIDO2)) |> 
  mutate(FNAC = as.Date(FNAC)) |> 
  mutate(EDAD = ((today()-FNAC)/365) |> floor() |> as.numeric()) -> datos_transformado

datos_transformado$`NOMBRE COMPLETO`
datos_transformado$NOMBRE

datos_transformado$NOTA |> hist()
datos_transformado$NOTA |> shapiro.test()

library(car)
datos_transformado$NOTA |> powerTransform() -> trans_bc
trans_bc$lambda

bcPower(datos_transformado$NOTA,trans_bc$lambda) |> hist()
bcPower(datos_transformado$NOTA,trans_bc$lambda) |> shapiro.test()

datos_transformado |> 
  mutate(NOTA1 = bcPower(NOTA,trans_bc$lambda)) -> datos_transformado

set.seed(230) 
x = rnorm(1000,5,1) 
x11();par(mfrow=c(1,3)) 
x|> hist() 
x |> log() |> hist() 
x**2 |> hist()

X11();par(mfrow=c(1,3))
datos_transformado$NOTA |> hist()
datos_transformado$NOTA |> log() |> hist()
datos_transformado$NOTA**2 |> hist()

datos_transformado |> 
  mutate(RANGO_EDAD = cut(EDAD, breaks = c(0,11,17,29,59,120),
                          labels = c("Niño","Adolescente","Joven","Adulto","Adulto mayor"))) -> datos_transformado

datos_transformacion |> 
  mutate('NOMBRE COMPLETO'= paste(NOMBRE,APELLIDO1,APELLIDO2),
         FNAC       = as.Date(FNAC),
         EDAD       = ((today()-FNAC)/365) |> ceiling()  |> as.numeric(),
         NOTA1      = bcPower(NOTA,trans_bc$lambda),
         RANGO_EDAD = cut(EDAD, 
                          breaks = c(-Inf,11,17,29,59,Inf),
                          labels = c("Niño","Adolescente","Joven","Adulto","Adulto mayor"))) -> datos_transformado

library(sjmisc)
datos_transformado |> 
  to_dummy(ZONA, suffix = "label") |> 
  bind_cols(datos_transformado) |> 
  select(-ZONA_CALLAO) -> datos_transformado

datos_transformado |> transmute_if(is.character,toupper)

datos_transformado |> transmute_if(is.numeric,log)

datos_transformado |> transmute_at(c("NOMBRE","APELLIDO1"),toupper)

datos_transformado |> mutate_if(is.numeric,log)

write.csv(datos_transformado, '02 - normalizacion.csv')

write.csv(datos_transformado, '02 - normalizacion_A.csv', row.names = FALSE)

# Normalizacion -----------------------------------------------------------

read_csv('02 - normalizacion.csv', 
         locale = locale(encoding = "latin1")) -> datos_normalizacion

datos_normalizacion |> mutate(NOTA2 = NOTA |> scale()) -> datos_normalizados

datos_normalizados$NOTA2 |> hist()

minmax <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))}

datos_normalizacion |> mutate(NOTA2 = NOTA |> minmax()) -> datos_reduccion

datos_reduccion$NOTA2 |> hist()

datos_reduccion$FACULTAD |> table()

# Limpieza ----------------------------------------------------------------

library(skimr)

datos_reduccion |> skim()

datos_reduccion[datos_reduccion$FACULTAD=="Meteorología",]$FACULTAD = "Ciencias"

datos_reduccion |> skim()

datos_reduccion |> 
  filter(FACULTAD=="Ciencias" & EDAD %in% c(20,21))

datos_reduccion |> 
  filter(FACULTAD=="Zootecnia" & EDAD >=18 & EDAD < 24)

datos_reduccion |> 
  filter(FACULTAD=="Ciencias" | EDAD %in% c(20,21))

datos_reduccion |> 
  filter(FNAC >= as.Date("2000-01-01"))

# Reduccion ---------------------------------------------------------------

datos_reduccion |> 
  select(-...1,-NOMBRE,-APELLIDO1,-APELLIDO2,-FNAC,-NOTA,-NOTA1,-EDAD) |> 
  select(`NOMBRE COMPLETO`, DISTRITO, ZONA_CENTRO, ZONA_ESTE, ZONA_NORTE, ZONA_SUR, 
         RANGO_EDAD,CARRERA,FACULTAD,NOTA2)-> datos_final

datos_reduccion |> 
  select_if(is.numeric) |> 
  select(-'...1')

datos_reduccion |> 
  select_at(vars(starts_with('APE')),tolower)

datos_reduccion |> 
  select_at(c("NOMBRE","APELLIDO1"),tolower)

library(skimr)
datos_final |> skim()
