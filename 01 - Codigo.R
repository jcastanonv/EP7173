
# Paquetes ----------------------------------------------------------------

library(readr)
library(caret)
library(dplyr)
library(tidymodels)

# Lectura de datos --------------------------------------------------------

read_csv("01 - vacunas_febrero.csv") -> datos 

datos$SEXO |> levels()

datos |> 
  mutate(SEXO = parse_factor(SEXO)) -> datos

datos$SEXO |> levels()

datos |> 
  mutate(SEXO1 = recode_factor(SEXO, 
                               MASCULINO = "MASC", FEMENINO = "FEME"))|> 
  relocate(SEXO1, .after = SEXO) -> datos1


# Particionamiento usando caret -------------------------------------------

(datos |> nrow() -> ntotal)

#set.seed(3345)
datos$SEXO |> createDataPartition(p = 0.8, list = FALSE, times = 1) -> caret_indices
(caret_indices |> length() -> ntrain)
0.8*ntotal

datos[caret_indices,] -> caret_train
datos[-caret_indices,] -> caret_test

caret_train |> dim()
caret_test |> dim()

caret_train |> count(SEXO) |> mutate(Porc = n/sum(n))
caret_test |> count(SEXO) |> mutate(Porc = n/sum(n))

folds <- createFolds(caret_train$SEXO, k = 10, returnTrain = TRUE)
folds$Fold01[1:10]
folds$Fold02[1:10]
folds$Fold03[1:10]
folds$Fold04[1:10]
folds$Fold05[1:10]
folds$Fold06[1:10]
folds$Fold07[1:10]
folds$Fold08[1:10]
folds$Fold09[1:10]
folds$Fold10[1:10]
folds$Fold10 |> length()
caret_train |> dim()

folds_ <- createMultiFolds(caret_train$SEXO, k = 10, times = 1)
folds_$Fold01[1:10]
str(folds_)

folds_ <- createMultiFolds(caret_train$SEXO, k = 10, times = 2)
folds_$Fold01[1:10]
str(folds_)
folds_$Fold1.Rep1[1:10]
folds_ <- createMultiFolds(caret_train$SEXO, k = 2, times = 2)
str(folds_)
folds_$Fold1.Rep1[1:10]
folds_$Fold2.Rep1[1:10]
folds_$Fold1.Rep2[1:10]
folds_$Fold2.Rep2[1:10]

trainControl(method = "cv", # cross validation
             number = 10, # número de folds
             savePredictions = "all",
             classProbs = TRUE) -> caret_control

#train(..., trControl = caret_control, ...)

set.seed(333)
datos$SEXO |> createDataPartition(p = 0.8, list = FALSE, times = 5) -> caret_train2
set.seed(333)

trainControl(method  = "repeatedcv", # cross validation
             number  = 10, # número de folds
             repeats = 4,
             savePredictions = "all",
             classProbs = TRUE)

# train(..., trControl = caret_control, ...)

# Particionamiento usando tidymodels --------------------------------------

(datos |> initial_split(strata = SEXO, prop = 0.8) -> tidy_split)
tidy_split |> training() -> tidy_train
tidy_split |> testing() -> tidy_test

tidy_train |> dim()
tidy_test |> dim()

tidy_train |> count(SEXO) |> mutate(Porc = n/sum(n))
tidy_test |> count(SEXO) |> mutate(Porc = n/sum(n))






