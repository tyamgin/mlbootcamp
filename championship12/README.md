Решение на 9 место (68 паблик)
---------------------

### Чистка данных
Выписал все подозрительные строки: ap_lo < 50 | ap_lo > 140 | ap_hi < 70 | ap_hi > 210. Вручную исправил. В большинстве по принципу "поделить на 10, умножить на 10". Также свопнул ap_hi < ap_lo.

Те, которые не понятно как исправлять, выкидываются из трейна. Для теста заполняются средним значением.

<a href="https://github.com/tyamgin/mlbootcamp/blob/master/championship12/fix.R">Файл исправлений.</a>

### Валидация
Зафиксировал 15 сплитов на 7 фолдов в начале конкурса. Честный рандом, без всяких stratified.

### Заполнение пропусков
smoke = 0
alco = 0
active = 1

### Модели
Xgboost и LightGBM. Просто среднее арифметическое.

#### Xgboost:
##### Параметры
  max_depth=4 
  gamma=0.9
  lambda=1
  alpha=10
  eta=0.075
  subsample=0.9
  colsample_bytree=0.7
  min_child_weight=10
  nrounds=175
  num_parallel_tree=1

##### Фичи
age
gender
ap_hi
ap_lo
cholesterol
gluc
smoke
alco
active
cholesterol ? 1 & gluc ? 1
lol2 = cholesterol - gluc
lol3 = cholesterol + gluc + 3*smoke + alco - 4*active
<a href="http://halls.md/race-body-fat-percentage/">fat</a> = 1.39 * weight / (height/100)^2 + 0.16 * age / 365 - 10.34 * gender - 9 
smoke ? 0 & alco ? 0
gender ? 1 & cholesterol ? 2
log(height) / log(weight)
log(age) * height^2
gender ? 1 | gluc ? 1
gender ? 0 & cholesterol ? 3
smoke ? 0 | active ? 0
log(ap_hi) * log(ap_lo)

#### LightGBM:

##### Параметры

  num_leaves=15
  nrounds=55
  learning_rate=0.223558
  max_depth=4
  lambda_l2=10
  feature_fraction=0.746088
  min_data_in_leaf=382
  bagging_fraction=0.910187

##### Фичи
age
gender
ap_hi
ap_lo
cholesterol
gluc
smoke
alco
cholesterol ? 1 & gluc ? 1
lol2 = cholesterol - gluc
log(age) * height^2
gluc ? 3 & active ? 0
cholesterol ? 1 & alco ? 0
gender ? 0 & active ? 0
gender ? 1 | smoke ? 1
sqrt(height) / log(weight)
gender ? 1 | gluc ? 3

Фичи получены брутфорсом: рандомное добавление или удаление. Проверка производилась на 5 фолдах * 7 сплитах. Сплиты другие, чтобы не переобучаться.
