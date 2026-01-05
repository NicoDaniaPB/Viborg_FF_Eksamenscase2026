pacman::p_load("tidyverse", "forcats", "ggplot2", "glmnet", "leaps", "caret", "purrr", "car", "tibble")

# 1. Load data -------------------------------------------------------------
VFF_samlet_variabler <- readRDS("VFFdata/VFF_samlet_variabler.rds")  

# Vi konverter relevante variabler til faktorer
# Vi bruger mutate til at bygge videre på de samlet variabler, og definer dem 
# "as factor".Dette er vigtigt i forbindelse med lineær regression, hvor 
# faktorvariable behandles som kategoriske og derfor omsættes til dummyvariable 
# i matricen.
VFF_samlet_variabler <- VFF_samlet_variabler |>
  mutate(
    udehold_rang    = as.factor(udehold_rang),
    Er_helligdag    = as.factor(Er_helligdag),
    i_sommerferie   = as.factor(i_sommerferie),
    i_efterårsferie = as.factor(i_efterårsferie),
    i_vinterferie   = as.factor(i_vinterferie),
    ferieperiode    = as.factor(ferieperiode),
    udehold         = as.factor(udehold)
  )

# Vi samler udehold der kun optræder få gange i en fælles kategori. 
# Vi tæller hvor mange gange et hold opretter, og laver en ny kolonne der hedder
# "n_udehold" som tæller antallet af gange holdet gentages i datasættet.
# Vi bruger mutate og if_else til at derfiner at et "stor hold" er et hold hvor
# observation gentages mere end 2 gange. Dvs hold der gentages 2 eller færre 
# gange defineres efterfølgende som faktoren "små hold". Til sidst bruger vi 
# select til at fjerne kolonnen "n_udehold". 

VFF_samlet_variabler <- VFF_samlet_variabler |>
  add_count(udehold, name = "n_udehold") |>
  mutate(
    små_hold = if_else(n_udehold > 2, "stor hold", as.character(udehold)),
    små_hold = as.factor(små_hold)
  ) |>
  select(-n_udehold)

view(VFF_samlet_variabler)


# 2. Opdeling af data ------------------------------------------------------
# Vi bruger set.seed til at definer, at vi er gruppe 1. nrow tæller hvor rækker
# (dvs. kampe) der er i vores datasæt. vi gemmer det i et objekt vi kalder "n".
# Vi opdeler efterfølgende dataene helt tilfældigt 70/30 procent. Vi definer
# vores træningsindex som 70%, og vores test data som minus træningsindex, dvs
# 100 procent minus 70 procent, som gir 30 procent. 


set.seed(1)
n <- nrow(VFF_samlet_variabler)
train_index <- sample(1:n, size = round(0.7*n))
train_data <- VFF_samlet_variabler[train_index, ]
test_data  <- VFF_samlet_variabler[-train_index, ]

view(train_data)
view(test_data)

# Vi sikrer at test_data kun har niveauer, som modellen kender
# Vi bruger mutate til at bygge videre på test_dataene, og sørger for, udehold
# gendannes som faktor med samme levels som i træningsdataene. 
# Levels(train_data$udehold henter alle de niveauer, som udehold har i trænings-
#-datasættet
test_data <- test_data %>%
  mutate(
    udehold = factor(udehold, levels = levels(train_data$udehold))
  )

# 3. Lineær regressionsmodeller ------------------------------------------

# Vi laver en lineær regressionsmodel for at afdække potentielle sammenhænge
# i vores mange forklarende variabl

# Lineær model med billetsalg 3 dage før kampstart 
stor_lm_model <- lm(
  tilskuere ~ temp_dry + wind_speed + nedbør_seneste_7_timer +
    ugedag + placering_lag_hjemme + placering_lag_ude + mål_seneste_3_hjemme
  + mål_sæson_lag_hjemmehold + mål_sæson_lag_udehold + form_seneste_3_hjemmehold 
  + form_seneste_3_udehold + tilskuere_hold_lag + udehold_rang + sæson + runde + tidsperiode +
    Er_helligdag + i_sommerferie + i_efterårsferie + i_vinterferie +
    ferieperiode + total_income + total_expense + net_balance +
    antal_transfers + d3_tilskuere + stadion_kapacitet,
  data = train_data
)

summary(stor_lm_model)

# Test for multikollinearitet - VIF (Variance inflation factor) via car-pakken
vif(stor_lm_model)
# VIF-testen fejler, fordi vi har perfekt multikollinearitet

# Vi finder de perfekte lineære afhængigheder
alias(stor_lm_model)

# Designmatrix-rank (bekræftelse af multikollinearitet problemet
kampdag_X <- model.matrix(stor_lm_model)
qr(kampdag_X)$rank
ncol(kampdag_X)
# Vi har 57 kolonner, 45 kolonner er ikke lineært afhængige af andre kolonner. 
# Det betyder, at 12 kolonner er lineræt afhængige af andre kolonner.

# I samlet_variabler har vi vores rå data, her har vi 43 kolonner, som repræsenterer 
# vores 43 variabler. De 57 kolonner her antallet af kolonner i designmatricen 
# efter vi har kørt dummy-kodningen. De numeriske variabler giver 1 kolonne. 
# Faktorvariablerne bliver omdannet til dummies, og giver k - 1, hvor k er antallet af dummykolonner.

# Vores model lider af multikollinearitet, fordi flere af  vores variabler
# beskriver det samme fænomen. Det giver perfekt lineær afhængighed mellem 
# variablerne (dette kaldes også aliasing). Nogle af de variabler der forårsager 
# den perfekt mulitkollinearitet er bl.a. følgende:
# ugedag / tidsperiode (tidsperiode indeholder informationer om ugedag)
# Ferievariablerne (ferieperioden gentages flere gange, de er derfor perfekt
# afhængige af hinanden)
# Transfervariablerne (balance = indkomst - omkostninger, dvs perfekt afhængighed)
# Lagged tilskueretal (d3,d7,d10). Vi inkluderer dem alle og lager dem, det er 
# lagged versioner af samme variabel, og de er derfor meget stærkt korrelerede. 
# Dette giver perfekt multikollinearitet. Det vi gør nu, er at vi beholder en 
# af disse variabler, så vi ikke beskriver det samme igen og igen. 


# Ny model - 3 dage før 
stor_lm_model_ny <- lm(
  tilskuere ~ 
    temp_dry + wind_speed + nedbør_seneste_7_timer +
    tidsperiode +
    placering_lag_hjemme + placering_lag_ude +
    mål_seneste_3_hjemme +
    mål_sæson_lag_udehold +
    form_seneste_3_hjemmehold + form_seneste_3_udehold + d3_tilskuere + 
    udehold_rang +
    sæson + runde +
    ferieperiode +
    net_balance +
    stadion_kapacitet,
  data = train_data
)

summary(stor_lm_model_ny)

# Vi tester igen for multikollinearitet - VIF (Variance inflation factor) via car-pakken
# Model.matrix udtrækker desginmatricen fra modellen 
# qr() laver en QR-dekomposition af designmatricen
# $rank er antalelt af lineært uafhængige kolonner i X (som vi har defineret som
# modelmatricen)
# ncol tæller antallet af regressorer i modellen, dvs det antal af kolonner, som 
# ikke er lineært afhængige af hinanden. 
# alias() identificer hvilke variabler der er 100% lineære afhængige af hinanden. 
X <- model.matrix(stor_lm_model_ny)
qr(X)$rank
ncol(X)
alias(stor_lm_model_ny)



# Det kan ses, at net_balance skyldes fejlen. Den fjerner vi nu
stor_lm_model_ny <- lm(
  tilskuere ~ 
    temp_dry + wind_speed + nedbør_seneste_7_timer +
    tidsperiode +
    placering_lag_hjemme + placering_lag_ude +
    mål_seneste_3_hjemme +
    mål_sæson_lag_udehold +
    form_seneste_3_hjemmehold + form_seneste_3_udehold +
    d3_tilskuere +
    udehold_rang +
    sæson + runde +
    ferieperiode +
    stadion_kapacitet,
  data = train_data
)

summary(stor_lm_model_ny)

# Vi tester igen for multikollinearitet - VIF (Variance inflation factor) via car-pakken
# Model.matrix udtrækker desginmatricen fra modellen 
# qr() laver en QR-dekomposition af designmatricen
# $rank er antallet af lineært uafhængige kolonner i X (som vi har defineret som
# modelmatricen)
# ncol tæller antallet af regressorer i modellen, dvs det antal af kolonner, som 
# ikke er lineært afhængige af hinanden. 
# alias() identificer hvilke variabler der er 100% lineære afhængige af hinanden. 
X <- model.matrix(stor_lm_model_ny)
qr(X)$rank
ncol(X)
alias(stor_lm_model_ny)
# Nu er der 42/42, så ingen lineære afhængighed.
# Vi tester nu for VIF igen. 
vif(stor_lm_model_ny)

# Det kan ses, at ingen af variablerne har et kritisk VIF-niveau (over 5). 
# De fleste ligger på et moderat niveau mellem 1 og 5. Vi skal dog være OBS på,
# at mål_sæson_lag_udehold og runde nærmere sig et højt niveau. Vi har allerede
# en tårnhøj R^2, så vi prøver lige at fjerne mål_sæson_lag_udehold. 

stor_lm_model_3_dage <- lm(
  tilskuere ~ 
    temp_dry + wind_speed + nedbør_seneste_7_timer +
    tidsperiode +
    placering_lag_hjemme + placering_lag_ude +
    mål_seneste_3_hjemme +
    form_seneste_3_hjemmehold + form_seneste_3_udehold +
    d3_tilskuere +
    udehold_rang +
    sæson + runde +
    ferieperiode +
    stadion_kapacitet,
  data = train_data
)

summary(stor_lm_model_3_dage)

# Vi tester nu for VIF igen. 
vif(stor_lm_model_3_dage)

# Model 7 dage før
stor_lm_model_7_dage <- lm(
  tilskuere ~ 
    temp_dry + wind_speed + nedbør_seneste_7_timer +
    tidsperiode +
    placering_lag_hjemme + placering_lag_ude +
    mål_seneste_3_hjemme +
    form_seneste_3_hjemmehold + form_seneste_3_udehold +
    d7_tilskuere +
    udehold_rang +
    sæson + runde +
    ferieperiode +
    stadion_kapacitet,
  data = train_data
)

summary(stor_lm_model_7_dage)

# Model 10 dage før
stor_lm_model_10_dage <- lm(
  tilskuere ~ 
    temp_dry + wind_speed + nedbør_seneste_7_timer +
    tidsperiode +
    placering_lag_hjemme + placering_lag_ude +
    mål_seneste_3_hjemme +
    form_seneste_3_hjemmehold + form_seneste_3_udehold +
    d10_tilskuere +
    udehold_rang +
    sæson + runde +
    ferieperiode +
    stadion_kapacitet,
  data = train_data
)

summary(stor_lm_model_10_dage)

# Model flere måneder før
stor_lm_model_flere_måneder <- lm(
  tilskuere ~ sæson + runde + tidsperiode
  + ferieperiode + stadion_kapacitet,
  data = train_data
)

summary(stor_lm_model_flere_måneder)

vif(stor_lm_model_flere_måneder)


# 4. Predict på testdata ---------------------------------------------------
# Vi bruger predict til at tage koefficientere fra den estimerede model, (som 
# vi har defineret foroven). 
tilskuere_hat3 <- predict(stor_lm_model_3_dage, newdata = test_data)
tilskuere_hat7 <- predict(stor_lm_model_7_dage, newdata = test_data)
tilskuere_hat10 <- predict(stor_lm_model_10_dage, newdata = test_data)
tilskuere_hat_flere_måneder <- predict(stor_lm_model_flere_måneder, newdata = test_data)

# 5. Beregning af MSE, RMSE og R^2 ---------------------------------------------¨

# Funktion til beregning af RMSE og MSE
# Vi opretter en funktion, som vi kalder "calc_metrics". 
# obs er de observerede værdier (dvs faktiske tilskuertal)
# pred er forudsagtes værdier fra vores model
# Efter det skriver vi formlerne til at beregne hhv. MSE, RMSE og R^2. 
# Til sidst returnerer vi en liste med alle tre metrics.

calc_metrics <- function(obs, pred) {
  mse <- mean((obs - pred)^2)
  rmse <- sqrt(mse)
  ss_res <- sum((obs - pred)^2)
  ss_tot <- sum((obs - mean(obs))^2)
  r2 <- 1 - ss_res/ss_tot
  return(list(MSE = mse, RMSE = rmse, R2 = r2))
}

# Vi definer vores modeller, og hvad der skal beregnes
metrics_3_dage    <- calc_metrics(test_data$tilskuere, tilskuere_hat3)
metrics_7_dage    <- calc_metrics(test_data$tilskuere, tilskuere_hat7)
metrics_10_dage   <- calc_metrics(test_data$tilskuere, tilskuere_hat10)
metrics_flere_måneder   <- calc_metrics(test_data$tilskuere, tilskuere_hat_flere_måneder)

# Vi ser resultateterne for vores lineære regressionsmodeller
metrics_3_dage
metrics_7_dage
metrics_10_dage
metrics_flere_måneder


# Vi samler resultaterne i en tibble
# Vi smider de i en tibble, og bruger c (combine) til kombiner de forskellige 
# modeller for de forskellige perioder, og nøgletallene for hver model. 
metrics_samlet <- tibble(
  Model = c("3 dage før", "7 dage før", "10 dage før", "flere måneder"),
  MSE  = c(metrics_3_dage$MSE, metrics_7_dage$MSE, metrics_10_dage$MSE, metrics_flere_måneder$MSE),
  RMSE = c(metrics_3_dage$RMSE, metrics_7_dage$RMSE, metrics_10_dage$RMSE, metrics_flere_måneder$RMSE),
  R2   = c(metrics_3_dage$R2, metrics_7_dage$R2, metrics_10_dage$R2, metrics_flere_måneder$R2)
)

view(metrics_samlet)


# 6. Residualer i stor lineær model ---------------------------------------

# Vi laver et ggplot til at se residualerne, dvs hvor meget prikkerne afviger 
# fra linjen og hvor præcis modellen er. 

# Vi starter med at samle dataene til plottet. 
# vi bruger bind rows til at sættrer rækkerne fra de forskellige tabeller sammen.
# Vi har vores model, vores observeret værdi og vores forudsagte (predicted)
# værdi. 
plot_data <- bind_rows(
  tibble(
    Model = "3 dage før",
    Observed = test_data$tilskuere,
    Predicted = tilskuere_hat3
  ),
  tibble(
    Model = "7 dage før",
    Observed = test_data$tilskuere,
    Predicted = tilskuere_hat7
  ),
  tibble(
    Model = "10 dage før",
    Observed = test_data$tilskuere,
    Predicted = tilskuere_hat10
  ),
  tibble(
    Model = "Flere måneder før",
    Observed = test_data$tilskuere,
    Predicted = tilskuere_hat_flere_måneder
  )
)

# Vi laver vores ggplot ud fra vores plot_data. 
# x er = observed og y = predicted 
# geom_point() tegner et punkt for hver observation.
# alpha = 0.4 gør punkterne halvgennemsigtige
# geom_abline tegner en 45 graders linje. Hvor vi definer at den skal være 
# stiplet eller "dashed", og at farven skal være "rød".
# Vi bruger facet_wrap til at lave separate plots for hver model
# ~ betyder, at hver model får sit eget plit
# scales = "free" betydet, at hver plto kan have sin egen skal på x- og y-aksen,
# så alle punkterne passer. 
# Vi bruger labs til at lave overskrifterne. 

ggplot(plot_data, aes(x = Observed, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ Model, scales = "free") +
  labs(
    title = "Observeret vs. forudsagt tilskuertal",
    x = "Observeret",
    y = "Forudsagt"
  ) +
  theme_minimal()

# Jo tættere på kampdagen vi er, jo mindre bliver residualerne. Dette betyder,
# at modellen bliver mere og mere præcis, jo tættere på kampdagen vi kommer. 
# Årsagen  til denne udvikling, er at vi har flere variabler jo tættere på 
# kampdagen vi er. Dette betyder, at vores model kan forklare mere af variationen,
# hvilket bekræftes ved at R^2 stiger jo tættere på kampdagen vi kommer. 

# 7. Ridge- og Lasso- regression ------------------------------------------

# Lambda-grid
# Vi opretter 100 lamda-værdier fra 10^5 til 10^-3
# Vi bruger det til regularisering i shrinkage metoder (Ridge og Lasso).
# Jo større lamda-værdien er, jo større er straffen. En lille lambda-værdi, er 
# næsten det samme som almindelig lineær regression. Vi vurder 100 lambda-værdier,
# til at være tilstrækkeligt. 
lambda_grid <- 10^seq(5, -3, length = 100)

# Funktion til metrics
# Vi opretter en funktion, som vi kalder "calc_metrics". 
# obs er de observerede værdier (dvs faktiske tilskuertal)
# pred er forudsagtes værdier fra vores model
# Efter det skriver vi formlerne til at beregne hhv. MSE, RMSE og R^2. 
# Til sidst returnerer vi en liste med alle tre metrics.
calc_metrics <- function(obs, pred) {
  mse <- mean((obs - pred)^2)
  rmse <- sqrt(mse)
  ss_res <- sum((obs - pred)^2)
  ss_tot <- sum((obs - mean(obs))^2)
  r2 <- 1 - ss_res / ss_tot
  list(MSE = mse, RMSE = rmse, R2 = r2)
}

# Funktion til at lave metrics tibble
# Vi pakker vores resultater i en tibble med vores værdier, og hvilke modeltype
# det er. Formålet her, at at gære resultaterne lettere at sammenligne og samle. 
make_metrics_tibble <- function(obs, pred, model_name, lambda, periode) {
  metrics <- calc_metrics(obs, pred)
  tibble(
    Model = model_name,
    Lambda = lambda,
    MSE = metrics$MSE,
    RMSE = metrics$RMSE,
    R2 = metrics$R2,
    Periode = periode
  )
}

# Base features
# Vi definer de fælles forklarende variabler for de fleste modeller.
# Vi bruger combine (c) til at sammensætte de forskellige variabler.
base_features <- c(
  "temp_dry", "wind_speed", "nedbør_seneste_7_timer",
  "ugedag",
  "placering_lag_hjemme", "placering_lag_ude",
  "form_seneste_3_hjemmehold", "form_seneste_3_udehold",
  "udehold_rang",
  "sæson", "runde", "tidsperiode",
  "ferieperiode",
  "antal_transfers",
  "stadion_kapacitet"
)

# Features for hvert tidspunkt
# Vi laver fire modeller med forskellige tidshorisonter.
# Vi bruger combine (c) til at kombinerer base_features, som vi defineret 
# foroven, samt billetsalg varaibel og mål i seneste 3 hjemmekampe. 
# I "flere måneder"-modellen definerer hvilke variabler der skal med via 
# combine (c).
features_list <- list(
  "3 dage" = c(base_features, "d3_tilskuere", "mål_seneste_3_hjemme"),
  "7 dage" = c(base_features, "d7_tilskuere", "mål_seneste_3_hjemme"),
  "10 dage" = c(base_features, "d10_tilskuere", "mål_seneste_3_hjemme"),
  "flere måneder" = c(
    "ugedag", "sæson", "runde", "tidsperiode",
    "ferieperiode", "antal_transfers", "stadion_kapacitet"
  )
)

# Vi kører en Ridge-funktion, der vælger den bedste lambda. 
# Vi kører den for vores forskellige features for alle modeller, og den periode
# som modellerne er i. 
# Vi laver designmatricer (defineret som x_train og x_test) og sikrer, at 
# vores test-matrice har samme kolonner som train-matricen. 
# Vi træner vores Ridge-model til af alpha = 0 for alle lambda-værdier i 
# lambda-grind. Vi beregner vores nøgletal (MSE, RMSE og R^2) på testdata for 
# hver lambda. Vores Ridge-regressionsmodel returnerer den lambda og de metrics,
# som giver den laveste MSE for den valgte periode. 

run_ridge_best_lambda <- function(features, periode) {
  
  formula <- as.formula(paste("tilskuere ~", paste(features, collapse = "+")))
  
  x_train <- model.matrix(formula, train_data)[, -1]
  y_train <- train_data$tilskuere
  
  x_test_raw <- model.matrix(formula, test_data)[, -1]
  
  x_test <- matrix(0, nrow = nrow(x_test_raw), ncol = ncol(x_train))
  colnames(x_test) <- colnames(x_train)
  common_cols <- intersect(colnames(x_test_raw), colnames(x_train))
  x_test[, common_cols] <- x_test_raw[, common_cols]
  
  ridge_mod <- glmnet(x_train, y_train, alpha = 0, lambda = lambda_grid)
  
  metrics <- bind_rows(lapply(seq_along(ridge_mod$lambda), function(i) {
    pred <- predict(ridge_mod, s = ridge_mod$lambda[i], newx = x_test)
    make_metrics_tibble(
      obs = test_data$tilskuere,
      pred = pred,
      model = "Ridge",
      lambda = ridge_mod$lambda[i],
      periode = periode
    )
  }))
  
  metrics[which.min(metrics$MSE), ]
}

# Vi kører en Lasso-funktion, der vælger den bedste lambda. 
# Vi kører den for vores forskellige features for alle modeller, og den periode
# som modellerne er i. 
# Vi laver designmatricer (defineret som x_train og x_test) og sikrer, at 
# vores test-matrice har samme kolonner som train-matricen. 
# Vi træner vores Ridge-model til af alpha = 0 for alle lambda-værdier i 
# lambda-grind. Vi beregner vores nøgletal (MSE, RMSE og R^2) på testdata for 
# hver lambda. Vores Ridge-regressionsmodel returnerer den lambda og de metrics,
# som giver den laveste MSE for den valgte periode. 

run_lasso_best_lambda <- function(features, periode) {
  
  formula <- as.formula(paste("tilskuere ~", paste(features, collapse = "+")))
  
  x_train <- model.matrix(formula, train_data)[, -1]
  y_train <- train_data$tilskuere
  
  x_test_raw <- model.matrix(formula, test_data)[, -1]
  
  x_test <- matrix(0, nrow = nrow(x_test_raw), ncol = ncol(x_train))
  colnames(x_test) <- colnames(x_train)
  common_cols <- intersect(colnames(x_test_raw), colnames(x_train))
  x_test[, common_cols] <- x_test_raw[, common_cols]
  
  lasso_mod <- glmnet(x_train, y_train, alpha = 1, lambda = lambda_grid)
  
  metrics <- bind_rows(lapply(seq_along(lasso_mod$lambda), function(i) {
    pred <- predict(lasso_mod, s = lasso_mod$lambda[i], newx = x_test)
    make_metrics_tibble(
      obs = test_data$tilskuere,
      pred = pred,
      model = "Lasso",
      lambda = lasso_mod$lambda[i],
      periode = periode
    )
  }))
  
  metrics[which.min(metrics$MSE), ]
}

# Vi kører nu alle periodemodellerne for Ridge- og Lasso- regression, og samler
# resulterne i en tabel.

# Vi laver et loop over alle perioder i features_listen, og kører 
# run_ridge_best_lambda. Og vi bruger bind_rows funktioenn til at binde resultaterne
# i en tibble med den bedste lambda og de bedste metrics for hver periode. 
metrics_ridge_best <- bind_rows(
  lapply(names(features_list), function(p) {
    run_ridge_best_lambda(features_list[[p]], p)
  })
)

# Vi laver et loop over alle perioder i features_listen, og kører 
# run_lasso_best_lambda. Og vi bruger bind_rows funktioenn til at binde resultaterne
# i en tibble med den bedste lambda og de bedste metrics for hver periode. 
metrics_lasso_best <- bind_rows(
  lapply(names(features_list), function(p) {
    run_lasso_best_lambda(features_list[[p]], p)
  })
)

# Vi ser resultaterne 
view(metrics_ridge_best)
view(metrics_lasso_best)

# 8. Best subset selection ----------------------------------------------------

# Metrics-funktion
# Funktion til metrics
# Vi opretter en funktion, som vi kalder "calc_metrics". 
# obs er de observerede værdier (dvs faktiske tilskuertal)
# pred er forudsagtes værdier fra vores model
# Efter det skriver vi formlerne til at beregne hhv. MSE, RMSE og R^2. 
# Til sidst returnerer vi en liste med alle tre metrics.
calc_metrics <- function(obs, pred) {
  mse <- mean((obs - pred)^2)
  rmse <- sqrt(mse)
  ss_res <- sum((obs - pred)^2)
  ss_tot <- sum((obs - mean(obs))^2)
  r2 <- 1 - ss_res / ss_tot
  list(MSE = mse, RMSE = rmse, R2 = r2)
}

# Alignment-funktion
# Vi sørger for, at vores designmatrix (newx) får samme kolonner og rækkefælge som
# vores reference-matrix (refx).
# Vi opretter et tom matrix med det samme antal kolonner og kolonneanavne som refx.
# Vi finder kolonner, der findes i både designmatrixen og reference-matrixen. 
# Vi kopier værdier fra newx til de matchende kolonner til refx. Funktioenn 
# returnerer en matrix, hvor alle kolonner fra refx er til stede, og evt 
# manglende kolonner fra newx fyldes med "0". 
# Formålet med dette er at sikre, at train- og test- matricen har samme struktur
#, hvilket er nødtvendigt for at glmnet-pakken kan lave forudsigelser. 

align_matrix <- function(newx, refx) {
  out <- matrix(0, nrow = nrow(newx), ncol = ncol(refx))
  colnames(out) <- colnames(refx)
  common <- intersect(colnames(newx), colnames(refx))
  out[, common] <- newx[, common]
  out
}

# # Base features
# Vi definer de fælles forklarende variabler for de fleste modeller.
# Vi bruger combine (c) til at sammensætte de forskellige variabler.
base_features <- c(
  "temp_dry", "wind_speed", "nedbør_seneste_7_timer",
  "ugedag",
  "placering_lag_hjemme", "placering_lag_ude",
  "form_seneste_3_hjemmehold", "form_seneste_3_udehold",
  "udehold_rang",
  "sæson", "runde", "tidsperiode",
  "ferieperiode",
  "antal_transfers",
  "stadion_kapacitet"
)

# Features for hvert periode
# Vi laver fire modeller med forskellige tidshorisonter.
# Vi bruger combine (c) til at kombinerer base_features, som vi defineret 
# foroven, samt billetsalg varaibel og mål i seneste 3 hjemmekampe. 
# I "flere måneder"-modellen definerer hvilke variabler der skal med via 
# combine (c).
features_list <- list(
  "3 dage" = c(base_features, "d3_tilskuere", "mål_seneste_3_hjemme"),
  "7 dage" = c(base_features, "d7_tilskuere", "mål_seneste_3_hjemme"),
  "10 dage" = c(base_features, "d10_tilskuere", "mål_seneste_3_hjemme"),
  "flere måneder" = c(
    "ugedag", "sæson", "runde", "tidsperiode",
    "ferieperiode", "antal_transfers", "stadion_kapacitet"
  )
)

# Best Subset funktion
# Vi finder den model ved best subset regression.
# Vi udvælger outcome og features fra train- og testdata.
# Vi opretter designmatricer uden intercept og fjerner perfekt multikollinearitet,
# det sker ved qrX og X_train + X_test.
# Vi tilpasser test-matrix, så den har samme kolonner som train.
# Vi tester alle kombinationer af op til max_vars variable (exhaustive search).
# Vi beregner MSE, RMSE og R² på testdata for hver model.
# Til sidst eturnerer koden den model, der giver lavest RMSE, 
# med antal variable og performance-metrics.

run_best_subset <- function(features, tidspunkt, max_vars = 20, outcome_var = "tilskuere") {
  
  train_sub <- train_data %>% select(all_of(c(outcome_var, features)))
  test_sub  <- test_data  %>% select(all_of(c(outcome_var, features)))
  
  X_train_raw <- model.matrix(as.formula(paste(outcome_var, "~ . -1")), train_sub)
  X_test_raw  <- model.matrix(as.formula(paste(outcome_var, "~ . -1")), test_sub)
  
  y_train <- train_sub[[outcome_var]]
  y_test  <- test_sub[[outcome_var]]
  
  qrX <- qr(X_train_raw)
  X_train <- X_train_raw[, qrX$pivot[1:qrX$rank], drop = FALSE]
  X_test  <- align_matrix(X_test_raw, X_train)
  
  regfit <- regsubsets(x = X_train, y = y_train, nvmax = max_vars, method = "exhaustive", really.big = TRUE)
  
  metrics_list <- lapply(1:max_vars, function(k) {
    beta <- coef(regfit, id = k)
    Xk <- X_test[, names(beta)[-1], drop = FALSE]
    pred <- beta[1] + Xk %*% beta[-1]
    m <- calc_metrics(y_test, pred)
    tibble(
      Model = "Best Subset",
      Tidspunkt = tidspunkt,
      Antal_variabler = k,
      MSE  = m$MSE,
      RMSE = m$RMSE,
      R2   = m$R2
    )
  })
  
  metrics_df <- bind_rows(metrics_list)
  
  best_row <- metrics_df[which.min(metrics_df$RMSE), ]
  return(best_row)
}

# Vi kører Best Subset Method for alle perioder
# Vi looper over alle tidspunkter/perioder i features_list. Vi kører 
# run_best_subset for hver periode med de tilhørende features. 
# Til sidst binder vi alle resultater sammen i en tibble via bind_rows. 
results_subset <- bind_rows(
  lapply(names(features_list), function(tidspunkt) {
    run_best_subset(features_list[[tidspunkt]], tidspunkt)
  })
)

# 9. Samling af de forskellige modeller i en tibble ----------------------

# Hjælpefunktion
# Vi opretter en tibble med kolonnerne over vores værdier (MSE, RMSE og R^2) for
# alle modeller.
metrics_to_tibble <- function(metrics, model_name) {
  tibble(
    Model = model_name,
    MSE   = metrics$MSE,
    RMSE  = metrics$RMSE,
    R2    = metrics$R2
  )
}

# Lineær regression 
# Vi bruger bind_rows til at danne metrics for hver lm model til en tibble
metrics_lm <- bind_rows(
  metrics_to_tibble(metrics_3_dage, "3 dage før"),
  metrics_to_tibble(metrics_7_dage, "7 dage før"),
  metrics_to_tibble(metrics_10_dage, "10 dage før"),
  metrics_to_tibble(metrics_flere_måneder, "Flere måneder")
)

# Ridge regression
# Vi bruger transmute til at lave en ny tibble med kun de valgte kolonner. 
# Vi bruger paste("Ridge -") til at tilføje teksten "Ridge" foran perioden.
# Vi beholder MSE, RMSE og R^2 som performance-metrics. 
metrics_ridge <- metrics_ridge_best %>%
  transmute(
    Model = paste("Ridge –", Periode),
    MSE,
    RMSE,
    R2
  )

# Lasso regression
# Vi bruger transmute til at lave en ny tibble med kun de valgte kolonner. 
# Vi bruger paste("Lasso -") til at tilføje teksten "Lasso" foran perioden.
# Vi beholder MSE, RMSE og R^2 som performance-metrics. 
metrics_lasso <- metrics_lasso_best %>%
  transmute(
    Model = paste("Lasso –", Periode),
    MSE,
    RMSE,
    R2
  )

# Best Subset 
# Vi bruger transmute til at lave en ny tibble med kun de valgte kolonner. 
# Vi bruger paste("Best subset-") til at tilføje teksten "Best subset" 
# foran perioden. Vi beholder MSE, RMSE og R^2 som performance-metrics. 
metrics_subset <- results_subset %>%
  transmute(
    Model = paste("Best subset –", Tidspunkt),
    MSE,
    RMSE,
    R2
  )

# Vi samler alle modeller i en tibble 
# Vi bruger bind_rows til at samle alle modeller, og bruger arrange til at 
# sorter efter den bedste (laveste) MSE. 
metrics_samlet_alle <- bind_rows(
  metrics_lm,
  metrics_ridge,
  metrics_lasso,
  metrics_subset
) %>%
  arrange(MSE)

# Vi ser resultatet

View(metrics_samlet_alle)

# 10. Cross Validation MSE ----------------------------------------------------

# Vi kører K-fold CV for Lasso regressions modellen, da det er den model med den 
# laveste RMSE

# Vi laver en hjælpefunktion
# Funktionen beregner RMSE mellem de observerede værdier og de forudsagte
# (predicted) værdier. Vi skriver formlen ind for R^2, sætter at na.rm = TRUE, 
# så alle NA-værdier bliver fjernet. 

rmse <- function(obs, pred) {
  sqrt(mean((obs - pred)^2, na.rm = TRUE))
}


# Vi laver en align_matrix
# Formålet med vores funktion her, er at tilpasse en matrix (newx) til strukturen
# i en reference-matrix (refx), så de har samme kolonner. 
# out <- matrix(0, nrow = nrow(newx), ncol = ncol(refx)) Opretter en ny 
# matrix (out) fyldt med 0’er, med samme antal rækker som newx og samme antal 
# kolonner som refx.
# colnames(out) <- colnames(refx): Giver out de samme kolonnenavne som 
# reference-matricen refx.
# common <- intersect(colnames(newx), colnames(refx)): Finder de kolonnenavne, 
# som findes i begge matricer.
# out[, common] <- newx[, common]: Indsætter værdierne fra newx i de kolonner 
# i out, som de har til fælles. Kolonner, der ikke findes i newx, forbliver 0.


align_matrix <- function(newx, refx) {
  out <- matrix(0, nrow = nrow(newx), ncol = ncol(refx))
  colnames(out) <- colnames(refx)
  common <- intersect(colnames(newx), colnames(refx))
  out[, common] <- newx[, common]
  out
}

# K-fold CV funktion for LASSO (da det er vores bedste model)
# Vi laver en funktion, som evaluerer, hvor godt vores Lasso-model forudsiger
# vores y-varibel: Antallet af tilskuere. 

cv_lasso <- function(features, data, k = 5, lambda_grid) {
  
  folds <- createFolds(data$tilskuere, k = k)
  
  mse_vec  <- numeric(k)
  rmse_vec <- numeric(k)
  
  for (i in seq_along(folds)) {
    
    test_idx <- folds[[i]]
    train_cv <- data[-test_idx, ]
    test_cv  <- data[test_idx, ]
    
    formula <- as.formula(paste("tilskuere ~", paste(features, collapse = "+")))
    
    X_train <- model.matrix(formula, train_cv)[, -1]
    y_train <- train_cv$tilskuere
    
    X_test_raw <- model.matrix(formula, test_cv)[, -1]
    X_test <- align_matrix(X_test_raw, X_train)
    
    cv_fit <- cv.glmnet(
      x = X_train,
      y = y_train,
      alpha = 1,
      lambda = lambda_grid,
      standardize = TRUE
    )
    
    best_lambda <- cv_fit$lambda.min
    
  
    lasso_fit <- glmnet(
      x = X_train,
      y = y_train,
      alpha = 1,
      lambda = best_lambda,
      standardize = TRUE
    )
    
    pred <- predict(lasso_fit, s = best_lambda, newx = X_test)
    
    mse_vec[i]  <- mean((test_cv$tilskuere - pred)^2)
    rmse_vec[i] <- rmse(test_cv$tilskuere, pred)
  }
  
  tibble(
    CV_MSE  = mean(mse_vec),
    CV_RMSE = mean(rmse_vec)
  )
}

# Lambda-grid
# Vi opretter et numerisk sekvens, der starter ved 5 og slutter ved -3
# Sekvensen længde indeholder 100 jævnt fordelte værdier. 
# Hver værdi i sekvsensen bruges som en eksponent med grundtal 10, dvs 10^
lambda_grid <- 10^seq(5, -3, length = 100)
# Vores lambda_grid bliver en vektor med 100 positive tal
# Værdierne går fra 100.000 (10^5) ned til 0.001 (10^-3)
# Tallene er logaritmisk fordelt, ikke lineært. 

# Feature-lister 
# Vi definer de fælles forklarende variabler for de fleste modeller.
# Vi bruger combine (c) til at sammensætte de forskellige variabler.

base_features <- c(
  "temp_dry", "wind_speed", "nedbør_seneste_7_timer",
  "ugedag",
  "placering_lag_hjemme", "placering_lag_ude",
  "form_seneste_3_hjemmehold", "form_seneste_3_udehold",
  "udehold_rang",
  "sæson", "runde", "tidsperiode",
  "ferieperiode",
  "antal_transfers",
  "stadion_kapacitet"
)

# Features for hvert tidspunkt
# Vi laver fire modeller med forskellige tidshorisonter.
# Vi bruger combine (c) til at kombinerer base_features, som vi defineret 
# foroven, samt billetsalg varaibel og mål i seneste 3 hjemmekampe. 
# I "flere måneder"-modellen definerer hvilke variabler der skal med via 
# combine (c).
features_list <- list(
  "3 dage før" = c(base_features, "d3_tilskuere", "mål_seneste_3_hjemme"),
  "7 dage før" = c(base_features, "d7_tilskuere", "mål_seneste_3_hjemme"),
  "10 dage før" = c(base_features, "d10_tilskuere", "mål_seneste_3_hjemme"),
  "Flere måneder før" = c(
    "ugedag", "sæson", "runde", "tidsperiode",
    "ferieperiode", "antal_transfers", "stadion_kapacitet"
  )
)

# Vi kører LASSO K-fold CV

# Vi bruger set.seed(1), fordi vi er gruppe #1
# k <- 5 betyder, at vi anvender 5-fold cross-validation
set.seed(1)
k <- 5

# Vi kører LASSO-regression for hver feature periode. 
# Vi bruger map_dfr til at køre funktionen for hvert navn. Det samler alle 
# resultaterne i en data-frame. 
# Features_list vælger vores aktuelle feature sæt for hver model.
# Train_data er vores træningsdata.
# k = k: Dette er 5-fold cross-validation.
# Lambda_grid er de lambda_værdier, der testes i vores LASSO-model
# Resultatet gemmes i res(CV_MSE/RMSE).
cv_lasso_results <- map_dfr(names(features_list), function(name) {
  res <- cv_lasso(
    features = features_list[[name]],
    data = train_data,
    k = k,
    lambda_grid = lambda_grid
  )
  
  tibble(
    Model = "LASSO",
    Tidspunkt = name,
    CV_MSE  = res$CV_MSE,
    CV_RMSE = res$CV_RMSE
  )
})

# Vi ser resultaterne
View(cv_lasso_results)

# LOOCV - "leave one out cross validation" kunne her foretages, men da vi har mange faktorer med 
# mange niveauer, dette vil medføre at fjernelse af en enkelt observation, at nogle
# niveauer kun har en enkelt observation i træningsdatasættet, hvilket forhindrer dummy kodning.
# Bias variance trade-off -----
# LOOCV har lav bias men høj varians, hvorfor hver model er nærmest identisk.
# 5-fold giver en højere bias, men meget lavere varians, hvilket giver pålidelige vurderinger
# af modellens generaliserbarhed

# da vi allerede benytter 5-fold Cross validation på de lineære og lasso/ridge modeller, vil LOOCV
# ikke give ny og væsentlig information, hvorfor denne undlades.


# 11. Prædiktion  ---------------------------------------------------------

# Vi bruger Lasso-modellen til prædiktion, da det er den model der har den 
# laveste RMSE. Vi kan estimere flere måneder før, men til 10,7 og 3 dage før
# kampen, er vi nødt til at lave et skøn baseret på best og worst case scenario.

# Næste hjemmebanekamp er runde 20 i mod Brøndby IF, som skal spilles søndag 
# den 15/02/2026 klokken 18:00. 
# Deres stadionkapacitet er 9.566 tilskuere.

# Vi kan lave en prædiktion  for flere måneder. Vi kender ikke variabler som 
# fx vejrdata og billetsalg endnu, men vi kan godt estimere et interval flere 
# måneder ude. RMSE er dog højere ved modellen for flere måneder ude, hvilket 
# betyder, at intervallet vil være større end fx modellen for 3 dage, hvilket 
# er den model med den laveste RMSE ift alle modeller. I teorien giver dette også
# rigtige god mening, fordi jo flere forklarende signifikante variabler vi har, 
# jo bedre vil vores resultat være. 

# Klargøring af data
# Det første vi skal gøre, er at forberede træningsdata til vores Lasso-nodel.
# Vi starter med at konverterer karakterer/numeriske værdier til faktorer.
# Hvis vi ikke gør dette, så vil modellen fejlagitg antage en numerisk orden.
train_data <- train_data %>%
  mutate(
    ugedag      = as.factor(ugedag),
    tidsperiode = as.factor(tidsperiode),
    sæson       = as.factor(sæson)
  )

# Vi tjekker levels, og at vi har alt data med, som lige er blevet konverteret 
# til faktorer.
levels(train_data$ugedag)
levels(train_data$tidsperiode)
levels(train_data$sæson)


# Feature-lister 
# Vi definer de fælles forklarende variabler for de fleste modeller.
# Vi bruger combine (c) til at sammensætte de forskellige variabler.
features_list <- list(
  "3 dage" = c(base_features, "d3_tilskuere", "mål_seneste_3_hjemme"),
  "7 dage" = c(base_features, "d7_tilskuere", "mål_seneste_3_hjemme"),
  "10 dage" = c(base_features, "d10_tilskuere", "mål_seneste_3_hjemme"),
  "flere måneder" = c(
    "ugedag", "sæson", "runde", "tidsperiode",
    "ferieperiode", "antal_transfers", "stadion_kapacitet"
  )
)

# Vi definer "features" som "flere måneder" fra features_list
features <- features_list[["flere måneder"]]

# Vi laver en Lasso-model på vores træningsdata

# Responsevariabel
# Her definer y_træningsdata som vores responsvariabel. Det bruges som mål i 
# vores Lasso-model. 
y_train <- train_data$tilskuere

# Designmatrix for træningsdata
# Vi designer en matrix, som kan bruges direkte af glmnet. 
# model.matrix omdanner vores forklarende variable til en numerisk designmatrix.
# Kategoriske variabler (dem vi lige har konverteret til faktorer, bliver dummy-
# -kodet). 
# [,-1]: Fjerner intercept-kolonnen, og glmnet håndterer selv interceptet. 
# drop = FALSE: Sikrer, at output altid er en matrix (også ved en variabel).
X_train <- model.matrix(as.formula(paste("~", paste(features, collapse = "+"))), train_data)[, -1, drop = FALSE]

# Vi definer vores lambda-grid
# Vi opretter 100 lambda-værdier mellem 10^3 og 10^-3. Dette bestemmer, hvor 
# meget modellen straffes. 
lambda_grid <- 10^seq(3, -3, length = 100)

# Estimering af Lasso-modellen
# alpha = 1 : Lasso-regressionsmodel
# Modellen estimeres for alle lambda-værdier vi lige har sat. 
lasso_fit <- glmnet(X_train, y_train, alpha = 1, lambda = lambda_grid)

# Cross-validation for bedste lambda
# Vi finder den bedste balance mellem bias, varians og overfitting.
set.seed(1)
cv_fit <- cv.glmnet(X_train, y_train, alpha = 1, lambda = lambda_grid)
best_lambda <- cv_fit$lambda.min

# Ny kamp data
# Vi laver en tibble, hvor vi manuelt skriver værdierne ind for vores model, som
# skal forudsige tilskuereantallet flere måneder før. 
# De kategoriske variabler bliver sat som "factor, og de numeriske værdier er 
# forsat numeriske værdier.
ny_kamp_flere_måneder <- tibble(
  ugedag            = factor("Søndag", levels = levels(train_data$ugedag)),
  sæson             = factor("2025/2026", levels = levels(train_data$sæson)), 
  runde             = 20,
  tidsperiode       = factor("Søndag_aften", levels = levels(train_data$tidsperiode)),
  ferieperiode      = factor("Vinterferie", levels = levels(train_data$ferieperiode)),
  antal_transfers   = 0, 
  stadion_kapacitet = 9566
)

# Vi laver en designmatrix for den nye kampe, som matcher med vores oprindelige 
# designmatrix (X-train). 
# model.matrix() omdanner ny_kamp_flere_måneder til en numerisk matrix. 
# paste(features, collapse = "+"): Sikrer at samem features som i træningen bruges.
# [,-1]: Fjerner intercept.
# drop = FALSE: Bevarer matrix-format (selvom det kun er en række)
X_new_raw <- model.matrix(as.formula(paste("~", paste(features, collapse = "+"))), ny_kamp_flere_måneder)[, -1, drop = FALSE]

# Vi opretter en tom matrix med samme struktur som træningsdata.
# X_train indeholder alle dummy-variabler, men den nye kamp har kun en konkret
# kombination af levels. Der vil derfor mangle kolonner. Vi opretter derfor nu
# en nul-matrix med præcis samme kolonnenavne og rækkefølge som X_train. 
X_new <- matrix(0, nrow = 1, ncol = ncol(X_train))
colnames(X_new) <- colnames(X_train)

# Vi indsætter nu de relevante værdier. 
# intersect() finder de dummy-variabler, der findes i begge matricer. Kunn disse
# kolonner fyldes med 1'ere, resten forbliver 0. Hvis vores kolonner ikke matcher,
# så får vi enten en fejl eller en forkert prædiktion, derfor gør vi dette. 
common_cols <- intersect(colnames(X_train), colnames(X_new_raw))
X_new[, common_cols] <- X_new_raw[, common_cols]

# Prediktion med Lasso
# Vi bruger predict til at beregne outputtet for vores model
# lasso_fit er den allerede trænede Lasso-model, s = best_lambda vælger den lambda
# som giver den mindste cross validation (CV) fejl. 
tilskuere_pred <- predict(lasso_fit, s = best_lambda, newx = X_new)
# VVi konverterer resultatet til en numerisk værdi. 
tilskuere_pred <- as.numeric(tilskuere_pred)
# Vi ser resultatet
tilskuere_pred
# Vores estimat for kampdagen er 5129 tilskuere. Vi skal huske, at der kan være
# fejl i begge retninger. Vi bruger nu vores cross validation RMSE fra tidligere,
# til at finde det rigtige interval for tilskuereantallet for VFFs næste hjemmekamp.

# Vi vælger hvilken RMSE vi vil bruge. 
# Vi tager CV-RMSE fra cross-validation, som vi har lavet tidligere.
# Vi bruger filter til at isolere tidspunktet til "Flere måneder før"-modellen.
# Vi bruger efterfølgende pull-funktionen til at trække CV-RMSE ud. 
rmse_value <- cv_lasso_results %>%
  filter(Tidspunkt == "Flere måneder før") %>%
  pull(CV_RMSE)

# Vi beregner vores interval
lower <- tilskuere_pred - rmse_value
upper <- tilskuere_pred + rmse_value

# Vi printer resultatet
cat("Forventet tilskuertal for næste kamp:\n")
cat(round(tilskuere_pred), "±", round(rmse_value), "\n")
cat("Interval: ", round(lower), "til", round(upper), "\n")

# Vi kan se at vores interval ligger mellem 4663 til 7452. Dvs vi kan ud fra vores
# model konkludere, at for VFFs næste hjemmekamp imod Brøndby IF vil der være 
# et sted mellem 3808 til 6621 tilskuere. 

# 4. Feature sets for alle tidshorisonter ----- 
# vi definerer nu de forskellige features til de forskellige tidspunkter før kampdag
# flere måneder er en langsigtet prediction, hvorfor der er flere ukendte faktorer
# de 3 andre er mere kortsigtede, hvorfor der også er mere data til rådighed 
# jo nærmere vi er på kampstart

feature_sets <- list(
  "flere_måneder" = c(
    "ugedag", "sæson", "runde", "tidsperiode",
    "ferieperiode", "antal_transfers", "stadion_kapacitet"
  ),
  "10_dage" = c(
    "temp_dry", "wind_speed", "nedbør_seneste_7_timer",
    "ugedag", "placering_lag_hjemme", "placering_lag_ude",
    "form_seneste_3_hjemmehold", "form_seneste_3_udehold",
    "udehold_rang", "sæson", "runde", "tidsperiode",
    "ferieperiode", "stadion_kapacitet",
    "d10_tilskuere", "mål_seneste_3_hjemme"
  ),
  "7_dage" = c(
    "temp_dry", "wind_speed", "nedbør_seneste_7_timer",
    "ugedag", "placering_lag_hjemme", "placering_lag_ude",
    "form_seneste_3_hjemmehold", "form_seneste_3_udehold",
    "udehold_rang", "sæson", "runde", "tidsperiode",
    "ferieperiode", "stadion_kapacitet",
    "d7_tilskuere", "mål_seneste_3_hjemme"
  ),
  "3_dage" = c(
    "temp_dry", "wind_speed", "nedbør_seneste_7_timer",
    "ugedag", "placering_lag_hjemme", "placering_lag_ude",
    "form_seneste_3_hjemmehold", "form_seneste_3_udehold",
    "udehold_rang", "sæson", "runde", "tidsperiode",
    "ferieperiode", "stadion_kapacitet",
    "d3_tilskuere", "mål_seneste_3_hjemme"
  )
)


# 5. Træn Lasso modeller for alle tidshorisonter -----

train_lasso_model <- function(train_data, features, lambda_grid) {
  
  X_train <- model.matrix(
    as.formula(paste("~", paste(features, collapse = "+"))),
    train_data
  )[,-1, drop = FALSE]
  
  y_train <- train_data$tilskuere
  
  cv_fit <- cv.glmnet(
    x = X_train,
    y = y_train,
    alpha = 1,
    lambda = lambda_grid,
    standardize = TRUE
  )
  
  best_lambda <- cv_fit$lambda.min
  
  lasso_fit <- glmnet(
    x = X_train,
    y = y_train,
    alpha = 1,
    lambda = best_lambda,
    standardize = TRUE
  )
  
  list(
    model = lasso_fit,
    lambda = best_lambda,
    features = features,
    x_cols = colnames(X_train),
    factor_levels = train_data %>%
      select(where(is.factor)) %>%
      map(levels)
  )
}

# Lav ny kamp

lav_ny_kamp <- function(data, kamp_id, tidspunkt, feature_sets, lasso_models) {
  features <- feature_sets[[tidspunkt]]
  
  # Tag rækken for den kamp, vi vil evaluere
  ny <- data[kamp_id, features, drop = FALSE]  # brug korrekt kamp, ikke data[1, ]
  
  # Behold numeriske features som de er (ingen nulstilling)
  return(ny)
}

# 3. Predict tilskuere ----

predict_tilskuere <- function(tidspunkt, ny_kamp, lasso_models) {
  
  lasso_model <- lasso_models[[tidspunkt]]
  
  X_new_raw <- model.matrix(
    as.formula(paste("~", paste(lasso_model$features, collapse = "+"))), 
    ny_kamp
  )[, -1, drop = FALSE]
  
  # Juster kolonner så de matcher træningsdata
  X_new <- matrix(0, nrow = 1, ncol = length(lasso_model$x_cols))
  colnames(X_new) <- lasso_model$x_cols
  common_cols <- intersect(colnames(X_new_raw), lasso_model$x_cols)
  
  if(length(common_cols) > 0){
    X_new[, common_cols] <- X_new_raw[, common_cols]
  }
  
  pred <- predict(lasso_model$model, s = lasso_model$lambda, newx = X_new)
  as.numeric(pred)
}



# vi definerer en lambda grid som kontrollerer hvor kraftigt vi straffer store koeffecienter
# for lasso, som bruges til at undgå overfitting

lambda_grid <- 10^seq(3, -3, length = 100)

# vi træner en lasso model pr tidshorisont, med de features som passer til den.
# lapply sikrer, at vi laver alle modeller uden at gentage kode

lasso_models <- lapply(names(feature_sets), function(tidspunkt) {
  train_lasso_model(train_data = train_data, features = feature_sets[[tidspunkt]], lambda_grid = lambda_grid)
})

# Vi navngiver modellerne med tidshorisonterne så vi kan bruge den igen senere
names(lasso_models) <- names(feature_sets)


# 6. Funktion til at evaluere én kamp på alle tidshorisonter -----
# denne funktion tager kamp_id og returnerer predictions for alle tidshorisonterne
# for hver tidshorisont oprettes: lav_ny_kamp (opretter en dataframe med features)
#                                 Predict_tilskuere(laver prediction med korrekte lasso model)
#                                 Resultat gemmes i tibble

evaluer_kamp <- function(kamp_id, data, lasso_models, feature_sets) {
  map_dfr(names(feature_sets), function(tidspunkt) {
    ny_kamp <- lav_ny_kamp(data, kamp_id, tidspunkt, feature_sets, lasso_models)
    pred <- predict_tilskuere(tidspunkt, ny_kamp, lasso_models)
    tibble(
      Kamp = kamp_id,
      Tidspunkt = tidspunkt,
      Prediktion = pred
    )
  })
}



# 7. Evaluering af kamp -----
# vi kan nu teste vores modeller med forskellige tidshorisonter ved at indtaste kamp id 
# fra en af kampene i datasættet, indtast blot et tal mellem 1-204.og se hvordan de varierer
# på de forskellige tidspunkter før kampstart

kamp_id <- 99
eval_data <- evaluer_kamp(kamp_id, train_data, lasso_models, feature_sets)

print(eval_data)


# 8. Plot prediktioner vs. faktisk tilskuertal -----
# vi visualiserer sammenligningen af modellens prediktioner med det faktiske tilskuertal

faktisk <- train_data$tilskuere[kamp_id]

ggplot(eval_data, aes(x = Tidspunkt, y = Prediktion)) +
  geom_bar(stat = "identity", fill = "steelblue") +    # Prediktioner som blå søjler
  geom_hline(yintercept = faktisk, color = "red", linetype = "dashed") + # Faktisk tilskuertal rød streg
  labs(title = paste("Prediction for kamp", kamp_id),
       y = "Prediktion af tilskuere",
       x = "Tid før kamp") +
  theme_minimal()



