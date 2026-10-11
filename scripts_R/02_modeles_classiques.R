# ============================================================
# GLD / SLV — Modèles classiques en R
# Utilise les mêmes données d’apprentissage et de test que Python.
# ============================================================

# 1. Bibliothèques --------------------------------------------

packages <- c("forecast", "vars", "urca", "tseries", "FinTS", "rugarch")

manquants <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(manquants) > 0) {
  install.packages(
    manquants,
    repos = "https://cloud.r-project.org"
  )
}

invisible(lapply(packages, library, character.only = TRUE))
set.seed(42)

# 2. Dossiers ------------------------------------------------

racine <- paste0(
  "C:/Users/nourc/OneDrive/Bureau/ING3/",
  "serie temp/projet_or_argent"
)

if (!dir.exists(racine)) {
  stop("Dossier du projet introuvable : vérifie la variable racine.")
}

dossier_donnees <- file.path(racine, "data", "processed")
dossier_resultats <- file.path(dossier_donnees, "r")
dossier_figures <- file.path(racine, "figures", "r")
dossier_modeles <- file.path(racine, "models", "r")
dossier_rapport <- file.path(racine, "reports", "r")

for (dossier in c(
  dossier_resultats, dossier_figures,
  dossier_modeles, dossier_rapport
)) {
  dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
}

sauver_table <- function(tableau, nom) {
  write.csv(
    tableau,
    file.path(dossier_resultats, nom),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

# 3. Données -------------------------------------------------

lire_prix <- function(nom) {
  fichier <- file.path(dossier_donnees, nom)
  
  if (!file.exists(fichier)) {
    stop("Fichier absent : ", fichier)
  }
  
  tableau <- read.csv(fichier, check.names = FALSE)
  
  stopifnot(all(c("Date", "GLD", "SLV") %in% names(tableau)))
  
  tableau <- tableau[, c("Date", "GLD", "SLV")]
  tableau$Date <- as.Date(tableau$Date)
  valeurs <- as.matrix(tableau[, c("GLD", "SLV")])
  
  stopifnot(
    !anyNA(tableau),
    !anyDuplicated(tableau$Date),
    !is.unsorted(tableau$Date),
    all(is.finite(valeurs)),
    all(valeurs > 0)
  )
  
  tableau
}

prix_train <- lire_prix("prix_train.csv")
prix_test <- lire_prix("prix_test.csv")

stopifnot(max(prix_train$Date) < min(prix_test$Date))

actifs <- c("GLD", "SLV")
log_prix_train <- log(as.matrix(prix_train[, actifs]))

# Calculer les rendements logarithmiques en pourcentage.
rendements_train <- 100 * apply(log_prix_train, 2, diff)
colnames(rendements_train) <- actifs
dates_rendements <- prix_train$Date[-1]

cat("Apprentissage :", nrow(prix_train), "séances\n")
cat("Test :", nrow(prix_test), "séances\n")

sauver_table(
  data.frame(Date = dates_rendements, rendements_train),
  "rendements_train_R.csv"
)

# 4. Stationnarité -------------------------------------------

# ADF de tseries inclut une tendance ; KPSS teste ici la
# stationnarité autour d’une constante. Les retards diffèrent
# de Python : les valeurs numériques peuvent donc différer.
tests_stationnarite <- list()

for (actif in actifs) {
  series <- list(
    Log_prix = log_prix_train[, actif],
    Rendement = rendements_train[, actif]
  )
  
  for (nom in names(series)) {
    adf <- tseries::adf.test(series[[nom]])
    kpss <- tseries::kpss.test(series[[nom]], null = "Level")
    
    tests_stationnarite[[length(tests_stationnarite) + 1]] <-
      data.frame(
        Actif = actif,
        Serie = nom,
        ADF_statistique = unname(adf$statistic),
        ADF_p_value = adf$p.value,
        KPSS_statistique = unname(kpss$statistic),
        KPSS_p_value = kpss$p.value
      )
  }
}

table_stationnarite <- do.call(rbind, tests_stationnarite)
sauver_table(table_stationnarite, "stationnarite_R.csv")
print(table_stationnarite)

# 5. Fonction de diagnostic ----------------------------------

diagnostic_residus <- function(e, actif, modele, fitdf = 0) {
  e <- as.numeric(e)
  e <- e[is.finite(e)]
  
  data.frame(
    Actif = actif,
    Modele = modele,
    LB_p_10 = Box.test(
      e, lag = 10, type = "Ljung-Box", fitdf = fitdf
    )$p.value,
    LB_p_20 = Box.test(
      e, lag = 20, type = "Ljung-Box", fitdf = fitdf
    )$p.value,
    JB_p = tseries::jarque.bera.test(e)$p.value,
    ARCH_p_10 = FinTS::ArchTest(
      e, lags = 10, demean = TRUE
    )$p.value
  )
}

# 6. ARIMA ---------------------------------------------------

ordres <- list(
  c(0, 1, 0), c(1, 1, 0),
  c(0, 1, 1), c(1, 1, 1)
)

modeles_arima <- list()
modeles_auto <- list()
comparaison_arima <- list()
diagnostics_arima <- list()
selection_auto <- list()

for (actif in actifs) {
  serie <- ts(log_prix_train[, actif], frequency = 1)
  
  # Comparer de petits ordres, avec et sans dérive.
  for (ordre in ordres) {
    for (derive in c(FALSE, TRUE)) {
      modele <- forecast::Arima(
        serie,
        order = ordre,
        include.drift = derive,
        method = "ML"
      )
      
      comparaison_arima[[length(comparaison_arima) + 1]] <-
        data.frame(
          Actif = actif,
          Ordre = paste(ordre, collapse = ","),
          Derive = derive,
          AIC = AIC(modele),
          BIC = BIC(modele),
          Convergence = modele$code == 0
        )
    }
  }
  
  # Garder la même référence principale que dans Python.
  modeles_arima[[actif]] <- forecast::Arima(
    serie,
    order = c(0, 1, 0),
    include.drift = FALSE,
    method = "ML"
  )
  
  modeles_auto[[actif]] <- forecast::auto.arima(
    serie,
    d = 1,
    max.p = 3,
    max.q = 3,
    seasonal = FALSE,
    allowdrift = TRUE,
    allowmean = FALSE,
    ic = "aic",
    stepwise = TRUE,
    approximation = FALSE
  )
  
  ordre_auto <- forecast::arimaorder(modeles_auto[[actif]])
  
  selection_auto[[actif]] <- data.frame(
    Actif = actif,
    Ordre = paste(ordre_auto[1:3], collapse = ","),
    Derive = "drift" %in% names(coef(modeles_auto[[actif]])),
    AIC = AIC(modeles_auto[[actif]]),
    BIC = BIC(modeles_auto[[actif]])
  )
  
  # Exclure le premier résidu d’initialisation.
  e <- residuals(modeles_arima[[actif]])[-1]
  
  diagnostics_arima[[actif]] <- diagnostic_residus(
    e, actif, "ARIMA(0,1,0) sans derive"
  )
  
  png(
    file.path(dossier_figures, paste0("01_residus_ARIMA_", actif, ".png")),
    width = 1200, height = 800, res = 130
  )
  par(mfrow = c(2, 2))
  plot(e, type = "l", main = paste(actif, "— résidus"), ylab = "")
  acf(e, main = "ACF des résidus")
  acf(e^2, main = "ACF des résidus au carré")
  qqnorm(e)
  qqline(e, col = "red")
  dev.off()
}

sauver_table(
  do.call(rbind, comparaison_arima),
  "comparaison_arima_R.csv"
)
sauver_table(
  do.call(rbind, selection_auto),
  "selection_auto_arima_R.csv"
)
sauver_table(
  do.call(rbind, diagnostics_arima),
  "diagnostics_arima_R.csv"
)

saveRDS(
  list(retenus = modeles_arima, automatiques = modeles_auto),
  file.path(dossier_modeles, "arima_R.rds")
)

# 7. VAR : inclure explicitement l’ordre zéro -----------------

retard_max <- 10L

selection_var <- vars::VARselect(
  rendements_train, lag.max = retard_max, type = "const"
)

# Calculer les critères du VAR(0) sur le même échantillon
# que celui utilisé par VARselect pour les ordres 1 à 10.
y <- rendements_train[
  (retard_max + 1):nrow(rendements_train), ,
  drop = FALSE
]

e0 <- sweep(y, 2, colMeans(y), "-")
T_commun <- nrow(y)
K <- ncol(y)
sigma0 <- crossprod(e0) / T_commun
log_det <- as.numeric(
  determinant(sigma0, logarithm = TRUE)$modulus
)

criteres0 <- c(
  "AIC(n)" = log_det + 2 * K / T_commun,
  "HQ(n)" = log_det + 2 * log(log(T_commun)) * K / T_commun,
  "SC(n)" = log_det + log(T_commun) * K / T_commun,
  "FPE(n)" = ((T_commun + 1) / (T_commun - 1))^K * det(sigma0)
)

criteres_var <- cbind(criteres0, selection_var$criteria)
colnames(criteres_var) <- as.character(0:retard_max)

table_selection_var <- data.frame(
  Retard = 0:retard_max,
  t(criteres_var),
  check.names = FALSE
)

sauver_table(table_selection_var, "selection_var_R.csv")

ordres_selectionnes <- apply(
  criteres_var, 1,
  function(x) as.integer(names(which.min(x)))
)

cat("\nOrdres VAR sélectionnés :\n")
print(ordres_selectionnes)

# Estimer les références comparables à Python.
moyenne_var0 <- colMeans(rendements_train)
residus_var0 <- sweep(rendements_train, 2, moyenne_var0, "-")
var1 <- vars::VAR(rendements_train, p = 1, type = "const")

diagnostics_var <- list()

for (actif in actifs) {
  diagnostics_var[[length(diagnostics_var) + 1]] <-
    diagnostic_residus(residus_var0[, actif], actif, "VAR(0)")
  
  diagnostics_var[[length(diagnostics_var) + 1]] <-
    diagnostic_residus(
      residuals(var1)[, actif], actif, "VAR(1)", fitdf = 2
    )
}

sauver_table(
  do.call(rbind, diagnostics_var),
  "diagnostics_equations_var_R.csv"
)

capture.output(
  vars::serial.test(var1, lags.pt = 20, type = "PT.adjusted"),
  vars::normality.test(var1, multivariate.only = TRUE),
  vars::arch.test(var1, lags.multi = 5, multivariate.only = TRUE),
  file = file.path(dossier_rapport, "diagnostics_multivaries_VAR1.txt")
)

granger <- do.call(rbind, lapply(actifs, function(source) {
  test <- vars::causality(var1, cause = source)$Granger
  
  data.frame(
    Source = source,
    Cible = setdiff(actifs, source),
    Statistique = unname(test$statistic),
    p_value = test$p.value
  )
}))

sauver_table(granger, "granger_VAR1_R.csv")

saveRDS(
  list(moyenne_var0 = moyenne_var0, var1 = var1),
  file.path(dossier_modeles, "var_R.rds")
)

# 8. Cointégration : Johansen --------------------------------

# Contrôle complémentaire à Engle–Granger en Python.
# Documenter une éventuelle différence de conclusion.
selection_niveaux <- vars::VARselect(
  log_prix_train, lag.max = 10, type = "const"
)

K_johansen <- max(
  2L, as.integer(selection_niveaux$selection["SC(n)"])
)

johansen <- urca::ca.jo(
  log_prix_train,
  type = "trace",
  ecdet = "const",
  K = K_johansen,
  spec = "transitory"
)

table_johansen <- data.frame(
  Hypothese = rownames(johansen@cval),
  Statistique = as.numeric(johansen@teststat),
  Critique_5_pct = johansen@cval[, "5pct"],
  Retards_VAR_niveaux = K_johansen
)

sauver_table(table_johansen, "johansen_R.csv")

capture.output(
  summary(johansen),
  file = file.path(dossier_rapport, "johansen_R.txt")
)

rejet_r0 <- tail(johansen@teststat, 1) >
  tail(johansen@cval[, "5pct"], 1)

rejet_r1 <- johansen@teststat[1] >
  johansen@cval[1, "5pct"]

rang <- if (!rejet_r0) 0L else if (!rejet_r1) 1L else 2L
vecm <- NULL

cat("\nRang de Johansen à 5 % :", rang, "\n")

if (rang == 1L) {
  vecm <- urca::cajorls(johansen, r = 1)
  
  capture.output(
    vecm$beta,
    summary(vecm$rlm),
    file = file.path(dossier_rapport, "VECM_R.txt")
  )
  
  cat("VECM estimé : examiner les coefficients avant interprétation.\n")
} else if (rang == 0L) {
  cat("Absence de cointégration non rejetée : pas de VECM retenu.\n")
} else {
  cat("Rang plein : vérifier les tests et la spécification.\n")
}

saveRDS(
  list(johansen = johansen, rang = rang, vecm = vecm),
  file.path(dossier_modeles, "cointegration_R.rds")
)

# 9. GARCH(1,1) Student --------------------------------------

specification <- rugarch::ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = FALSE
  ),
  distribution.model = "std"
)

modeles_garch <- list()
parametres_garch <- list()
diagnostics_garch <- list()
volatilites <- data.frame(Date = dates_rendements)

for (actif in actifs) {
  modele <- rugarch::ugarchfit(
    spec = specification,
    data = rendements_train[, actif],
    solver = "hybrid"
  )
  
  if (modele@fit$convergence != 0) {
    stop("Échec de convergence GARCH pour ", actif)
  }
  
  modeles_garch[[actif]] <- modele
  p <- coef(modele)
  
  parametres_garch[[actif]] <- data.frame(
    Actif = actif,
    Omega = unname(p["omega"]),
    Alpha = unname(p["alpha1"]),
    Beta = unname(p["beta1"]),
    Persistance = unname(p["alpha1"] + p["beta1"]),
    Nu_Student = unname(p["shape"])
  )
  
  z <- as.numeric(residuals(modele, standardize = TRUE))
  z <- z[is.finite(z)]
  
  diagnostics_garch[[actif]] <- data.frame(
    Actif = actif,
    LB_z_p_10 = Box.test(z, lag = 10, type = "Ljung-Box")$p.value,
    LB_z_p_20 = Box.test(z, lag = 20, type = "Ljung-Box")$p.value,
    LB_z2_p_10 = Box.test(z^2, lag = 10, type = "Ljung-Box")$p.value,
    LB_z2_p_20 = Box.test(z^2, lag = 20, type = "Ljung-Box")$p.value,
    ARCH_p_10 = FinTS::ArchTest(
      z, lags = 10, demean = TRUE
    )$p.value
  )
  
  volatilites[[actif]] <- as.numeric(sigma(modele))
  
  capture.output(
    show(modele),
    file = file.path(dossier_rapport, paste0("GARCH_", actif, "_R.txt"))
  )
}

sauver_table(
  do.call(rbind, parametres_garch),
  "parametres_garch_R.csv"
)
sauver_table(
  do.call(rbind, diagnostics_garch),
  "diagnostics_garch_R.csv"
)
sauver_table(volatilites, "volatilites_garch_train_R.csv")

saveRDS(modeles_garch, file.path(dossier_modeles, "garch_R.rds"))

png(
  file.path(dossier_figures, "02_volatilites_garch_R.png"),
  width = 1300, height = 650, res = 130
)
matplot(
  volatilites$Date,
  as.matrix(volatilites[, actifs]),
  type = "l", lty = 1,
  col = c("darkgoldenrod", "slategray"),
  xlab = "Date",
  ylab = "Écart-type conditionnel (%)",
  main = "GARCH(1,1) Student"
)
legend(
  "topright", legend = actifs,
  col = c("darkgoldenrod", "slategray"), lty = 1
)
dev.off()

# 10. Prévisions initiales -----------------------------------

h_max <- 10L
stopifnot(nrow(prix_test) >= h_max)

intervalles_arima <- list()
previsions_volatilite <- list()

for (actif in actifs) {
  prevision <- forecast::forecast(
    modeles_arima[[actif]], h = h_max, level = 95
  )
  
  # Exponentier les bornes des log-prix.
  # Ces intervalles gaussiens restent limités par les diagnostics.
  intervalles_arima[[actif]] <- data.frame(
    Date = prix_test$Date[1:h_max],
    Actif = actif,
    Horizon = 1:h_max,
    Mediane_prix = exp(as.numeric(prevision$mean)),
    Borne_basse_95 = exp(as.numeric(prevision$lower[, 1])),
    Borne_haute_95 = exp(as.numeric(prevision$upper[, 1])),
    Observe = prix_test[[actif]][1:h_max]
  )
  
  prevision_garch <- rugarch::ugarchforecast(
    modeles_garch[[actif]], n.ahead = h_max
  )
  
  previsions_volatilite[[actif]] <- data.frame(
    Date = prix_test$Date[1:h_max],
    Actif = actif,
    Horizon = 1:h_max,
    Volatilite_prevue_pct = as.numeric(sigma(prevision_garch))
  )
}

sauver_table(
  do.call(rbind, intervalles_arima),
  "intervalles_arima_initiaux_R.csv"
)
sauver_table(
  do.call(rbind, previsions_volatilite),
  "previsions_volatilite_garch_R.csv"
)

# 11. Évaluation glissante : horizons exacts 5 et 10 -----------

prix_complets <- rbind(prix_train, prix_test)
log_complets <- log(as.matrix(prix_complets[, actifs]))
rendements_complets <- 100 * apply(log_complets, 2, diff)

horizons <- c(5L, 10L)

origines <- seq.int(
  nrow(prix_train),
  nrow(prix_complets) - max(horizons)
)

# Conserver les coefficients du VAR(1), puis actualiser seulement
# son état initial avec les rendements disponibles à chaque origine.
A1 <- vars::Acoef(var1)[[1]]

constante <- vapply(
  var1$varresult,
  function(equation) unname(coef(equation)["const"]),
  numeric(1)
)

predictions <- list()

for (o in origines) {
  dernier_prix <- as.numeric(
    unlist(prix_complets[o, actifs], use.names = FALSE)
  )
  
  etat <- as.numeric(rendements_complets[o - 1, ])
  
  rendements_predits <- matrix(
    NA_real_, nrow = max(horizons), ncol = length(actifs)
  )
  
  for (pas in seq_len(max(horizons))) {
    etat <- as.numeric(constante + A1 %*% etat)
    rendements_predits[pas, ] <- etat
  }
  
  for (h in horizons) {
    previsions <- list(
      "Prix inchange" = dernier_prix,
      "ARIMA(0,1,0) sans derive" = dernier_prix,
      "VAR(0)" = dernier_prix * exp(h * moyenne_var0 / 100),
      "VAR(1) complementaire" = dernier_prix * exp(
        colSums(rendements_predits[1:h, , drop = FALSE]) / 100
      )
    )
    
    reels <- as.numeric(
      unlist(prix_complets[o + h, actifs], use.names = FALSE)
    )
    
    for (nom in names(previsions)) {
      predictions[[length(predictions) + 1]] <- data.frame(
        Date_origine = prix_complets$Date[o],
        Date_cible = prix_complets$Date[o + h],
        Horizon = h,
        Actif = actifs,
        Modele = nom,
        Reel = reels,
        Prevision = as.numeric(previsions[[nom]])
      )
    }
  }
}

table_predictions <- do.call(rbind, predictions)
table_predictions$Erreur <-
  table_predictions$Reel - table_predictions$Prevision

sauver_table(
  table_predictions,
  "predictions_test_glissant_classiques_R.csv"
)

groupes <- split(
  table_predictions,
  interaction(
    table_predictions$Horizon,
    table_predictions$Actif,
    table_predictions$Modele,
    drop = TRUE
  )
)

table_metriques <- do.call(rbind, lapply(groupes, function(groupe) {
  e <- groupe$Erreur
  
  data.frame(
    Horizon = groupe$Horizon[1],
    Actif = groupe$Actif[1],
    Modele = groupe$Modele[1],
    N = nrow(groupe),
    MSE = mean(e^2),
    RMSE = sqrt(mean(e^2)),
    MAE = mean(abs(e)),
    MAPE_pct = 100 * mean(abs(e / groupe$Reel))
  )
}))

rownames(table_metriques) <- NULL

table_metriques <- table_metriques[
  order(
    table_metriques$Horizon,
    table_metriques$Actif,
    table_metriques$Modele
  ),
]

sauver_table(
  table_metriques,
  "metriques_test_glissant_classiques_R.csv"
)

# 12. Versions et résumé -------------------------------------

capture.output(
  sessionInfo(),
  file = file.path(dossier_rapport, "sessionInfo_R.txt")
)

cat("\nNombre d’origines communes :", length(origines), "\n")
print(table_metriques, row.names = FALSE)

cat("\nScript terminé.\n")
cat("Résultats :", dossier_resultats, "\n")
cat("Figures :", dossier_figures, "\n")
cat("Modèles :", dossier_modeles, "\n")
cat("Diagnostics textuels :", dossier_rapport, "\n")