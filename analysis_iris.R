# =============================================================================
# PROJET R1 - ANALYSE DU JEU DE DONNÉES IRIS
# =============================================================================
# Objectif : Exploration, statistiques descriptives, visualisations et synthèse
# pour comprendre la structure des données et les tendances par espèce.
#
# Dépendances : install.packages(c("dplyr", "ggplot2", "tidyr"))
# =============================================================================

# --- Configuration ---
# Créer le dossier des figures (adapter le chemin si besoin)
FIG_PATH <- "output/figures"

if (!dir.exists(FIG_PATH)) dir.create(FIG_PATH, recursive = TRUE)


library(dplyr)
library(ggplot2)
library(tidyr)
# Chargement des données Iris (package datasets, chargé par défaut)
data(iris)

# Palette cohérente pour les espèces
palette_especes <- c("setosa" = "#2E86AB", "versicolor" = "#A23B72", "virginica" = "#F18F01")
theme_set(theme_minimal(base_size = 11))


# =============================================================================
# ÉTAPE 1 : EXPLORATION ET PRÉPARATION DES DONNÉES
# Objectif : Comprendre la structure et identifier les premières tendances
# =============================================================================

cat("\n")
cat("========== ÉTAPE 1 - EXPLORATION ET PRÉPARATION ==========\n\n")

# --- 1.1 Charger et observer le dataset IRIS ---
cat("--- Dimensions du jeu de données ---\n")
cat("Nombre de lignes :", nrow(iris), "\n")
cat("Nombre de colonnes :", ncol(iris), "\n")
cat("Colonnes :", paste(names(iris), collapse = ", "), "\n\n")

cat("--- Aperçu des premières lignes ---\n")
print(head(iris))

# --- 1.2 Analyser les types de variables et leurs distributions ---
cat("\n--- Structure et types des variables ---\n")
str(iris)
# 4 variables numériques (mesures en cm) : Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
# 1 variable facteur : Species (setosa, versicolor, virginica)

cat("\n--- Résumé des distributions (toutes variables) ---\n")
print(summary(iris))

# Répartition par espèce
cat("\n--- Répartition des espèces ---\n")
print(table(iris$Species))
cat("-> Équilibre parfait : 50 observations par espèce.\n")

# --- 1.3 Détecter les valeurs manquantes ---
cat("\n--- Valeurs manquantes par colonne ---\n")
na_par_col <- colSums(is.na(iris))
print(na_par_col)
if (sum(na_par_col) == 0) {
  cat("-> Aucune valeur manquante : jeu de données complet, prêt pour l'analyse.\n")
} else {
  cat("-> Présence de NA à traiter (imputation ou suppression).\n")
}

# Préparation : format long pour les visualisations multi-variables
iris_long <- iris %>%
  pivot_longer(cols = Sepal.Length:Petal.Width, names_to = "Variable", values_to = "Valeur_cm")


# =============================================================================
# ÉTAPE 2 : STATISTIQUES DESCRIPTIVES ET ANALYSE DES RELATIONS
# Objectif : Tendance centrale, dispersion, corrélations et influence sur l'espèce
# =============================================================================

cat("\n")
cat("========== ÉTAPE 2 - STATISTIQUES ET CORRÉLATIONS ==========\n\n")

# --- 2.1 Tendance centrale et dispersion (par variable et par espèce) ---
cat("--- Tendance centrale et dispersion (global) ---\n")
stats_global <- iris %>%
  summarise(
    Variable = "Toutes",
    N = n(),
    Moyenne_Sepal.L = mean(Sepal.Length), SD_Sepal.L = sd(Sepal.Length),
    Moyenne_Petal.L = mean(Petal.Length), SD_Petal.L = sd(Petal.Length),
    Min_Petal.L = min(Petal.Length), Max_Petal.L = max(Petal.Length)
  )
print(stats_global)

cat("\n--- Tendance centrale et dispersion par espèce ---\n")
stats_espece <- iris %>%
  group_by(Species) %>%
  summarise(
    N = n(),
    Moy_Sepal.L = round(mean(Sepal.Length), 2), 
    SD_Sepal.L = round(sd(Sepal.Length), 2),
    Max_Sepal.L = round(max(Sepal.Length), 2),
    Min_Sepal.L = round(min(Sepal.Length), 2),
    Moy_Petal.L = round(mean(Petal.Length), 2), 
    SD_Petal.L = round(sd(Petal.Length), 2),
    Max_Petal.L = round(max(Petal.Length), 2),
    Min_Petal.L = round(min(Petal.Length), 2),
    Moy_Petal.W = round(mean(Petal.Width), 2),
    
    .groups = "drop"
  )
print(stats_espece)

# --- 2.2 Analyse de la corrélation entre les variables ---
cat("\n--- Matrice de corrélation (variables numériques) ---\n")
mat_cor <- cor(iris[, 1:4])
print(round(mat_cor, 3))

# Interprétation attendue :
# -> Quelles variables sont fortement corrélées ?
# -> Comment ces mesures influencent-elles l'espèce ?
cat("\n--- Interprétation des corrélations ---\n")
cat("- Petal.Length et Petal.Width : corrélation très forte (~0.96).\n")
cat("- Sepal.Length et Petal.Length / Petal.Width : corrélations positives fortes.\n")
cat("- Ces mesures permettent de séparer les espèces (setosa a des pétales plus petits).\n")

# --- 2.3 Représentation visuelle : heatmap de corrélation ---
mat_cor_df <- as.data.frame(mat_cor)
mat_cor_df$Var1 <- rownames(mat_cor_df)
mat_cor_long <- mat_cor_df %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation")

p_heatmap <- ggplot(mat_cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(Correlation, 2)), color = "white", size = 4, fontface = "bold") +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0) +
  labs(
    title = "Heatmap des corrélations entre variables numériques",
    subtitle = "Petal.Length et Petal.Width sont très fortement corrélés",
    x = NULL, y = NULL
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), panel.grid = element_blank(),
        plot.title = element_text(face = "bold")) +
  coord_fixed()

print(p_heatmap)
ggsave(file.path(FIG_PATH, "01_heatmap_correlation.png"), p_heatmap, width = 6, height = 5, dpi = 150)
cat("\n-> Graphique sauvegardé :", file.path(FIG_PATH, "01_heatmap_correlation.png"), "\n")


# =============================================================================
# ÉTAPE 3 : VISUALISATION ET IDENTIFICATION DES TENDANCES
# Objectif : Histogrammes, boxplots, scatter plots et interprétations
# =============================================================================

cat("\n")
cat("========== ÉTAPE 3 - VISUALISATIONS ==========\n\n")

# --- 3.1 Histogrammes : répartition des valeurs ---
# Par variable, avec densité par espèce (ex. Petal.Length)
p_hist_petal <- ggplot(iris, aes(x = Petal.Length, fill = Species)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25, alpha = 0.7, position = "identity") +
  geom_density(alpha = 0.4, linewidth = 0.8) +
  scale_fill_manual(values = palette_especes) +
  labs(
    title = "Répartition de la longueur du pétale",
    subtitle = "Distribution par espèce : setosa bien séparée",
    x = "Longueur du pétale (cm)", y = "Densité"
  ) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

# Histogrammes pour les 4 variables (facettes)
p_hist_all <- ggplot(iris_long, aes(x = Valeur_cm, fill = Species)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, alpha = 0.7, position = "identity") +
  facet_wrap(~ Variable, scales = "free", ncol = 2) +
  scale_fill_manual(values = palette_especes) +
  labs(title = "Répartition des variables par espèce", x = "Valeur (cm)", y = "Densité") +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

print(p_hist_petal)
print(p_hist_all)
ggsave(file.path(FIG_PATH, "02_histogramme_petal_length.png"), p_hist_petal, width = 7, height = 4, dpi = 150)
ggsave(file.path(FIG_PATH, "03_histogrammes_toutes_variables.png"), p_hist_all, width = 8, height = 6, dpi = 150)
cat("-> Sauvegardé : 02_histogramme_petal_length.png, 03_histogrammes_toutes_variables.png\n")

# --- 3.2 Boxplots : détection des outliers et différences entre espèces ---
p_box <- ggplot(iris_long, aes(x = Species, y = Valeur_cm, fill = Species)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.8) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = palette_especes) +
  labs(
    title = "Boxplots par espèce - Détection des outliers",
    subtitle = "Différences significatives entre espèces ; quelques points atypiques visibles",
    x = "Espèce", y = "Valeur (cm)"
  ) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "none",
        plot.title = element_text(face = "bold"))

print(p_box)
ggsave(file.path(FIG_PATH, "04_boxplots_par_espece.png"), p_box, width = 8, height = 6, dpi = 150)

# Analyse attendue : différences significatives ? outliers ?
cat("\n--- Analyse boxplots ---\n")
cat("- Setosa : pétales nettement plus petits (Petal.Length, Petal.Width).\n")
cat("- Virginica : sépales et pétales en moyenne plus grands.\n")
cat("- Outliers : quelques points isolés (ex. Sepal.Width) sans remettre en cause les tendances.\n")

# --- 3.3 Scatter plots : relations entre variables et séparation des espèces ---
# Scatter principal : Sepal.Length vs Petal.Length (séparation claire)
p_scatter1 <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species, shape = Species)) +
  geom_point(size = 3, alpha = 0.85) +
  scale_color_manual(values = palette_especes) +
  labs(
    title = "Sépale vs Pétale (longueurs) - Séparation des espèces",
    subtitle = "Les trois espèces sont bien séparées ; patterns exploitables pour la classification",
    x = "Longueur du sépale (cm)", y = "Longueur du pétale (cm)"
  ) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

# Scatter Sepal.Width vs Petal.Width
p_scatter2 <- ggplot(iris, aes(x = Sepal.Width, y = Petal.Width, color = Species, shape = Species)) +
  geom_point(size = 3, alpha = 0.85) +
  scale_color_manual(values = palette_especes) +
  labs(
    title = "Largeurs Sépale vs Pétale",
    x = "Largeur du sépale (cm)", y = "Largeur du pétale (cm)"
  ) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

print(p_scatter1)
print(p_scatter2)
ggsave(file.path(FIG_PATH, "05_scatter_sepal_petal_length.png"), p_scatter1, width = 7, height = 5, dpi = 150)
ggsave(file.path(FIG_PATH, "06_scatter_sepal_petal_width.png"), p_scatter2, width = 7, height = 5, dpi = 150)

# Réponses à la problématique
cat("\n--- Réponses Scatter Plot ---\n")
cat("- Les espèces sont-elles bien séparées ? Oui, surtout setosa vs les deux autres.\n")
cat("- Patterns pour la classification : Petal.Length et Petal.Width séparent bien setosa ;\n")
cat("  Sepal.Length + Petal.Length permettent de distinguer versicolor et virginica.\n")


# =============================================================================
# ÉTAPE 4 : SYNTHÈSE ET PRINCIPAUX INSIGHTS (pour rapport et présentation)
# =============================================================================

cat("\n")
cat("========== ÉTAPE 4 - SYNTHÈSE ==========\n\n")

insights <- list(
  structure = "150 observations, 4 variables numériques (cm), 1 facteur (Species). Pas de NA.",
  especes = "50 fleurs par espèce (setosa, versicolor, virginica).",
  correlation = "Petal.Length et Petal.Width très corrélés ; corrélations fortes avec Sepal.Length.",
  tendance_centrale = "Setosa : pétales plus petits ; Virginica : mesures en moyenne plus grandes.",
  outliers = "Quelques outliers sur Sepal.Width sans impact majeur.",
  separation = "Setosa bien séparée ; versicolor et virginica partiellement chevauchantes sur certaines variables.",
  classification = "Petal.Length et Petal.Width sont les variables les plus discriminantes pour la classification."
)

for (i in seq_along(insights)) {
  cat(names(insights)[i], ":", insights[[i]], "\n")
}

cat("\n-> Tous les graphiques ont été enregistrés dans", FIG_PATH, "\n")
cat("-> Rapport détaillé : voir doc/rapport_iris.Rmd (knit vers PDF).\n")
cat("-> Structure présentation : doc/structure_presentation_slides.md\n")


# =============================================================================
# ---------------Arbres de décisions (Algo "Transparent")----------------------
# =============================================================================



# =============================================================================
# ÉTAPE 1 : Charger le dataset Iris
# =============================================================================

data(iris)

head(iris)
summary(iris)


# =============================================================================
# ÉTAPE 2 : Calcul manuel de l’indice de Gini
# =============================================================================

gini <- function(y) {
  proportions <- table(y) / length(y)
  1 - sum(proportions^2)
}

gini(iris$Species)

# =============================================================================
# ÉTAPE 3 : Construire l’arbre de décision
# =============================================================================


install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

model <- rpart(
  Species ~ .,
  data = iris,
  method = "class",
  parms = list(split = "gini")
)

summary(model)


# =============================================================================
# ÉTAPE 4 : Visualisation de l’arbre
# =============================================================================


rpart.plot(
  model,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Arbre de Décision - Iris Dataset"
)


# =============================================================================
# ÉTAPE 5 : Frontière de décision (2 variables)
# =============================================================================


library(ggplot2)

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) +
  geom_point(size = 3) +
  theme_minimal() +
  ggtitle("Distribution des espèces selon les pétales")



# =============================================================================
# ÉTAPE 6 : Prédiction d'une nouvelle fleur
# =============================================================================


new_flower <- data.frame(
  Sepal.Length = 5.1,
  Sepal.Width  = 3.5,
  Petal.Length = 4,
  Petal.Width  = 1.2
)

predict(model, new_flower, type = "class")


# =============================================================================
# ÉTAPE 7 : Test des prédictions sur un échantillon de test
# =============================================================================

# Chargement de la librairie nécessaire
library(dplyr)

# Sélection aléatoire de 20 observations pour constituer un échantillon test
TableTest <- iris[sample(nrow(iris), 20, replace = FALSE), ]

# Définition de la variable cible (y)
y <- iris$Species

# Définition des variables explicatives (X)
X <- iris
X$Species <- NULL



# =============================================================================
# -----------------Séparation des données : Train / Test-----------------------
# =============================================================================

set.seed(123)

# Échantillon test (20 observations)
TableTest <- iris[sample(nrow(iris), 20, replace = FALSE), ]

# Jeu d'entraînement = le reste des données
Train <- iris[!rownames(iris) %in% rownames(TableTest), ]

# Variables explicatives et cible
X_train <- Train[, -5]
y_train <- Train$Species

X_test  <- TableTest[, -5]
y_test  <- TableTest$Species



# =============================================================================
# Graphique – Distribution des espèce
# =============================================================================


library(ggplot2)

ggplot(Train, aes(x = Species, fill = Species)) +
  geom_bar() +
  theme_minimal() +
  labs(
    title = "Répartition des espèces - Jeu d'entraînement",
    x = "Espèce",
    y = "Nombre d'observations"
  ) +
  theme(legend.position = "none")





#Graphique clé (séparation des classes)
ggplot(Train, aes(Petal.Length, Petal.Width, color = Species)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_classic() +
  labs(
    title = "Séparation des espèces selon les dimensions des pétales",
    x = "Longueur du pétale",
    y = "Largeur du pétale"
  )




#Construction de l’arbre de décision
pred_test <- predict(model, X_test, type = "class")

# Matrice de confusion
table(Prediction = pred_test,
      Réel = y_test)



#Calcul de l’Accuracy
accuracy <- mean(pred_test == y_test)
accuracy


#Matrice de corrélation élégante

install.packages("corrplot")
library(corrplot)

cor_matrix <- cor(Train[,1:4])

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.cex = 0.8,
         title = "Matrice de corrélation - Variables explicatives",
         mar = c(0,0,1,0))

corrplot(cor_matrix,
         method = "number",
         type = "upper",
         tl.col = "black",
         tl.srt = 45)
