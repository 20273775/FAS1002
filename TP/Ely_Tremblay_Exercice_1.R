#Travail Pratique 1
#Chargement des paquetages nécessaires
library(tidyverse)

#Question 1
#Importez le fichier donnees_procrastination.csv dans R. Ajouter ensuite une colonne
#d'identifiant unique numérique par participant nommée id. Déplacer finalement id
#comme première colonne dans votre jeu de données.
donnees_procrastination <- read_csv("data/raw/donnees_procrastination.csv")

donnees_procrastination <- donnees_procrastination %>%
  mutate(id = row_number()) %>%
  select(id, everything())

#Vérification : id doit être numérique, unique et en première colonne
names(donnees_procrastination)[1]
anyDuplicated(donnees_procrastination$id) # doit être 0

#Question 2
#La variable CYCLE représente le cycle d'études de la personne : 1 = 1er cycle,
#2 = 2e cycle et 3 = 3e cycle. À partir du tableau de données original
#(donnees_procrastination), créez un tableau nommé procrastination_cycle_sup qui
#contient seulement les personnes inscrites au 2e ou au 3e cycle ET pour qui la
#procrastination n'est jamais un problème (DEGRE_PROC_PROB). Au moins une des
#conditions de filtrage doit être formulée par exclusion à l'aide de != ou de !.
procrastination_cycle_sup <- donnees_procrastination %>%
  filter(CYCLE != 1 & DEGRE_PROC_PROB == "Jamais un problème")

#Vérification : toutes les observations conservées doivent être en cycle supérieur
#(2e ou 3e cycle) ET avoir répondu "Jamais un problème"
table(procrastination_cycle_sup$CYCLE, useNA = "ifany") # ne devrait afficher que 2 et 3
table(procrastination_cycle_sup$DEGRE_PROC_PROB, useNA = "ifany") # ne devrait afficher que "Jamais un problème"

#Question 3
#À partir du tableau de données original (donnees_procrastination), calculez
#d'abord la moyenne observée de la variable procrastination en excluant les
#valeurs manquantes. Stockez cette valeur dans un objet nommé moyenne_proc.
#Créez ensuite un tableau nommé donnees_filtered_mean_procrastination contenant
#uniquement les participants dont le score de procrastination est inférieur à
#50 % de la moyenne ou supérieur à 150 % de la moyenne. Les seuils doivent être
#calculés à partir de l'objet moyenne_proc et ne doivent pas être inscrits
#directement dans le code sous forme de valeurs fixes.
moyenne_proc <- mean(donnees_procrastination$procrastination, na.rm = TRUE)

donnees_filtered_mean_procrastination <- donnees_procrastination %>%
  filter(procrastination < 0.5 * moyenne_proc | procrastination > 1.5 * moyenne_proc)

#Vérification : toutes les observations conservées respectent l'un ou l'autre des seuils
donnees_filtered_mean_procrastination %>%
  summarise(
    respecte_les_seuils = all(procrastination < 0.5 * moyenne_proc |
                                 procrastination > 1.5 * moyenne_proc),
    min_proc = min(procrastination),
    max_proc = max(procrastination)
  )

#Question 4 : Débogage et validation
#Le code suivant devait créer un tableau nommé filtered_data contenant seulement
#les participants : dont le score de procrastination est supérieur à la moyenne;
#qui sont célibataires; qui n'ont aucune donnée manquante sur la variable AGE.
#Cependant, le code original contenait des erreurs logiques (usage de | au lieu de
#&, et is.na(AGE) au lieu de !is.na(AGE)). Voici la version corrigée :
filtered_data <- donnees_procrastination |>
  filter(
    procrastination > moyenne_proc &
      CIVIL == "Célibataire" &
      !is.na(AGE)
  )

#Vérification : les trois critères doivent être respectés simultanément
filtered_data %>%
  summarise(
    proc_sup_moyenne = all(procrastination > moyenne_proc, na.rm = TRUE),
    tous_celibataires = all(CIVIL == "Célibataire"),
    aucun_age_manquant = all(!is.na(AGE))
  )

#Question 5.1 : Recoder l'échelle des items du CAQ
#Recoder toutes les variables d'items du CAQ de 0–6 vers 1–7 en sélectionnant
#les colonnes directement dans mutate(). Après le recodage, ajoutez une
#vérification montrant que la transformation a été effectuée correctement.
donnees_caq <- donnees_procrastination %>%
  mutate(across(starts_with("CAQ"), ~ .x + 1))

#Vérification : l'étendue de CAQ1 doit passer de 0-6 à 1-7
range(donnees_procrastination$CAQ1, na.rm = TRUE)
range(donnees_caq$CAQ1, na.rm = TRUE)

#Question 5.2 : Recoder les items inversés
#Les questions 10, 16, 17 et 18 sont des questions inversées. Utilisez paste0()
#pour créer dynamiquement un vecteur contenant les noms de ces variables, puis
#recodez-les dans le tableau obtenu à l'étape 5.1. Sur une échelle de 1 à 7, le
#recodage inversé doit transformer 1 en 7, 2 en 6, 3 en 5, etc.
items_inverses <- paste0("CAQ", c(10, 16, 17, 18))

donnees_caq <- donnees_caq %>%
  mutate(across(all_of(items_inverses), ~ 8 - .x))

#Vérification : pour CAQ10, la valeur finale attendue est 7 - (valeur d'origine 0-6)
donnees_procrastination %>%
  select(id, CAQ10_original = CAQ10) %>%
  left_join(donnees_caq %>% select(id, CAQ10_recode = CAQ10), by = "id") %>%
  mutate(valeur_attendue = 7 - CAQ10_original) %>%
  summarise(concordance = all(CAQ10_recode == valeur_attendue, na.rm = TRUE))

#Question 5.3 : Calculer le score total
#Le score total du CAQ est calculé à partir des items 1, 4, 6, 7, 10, 16, 17 et 18.
#Utilisez paste0() pour créer dynamiquement le vecteur des noms de variables à
#inclure dans le score. Calculez ensuite une variable CAQ_tot correspondant à la
#somme des huit items. Le score total doit être manquant (NA) lorsqu'au moins un
#des huit items est manquant. Pour les scores non manquants, la valeur théorique
#possible se situe entre 8 et 56.
items_score <- paste0("CAQ", c(1, 4, 6, 7, 10, 16, 17, 18))

donnees_caq <- donnees_caq %>%
  mutate(CAQ_tot = rowSums(across(all_of(items_score)), na.rm = FALSE))

#Vérification : étendue des scores non manquants et nombre de scores manquants
donnees_caq %>%
  summarise(
    min_tot = min(CAQ_tot, na.rm = TRUE),
    max_tot = max(CAQ_tot, na.rm = TRUE),
    n_manquants = sum(is.na(CAQ_tot))
  )

#Question 6.1 : Créer les catégories d'âge
#La variable AGE est numérique. Créez une nouvelle variable nommée age_cat
#regroupant les participants selon les catégories suivantes : 20 ans et -;
#21 à 29 ans; 30 à 39 ans; 40 ans et +. Utilisez case_when() et nommez les
#catégories exactement comme ci-dessus.
donnees_caq <- donnees_caq %>%
  mutate(age_cat = case_when(
    AGE <= 20 ~ "20 ans et -",
    AGE >= 21 & AGE <= 29 ~ "21 à 29 ans",
    AGE >= 30 & AGE <= 39 ~ "30 à 39 ans",
    AGE >= 40 ~ "40 ans et +"
  ))

#Question 6.2 : Convertir la variable en facteur
#Convertissez age_cat en facteur en vous assurant que les niveaux apparaissent
#dans l'ordre croissant d'âge. Ajoutez une vérification permettant d'examiner
#les niveaux du facteur et la distribution des observations.
donnees_caq <- donnees_caq %>%
  mutate(age_cat = factor(
    age_cat,
    levels = c("20 ans et -", "21 à 29 ans", "30 à 39 ans", "40 ans et +")
  ))

#Vérification
levels(donnees_caq$age_cat)
table(donnees_caq$age_cat, useNA = "ifany")

#Question 6.3 : Appliquer les critères de filtrage
#À partir du tableau obtenu à l'étape précédente : retirez les personnes de 20
#ans et moins; retirez les personnes de 40 ans et plus; excluez les participants
#célibataires, sauf ceux qui ont entre 30 et 39 ans.
donnees_procrastination_filtered <- donnees_caq %>%
  filter(age_cat %in% c("21 à 29 ans", "30 à 39 ans")) %>%
  filter(CIVIL != "Célibataire" | age_cat == "30 à 39 ans")

#Vérification : plus aucune personne de 20 ans et moins ou de 40 ans et plus,
#et plus aucun·e célibataire dans la catégorie 21 à 29 ans
table(donnees_procrastination_filtered$age_cat, useNA = "ifany")
donnees_procrastination_filtered %>%
  filter(age_cat == "21 à 29 ans") %>%
  count(CIVIL)

#Question 7 : Calculer des statistiques descriptives
#À partir de donnees_procrastination_filtered : conservez uniquement les
#étudiants de premier cycle, regroupez les observations selon SEXE et age_cat,
#calculez pour chaque groupe n, la moyenne de CAQ_tot et son écart-type (en
#excluant les valeurs manquantes), et arrondissez à deux décimales.
stats_caq_premier_cycle <- donnees_procrastination_filtered %>%
  filter(CYCLE == 1) %>%
  group_by(SEXE, age_cat) %>%
  summarise(
    n = n(),
    moyenne_CAQ = round(mean(CAQ_tot, na.rm = TRUE), 2),
    ecart_type_CAQ = round(sd(CAQ_tot, na.rm = TRUE), 2),
    .groups = "drop"
  )

stats_caq_premier_cycle

#Interprétation : chez les étudiant.e.s de premier cycle, la moyenne de CAQ_tot
#varie peu entre les quatre groupes (de 34,3 à 36,1 sur une échelle observée
#d'environ 28 à 48), un écart d'environ 2 points qui est petit par rapport aux
#écarts-types intra-groupes (2,1 à 3,4). Il n'y a donc pas de différence
#notable selon le sexe ou la catégorie d'âge dans ces données descriptives.
#Les groupes Homme 30-39 ans (n = 3) et Femme 30-39 ans (n = 15) comptent peu
#d'observations, donc leurs moyennes sont peu précises et ne devraient pas être
#sur-interprétées. Ces comparaisons sont purement descriptives; aucun test
#statistique n'a été effectué pour évaluer si ces différences sont significatives.

#Déclaration IA :
#Outil utilisé : Posit Assistant (Claude)
#Nature de l'aide obtenue : aide à la rédaction du code dplyr pour les filtres,
#recodages et agrégations, ainsi qu'à la vérification de la logique de chaque
#transformation demandée dans l'énoncé.
