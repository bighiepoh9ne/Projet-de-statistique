#Les librairies dont j'aurai besoin 
install.packages("dplyr")
install.packages("stringr")
install.packages("corrplot")
library(dplyr)
library(ggplot2)
library(corrplot)
library(FSA) 
library(stringr)
library(questionr)
library(ggpubr) 
library(effsize)


#création de base de données
#on travaille directement dans le dossier où se situe la base à extraire 

 data <- read.csv(file.choose(), header = TRUE, sep = ",")
 data2 <- data[,-c(11,12,15,16)]
 data2[data2 == ""] <- NA
 data2[data2 == "N/A"] <- NA 
 data3 <- na.omit(data2)
 View(data3)
 unique(data3$Platform)

 #on va créer une nouvelle variable franchise
 unique(data3$Name)
 data4<-data3 %>% mutate(Franchise=case_when(
   str_detect(Name, regex("Mario", ignore_case = TRUE)) ~ "Mario",
   str_detect(Name, regex("Pokemon|Pokémon|Poké", ignore_case = TRUE)) ~ "Pokémon",
   str_detect(Name, regex("Zelda|The Legend of Zelda", ignore_case = TRUE)) ~ "The Legend of Zelda",
   str_detect(Name, regex("Animal Crossing", ignore_case = TRUE)) ~ "Animal Crossing",
   str_detect(Name, regex("Donkey Kong", ignore_case = TRUE)) ~ "Donkey Kong",
   str_detect(Name, regex("Kirby", ignore_case = TRUE)) ~ "Kirby",
   str_detect(Name, regex("Wii Sports|Wii Play|Wii Fit|Wii Party", ignore_case = TRUE)) ~ "Wii Series",
   str_detect(Name, regex("Grand Theft Auto", ignore_case = TRUE)) ~ "Grand Theft Auto",
   str_detect(Name, regex("Call of Duty", ignore_case = TRUE)) ~ "Call of Duty",
   str_detect(Name, regex("Final Fantasy", ignore_case = TRUE)) ~ "Final Fantasy",
   str_detect(Name, regex("Need for Speed", ignore_case = TRUE)) ~ "Need for Speed",
   str_detect(Name, regex("Resident Evil", ignore_case = TRUE)) ~ "Resident Evil",
   str_detect(Name, regex("The Sims", ignore_case = TRUE)) ~ "The Sims",
   str_detect(Name, regex("Assassin's Creed", ignore_case = TRUE)) ~ "Assassin's Creed",
   str_detect(Name, regex("Halo", ignore_case = TRUE)) ~ "Halo",
   str_detect(Name, regex("Metal Gear Solid", ignore_case = TRUE)) ~ "Metal Gear Solid",
   str_detect(Name, regex("Monster Hunter", ignore_case = TRUE)) ~ "Monster Hunter",
   str_detect(Name, regex("Tekken", ignore_case = TRUE)) ~ "Tekken",
   str_detect(Name, regex("Tomb Raider", ignore_case = TRUE)) ~ "Tomb Raider",
   str_detect(Name, regex("Just Dance", ignore_case = TRUE)) ~ "Just Dance",
   str_detect(Name, regex("Street Fighter", ignore_case = TRUE)) ~ "Street Fighter",
   str_detect(Name, regex("God of War", ignore_case = TRUE)) ~ "God of War",
   str_detect(Name, regex("Battlefield", ignore_case = TRUE)) ~ "Battlefield",
   str_detect(Name, regex("NBA 2K", ignore_case = TRUE)) ~ "NBA 2K",
   str_detect(Name, regex("Madden NFL", ignore_case = TRUE)) ~ "Madden NFL",
   str_detect(Name, regex("FIFA", ignore_case = TRUE)) ~ "FIFA",
   str_detect(Name, regex("Age of Empires", ignore_case = TRUE)) ~ "Age of Empires",
   str_detect(Name, regex("Lego", ignore_case = TRUE)) ~ "Lego ",
   str_detect(Name, regex("Crash Bandicoot", ignore_case = TRUE)) ~ "Crash Bandicoot",
   str_detect(Name, regex("Gran Turismo", ignore_case = TRUE)) ~ "Gran Turismo",
   str_detect(Name, regex("Uncharted", ignore_case = TRUE)) ~ "Uncharted",
   str_detect(Name, regex("Gears of War", ignore_case = TRUE)) ~ "Gears of War",
   str_detect(Name, regex("Fallout", ignore_case = TRUE)) ~ "Fallout",
   str_detect(Name, regex("Far Cry", ignore_case = TRUE)) ~ "Far cry ",
   str_detect(Name, regex("Batman", ignore_case = TRUE)) ~ "Batman",
   TRUE ~ "Autre Franchise/Non Classé"
 ))
 
 #création d'une autre variable console 
 data5<-data4%>% mutate(Console_creator=case_when(
   str_detect(Platform,regex("psv|ps3|ps4|ps|psp|ps",ignore_case =TRUE))~ "Sony",
   str_detect(Platform,regex("wii|ds|3ds|wiiu|gc|gba",ignore_case = TRUE))~"Nintendo",
   str_detect(Platform,regex("xone|x360|xb",ignore_case = TRUE))~"Microsoft",
   TRUE~"Autre Createur/Non Classé"
 ))
 
 data6<-data5[,c(1,13,2,14,3,4,5,6,7,8,9,10,11,12)];

    data4 %>%
     filter(Franchise == "Wii Series") %>%
   select(Name)
                 
    sum(is.na(data2))   # avant nettoyage
    sum(is.na(data3))

# les valeurs de User_Score sont des chaine de caractère donc on va transformer le numeric
 data6$User_Score[data6$User_Score == "tbd"] <- NA

 #on va verifier la distribution des variables quantitatives
 #test de normalité 
 is.numeric(data6$User_Score) #on verifie d'abord que les valeurs sont numériques 
 data6$User_Score<- as.numeric(data6$User_Score)
 shapiro.test(data6$User_Score)
 
 #on voir que l'on ne peut faire se test qu'avec 5000 lignes maxi(un échantillon)
 set.seed(123)
 data6_sample <- data6[sample(nrow(data6), 5000), ]

 #on recommance avec la nouvelle base tronqué pour avoir un aperçu de la tendance actuelle 
 # on refait le test de normalité
 data6_sample$User_Score<- as.numeric(data6_sample$User_Score)
 shapiro.test(data6_sample$User_Score) # ne suit pas une loi normale 
 
 data6_sample$User_Count<- as.numeric(data6_sample$User_Count)
 shapiro.test(data6_sample$User_Count) # ne suit pas une loi normale 
 
 data6_sample$User_Score<- as.numeric(data6_sample$Global_Sales)
 shapiro.test(data6_sample$Global_Sales) # ne suit pas une loi normale 
 #les 3 variables quantitatives principales ne suivent pas de loi normale

 
 
 #corrélation entre les valeurs quantitatives 
 num_vars <- data6 %>% 
   select(User_Score, User_Count, Global_Sales)
 cor(num_vars, use="complete.obs", method="spearman")
 
#on va faire un calcul des moyennes de user score par variable qualitative 
 #moyenne par franchise 
 franchise_scores <- data6 %>%
   group_by(Franchise) %>%
   summarise(mean_score = mean(User_Score, na.rm = TRUE),
             n = n())
 
 #moyenne par genre 
 Genre_scores <- data6 %>%
   group_by(Genre) %>%
   summarise(mean_score1 = mean(User_Score, na.rm = TRUE),
             n = n()) 
 
 #moyenne par plateforme
 plateforme_score <- data6 %>%
   group_by(Platform) %>%
   summarise(mean_score2 = mean(User_Score, na.rm = TRUE),
             n = n()) 
 
 #moyenne par createur de console  
 creator_score <- data6 %>%
   group_by(Console_creator) %>%
   summarise(mean_score3 = mean(User_Score, na.rm = TRUE),
             n = n())
 
 #moyenne par publisher  
 publisher_score <- data6 %>%
   group_by(Publisher) %>%
   summarise(mean_score4= mean(User_Score, na.rm = TRUE),
             n = n()) 
 
 #moyenne par année de publication
 year_score <- data6 %>%
   group_by(Year_of_Release) %>%
   summarise(mean_score5 = mean(User_Score, na.rm = TRUE),
             n = n()) 
 
 
 #on va faire une visualisation pour les différentes moyennes
 ggplot(franchise_scores, aes(x = reorder(Franchise, mean_score), y = mean_score)) +
   geom_col(fill="blue") +
   coord_flip() +
   labs(title="Moyenne du user_score par Franchise")
 
 ggplot(Genre_scores, aes(x = reorder(Genre, mean_score1), y = mean_score1)) +
   geom_col(fill="red") +
   coord_flip() +
   labs(title="Moyenne du user_score par Genre")
 
 ggplot(plateforme_score, aes(x = reorder(Platform, mean_score2), y = mean_score2)) +
   geom_col(fill="green") +
   coord_flip() +
   labs(title="Moyenne du user_score par Plateforme")
 
 ggplot(creator_score, aes(x = reorder(Console_creator, mean_score3), y = mean_score3)) +
   geom_col(fill="purple") +
   coord_flip() +
   labs(title="Moyenne du user_score par Créateur de console")
 
 ggplot(publisher_score, aes(x = reorder(Publisher, mean_score4), y = mean_score4)) +
   geom_col(fill="yellow") +
   coord_flip() +
   labs(title="Moyenne du user_score par Publisher")
 
 ggplot(year_score, aes(x = reorder(Year_of_Release, mean_score5), y = mean_score5)) +
   geom_col(fill="orange") +
   coord_flip() +
   labs(title="Moyenne du user_score par année de publication")
 
 #on a une première visualisation qui nous montre que la moyenne du mean_score ne dépend pas vraiment du nombre d'élément dans chaque franchise 
 #on va faire un graphique pour un tableau avec la franchise et le user_score afin de voir de façon plus visuelle un autre réalité du user_score selon les franchises
 ggplot(franchise_scores, aes(x=Franchise, y=mean_score, fill=mean_score)) +
   geom_boxplot() +
   theme(axis.text.x = element_text(angle=45, hjust=1)) +
   labs(title="Répartition de la moyenne score par franchise")
 
 #on va voir qu'il y a une répartition des moyennes qui ne suit pas une loi normale et on peut voir quelques valeurs spécialement 
 #haute et d'autre spécialement basse
 
 # on va faire la même chose pour le genre 
 ggplot(Genre_scores, aes(x=Genre, y=mean_score1, fill=mean_score1)) +
   geom_boxplot() +
   theme(axis.text.x = element_text(angle=45, hjust=1)) +
   labs(title="Répartition de la moyenne score par genre")
#on voit une repartition sinusoidale des moyenne selon le type de jeu sauf toujours avec des valeurs qui sont décalées   

  
 #test de krustal-wallis vu que les donnés sont non paramétriques
 kruskal.test(User_Score~Franchise, data=data6_sample)
 res <- dunnTest(User_Score ~ Franchise, data = data6_sample, method = "bonferroni")
 
 # transformer le résultat en data.frame
 df_res <- as.data.frame(res$res)
 
 # filtrer les comparaisons significatives
 sig <- df_res %>% filter(P.adj < 0.05) %>%
   arrange(P.adj)
 
 print(sig)
 
 # boxplot trié par médiane
 meds <- data6_sample %>%
   group_by(Franchise) %>%
   summarize(med = median(User_Score, na.rm = TRUE)) %>%
   arrange(med)
 
 data6_sample$Franchise <- factor(data6_sample$Franchise, levels = meds$Franchise)
 
 boxplot(User_Score ~ Franchise, data = data6_sample,
         las = 2,  # labels verticales
         main = "User_Score par Franchise (ordonné par médiane)",
         ylab = "User_Score")
 
 #on fait la même chose pour la plateforme
 krus <- kruskal.test(User_Score ~ Platform, data = data6_sample)
 res_dunn <- dunnTest(User_Score ~ Platform, data = data6_sample, method = "bonferroni")
 df_dunn <- as.data.frame(res_dunn$res)
 
 meds2 <- data6_sample %>%
   group_by(Platform) %>%
   summarise(med = median(User_Score, na.rm = TRUE)) %>%
   arrange(med)
 data6_sample$Platform <- factor(data6_sample$Platform, levels = meds$Platform)
 
 ggplot(data6_sample, aes(x = User_Score , y = Platform)) +
   geom_boxplot(outlier.size = 1) +
   coord_flip() +
   labs(title = "User_Score par Platform (ordonné par médiane)", x = "", y = "User_Score") +
   theme_minimal()
 
 # 5) Mesure d'effet (Cliff's delta) pour une paire d'exemple :
 # remplacer "PC" et "PS4" par les plateformes qui t'intéressent
 groupA <- data6 %>% filter(Platform == "PC") %>% pull(User_Score)
 groupB <- data6 %>% filter(Platform == "PS4") %>% pull(User_Score)
 cliff <- cliff.delta(groupA, groupB)
 print(cliff)
 kruskal.test(User_Score~Genre, data=data6_sample)
 dunnTest(User_Score ~ Genre, data = data6_sample, method = "bonferroni")
 
 kruskal.test(User_Score~Console_creator, data=data6_sample)
 dunnTest(User_Score ~ Console_creator, data = data6_sample, method = "bonferroni")
 
 kruskal.test(User_Score~Publisher, data=data6_sample)
 ggplot(data6_sample, aes(x=reorder(Publisher, User_Score, median, na.rm=TRUE),
                          y=User_Score, fill=Publisher)) +
   geom_boxplot(show.legend = FALSE) +
   coord_flip() +
   labs(title="Distribution du User Score par Publisher",
        x="Publisher", y="User Score")
 
 kruskal.test(User_Score~Franchise, data=data6_sample)
 dunnTest(User_Score ~ Franchise, data = data6_sample, method = "bonferroni")
 
#on va calculer un peu différemment pour les la variables de ventes globale
 data6_sample <- data6_sample %>%
   mutate(Sales_Group = cut(Global_Sales,
                            breaks = quantile(Global_Sales, probs = seq(0,1,0.25), na.rm = TRUE),
                            labels = c("Faibles ventes", "Moyennes", "Fortes", "Très fortes"),
                            include.lowest = TRUE))
 
 kruskal.test(User_Score ~ Sales_Group, data = data6_sample)
 
 #calcul des indicateurs 
 indicateurs <- data6 %>%
   group_by(Genre, Platform, Publisher) %>%
   summarise(
     mean_user = mean(User_Score, na.rm = TRUE),
     sd_user = sd(User_Score, na.rm = TRUE),
     n = n()
   )
 
 #on va faire par la suite une map de chaleur 
 corrplot(cor(num_vars, use="complete.obs", method="spearman"), method="color")
 
 
 #on va faire des illustration avec des boites à moustaches
 ggplot(data6, aes(x = Console_creator, y = User_Score, fill = Console_creator)) +
   geom_boxplot() +
   theme_minimal() +
   labs(title = "User Scores par créateur de console", x = "Créateur de console", y = "Note des joueurs")
 
 ggplot(data6, aes(x = Platform, y = User_Score, fill = Platform)) +
   geom_boxplot() +
   theme_minimal() +
   labs(title = "User Scores par Platforme", x = "Platforme", y = "Note des joueurs")
 
 ggplot(data6, aes(x = Genre, y = User_Score, fill = Genre)) +
   geom_boxplot() +
   theme_minimal() +
   labs(title = "User Scores par genre", x = "Genre", y = "Note des joueurs")
 
 ggplot(data6, aes(x = Franchise, y = User_Score, fill = Franchise)) +
   geom_boxplot() +
   theme_minimal() +
   labs(title = "User Scores par franchise", x = "Platforme", y = "Note des joueurs")
 
 ggplot(data6, aes(x = Year_of_Release, y = User_Score, fill = Year_of_Release)) +
   geom_boxplot() +
   theme_minimal() +
   labs(title = "User Scores par année de sortie", x = "Année de sortie", y = "Note des joueurs")
 
 ggplot(data6, aes(x = Publisher, y = User_Score, fill = Publisher)) +
   geom_boxplot() +
   theme_minimal() +
   labs(title = "User Scores par Publisher ", x = "publisher ", y = "Note des joueurs")
 
   
   # 5 on regarde l'influence des variables sur le score user 
 # on va créer un modèle linéaire selon les variables qualitative 
 modele <- lm(User_Score ~ Franchise + Genre + Platform + Publisher + Global_Sales, data=data6)
 summary(modele)
 
 #On va regarder la multicolinéarité 
 car::vif(modele)
 #le modèle est nickel niveau multicolinéarité.
 #Aucune variable n’a une inflation de variance problématique : tous les GVIF^(1/(2*Df)) < 2 (et même < 1.3).
 #on peut donc garder toutes tes variables sans souci, elles n’expliquent pas la même chose entre elles.
 
 
 #on a fait des graphiques de residus du modele que nous avons créé (annexe)
 plot(modele)
 broom::tidy(modele)

 
 