Sys.setlocale("LC_ALL", "fr_FR.UTF-8")
options(encoding="UTF-8")
package <- c("shiny", "shinydashboard", "rgdal", "yaml",'dygraphs',"gridExtra","stringr",
             'xts','ggplot2','plyr','scales','grid','googleVis','DT','dplyr','shinyBS',
             "plotly","shinyjs","sp","tidyr","lubridate","magrittr","ggmap","xts",
             "jsonlite","urltools","utils","rvest","rgeos","xml2","selectr",
             "raster","purrr","RColorBrewer","padr","rmarkdown")
new.packages <- package[!(package %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")
lapply(package, require, character.only = T)
# #SAVE
# jour_sem <<- c("Lundi","Mardi","Mercredi","Jeudi","Vendredi","Samedi","Dimanche")
# BDD_menu <<- read.csv2("Data/Repas.csv", stringsAsFactors = F, sep=';', header = TRUE)
# save.image(file = "Data/Data.RData")
load("Data/Data.RData")
jour_sem <<- jour_sem
BDD_menu <<- BDD_menu

shinyUI(navbarPage(title = "C&L Cuisine",#span("C&L Cuisine ",style = "font-family: 'Impact'"),
                   theme = "style/style.css",
                   fluid = TRUE, 
                   collapsible = TRUE,
                   tabPanel(span("Accueil ",style = "font-family: 'Tahoma'"),
                            column(width=12,
                                   fluidRow(width=12,imageOutput("baniere")),
                                   fluidRow(width=12,h1("Bienvenue"),
                                   HTML("<p style='font-size:20px;'><center>Je m'apelle Claire et je partage mon expérience autour
                                   du régime cétogène. <br> Vous trouverez ici des recettes, des vidéos, un générateur de menu pour 
                                   la semaine ainsi qu'un générateur de liste de courses en fonction du menu. 
                                   <br>Vous pouvez utiliser mes recettes ou ajouter les vôtres. 
                                   <br>Tout est gratuit ici et fait dans un esprit de bienveillance. 
                                   <br><br></p><p style='font-size:15px;'>
                                   Petit rappel : je ne suis pas médecin, ni nutritionniste ni même informaticienne !<br>
                                   A vous d’adapter mes recettes/menus en fonction de vous. </p></center>"))),
                            column(width=2),
                            column(width=4,HTML("<h2 style='font-size:20px;'><center>Venez me suivre sur Youtube</h2></center>"),
                                   uiOutput("YT")),
                            column(width=4,HTML("<h2 style='font-size:20px;'><center>Ainsi que sur Instagram</h2></center>"),
                                   uiOutput("INSTA"))),
                   tabPanel(span("Générateur de menu ",style = "font-family: 'Tahoma'"),
                            column(width=12,
                                   fluidRow(width=12,imageOutput("agenda")),
                                   fluidRow(width=12,h1("Créer son menu")),
                                  fluidRow(width=12, 
                                           column(width=2),
                                           column(width=2, numericInput("nb_per", 'Nombre de personnes (en cours...)', 2, min = 1)),
                                           column(width=1, numericInput("nb_jour", 'Nombre de jours', 7, min = 1, max = 30)),
                                           column(width=1, selectInput("jour_sem", 'Premier jour',jour_sem)),
                                           column(width=2, selectInput("saison",'Prendre en compte la saison (en cours...)',c("Non","Ete","Automne","Hiver","Printemps"))),
                                           column(width=2, selectInput("Auteur",'Auteur',choices = c("Tous",unique(BDD_menu$Auteur)),multiple = T,selected = "Tous"))),
                                  fluidRow(width=12,
                                           column(width=2),
                                           column(width=10,
                                                  br(),
                                                  fluidRow(width=12,
                                                           actionButton("gen_men", "Nouveau menu"),
                                                           downloadButton("extr_menu", "Télécharger menu"),
                                                           downloadButton("extr_courses", "Télécharger liste de courses (en cours...)")))),
                                  br(),
                                  br(),
                                  fluidRow(width=12,column(width=2),column(width=8,dataTableOutput('tablemenu'))),
                                  br(),
                                  fluidRow(width=12,h1("Importer une recette")),
                                  fluidRow(width=12, 
                                           column(width=2),
                                           column(width=8,textInput("nvrecette_text", "Nom de la recette", value = "", width="100%"))),
                                  fluidRow(width=12, 
                                           column(width=2),
                                           column(width=2,radioButtons("nvrecette_midi", "Type de recette :",c("Midi","Soir"))),
                                           column(width=2,selectInput("nvrecette_saison", "Saisonnalité :",selected = "Non",
                                                                      c("Non","Ete","Automne","Hiver","Printemps"), multiple = T)),
                                           column(width=2,textInput("nvrecette_auteur", "Auteur", value = "Anonyme", width="100%"))),
                                  fluidRow(width=12, 
                                           column(width=2),
                                           column(width=2,actionButton("nvrecette_button", "Ajouter ma recette"))),
                                  br(),br(),"."
                                  )),
                   tabPanel(span("Recettes salées",style = "font-family: 'Tahoma'"),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Blinis")),
                                     column(width=2,h2("Boeuf en sauce")),
                                     column(width=2,h2("Boulette de viande")),
                                     column(width=2,h2("Brochettes froides"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("S1")),
                                     column(width=2,uiOutput("S2")),
                                     column(width=2,uiOutput("S3")),
                                     column(width=2,uiOutput("S4"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Brochettes de poulet")),
                                     column(width=2,h2("4 Burgers")),
                                     column(width=2,h2("Bavette à l'échalotte")),
                                     column(width=2,h2("Enorme burger"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("S5")),
                                     column(width=2,uiOutput("S6")),
                                     column(width=2,uiOutput("S7")),
                                     column(width=2,uiOutput("S8"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Burger sauce cheddar")),
                                     column(width=2,h2("Caprice aux amandes")),
                                     column(width=2,h2("Cordon bleu")),
                                     column(width=2,h2("Crackers"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("S9")),
                                     column(width=2,uiOutput("S10")),
                                     column(width=2,uiOutput("S11")),
                                     column(width=2,uiOutput("S12"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Crackers halloween")),
                                     column(width=2,h2("Camembert rôti")),
                                     column(width=2,h2("Croque monsieur")),
                                     column(width=2,h2("Cuillère au saumon"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("S13")),
                                     column(width=2,uiOutput("S14")),
                                     column(width=2,uiOutput("S15")),
                                     column(width=2,uiOutput("S16"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Cabillaud croute de chorizo")),
                                     column(width=2,h2("Crêpe au thon")),
                                     column(width=2,h2("Champignon farci")),
                                     column(width=2,h2("Cheesecake aux courgettes"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("S17")),
                                     column(width=2,uiOutput("S18")),
                                     column(width=2,uiOutput("S19")),
                                     column(width=2,uiOutput("S20"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Chips de fromage")),
                                     column(width=2,h2("Cake au bleu")),
                                     column(width=2,h2("Flan jambon et feta")),
                                     column(width=2,h2("Frite de céléri"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("S21")),
                                     column(width=2,uiOutput("S22")),
                                     column(width=2,uiOutput("S23")),
                                     column(width=2,uiOutput("S24"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Fondue de poireaux")),
                                     column(width=2,h2("Frittata")),
                                     column(width=2,h2("Flan jambon et feta")),
                                     column(width=2,h2("Gressin"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("S42")),
                                     column(width=2,uiOutput("S43")),
                                     column(width=2,uiOutput("S44")),
                                     column(width=2,uiOutput("S45"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Galette")),
                                     column(width=2,h2("Kiri Gouter")),
                                     column(width=2,h2("Hot-Dog")),
                                     column(width=2,h2("Lomo et haricots verts"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("S26")),
                                     column(width=2,uiOutput("S27")),
                                     column(width=2,uiOutput("S28")),
                                     column(width=2,uiOutput("S29"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Muffins au thon")),
                                     column(width=2,h2("Mini quiche")),
                                     column(width=2,h2("Oeuf rôti au chorizo")),
                                     column(width=2,h2("Pâtes"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("S30")),
                                     column(width=2,uiOutput("S31")),
                                     column(width=2,uiOutput("S32")),
                                     column(width=2,uiOutput("S33"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Pizza minute")),
                                     column(width=2,h2("Pizza tomate & gorgonzola")),
                                     column(width=2,h2("Pain")),
                                     column(width=2,h2("Pain de mie"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("S34")),
                                     column(width=2,uiOutput("S35")),
                                     column(width=2,uiOutput("S36")),
                                     column(width=2,uiOutput("S37"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Pain de viande")),
                                     column(width=2,h2("Pain de mozzarella")),
                                     column(width=2,h2("Pain grillé à l'ail")),
                                     column(width=2,h2("Pizza facile"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("S38")),
                                     column(width=2,uiOutput("S39")),
                                     column(width=2,uiOutput("S40")),
                                     column(width=2,uiOutput("S41"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Pizza burger")),
                                     column(width=2,h2("Pâtes au poulet et pesto")),
                                     column(width=2,h2("Pizza poulet et asperges")),
                                     column(width=2,h2("Pizza 4 fromages"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("S46")),
                                     column(width=2,uiOutput("S47")),
                                     column(width=2,uiOutput("S48")),
                                     column(width=2,uiOutput("S49"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Pizza au fromage")),
                                     column(width=2,h2("Poivrons farcis")),
                                     column(width=2,h2("Pizza poulet")),
                                     column(width=2,h2("Poulet et courgettes"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("S50")),
                                     column(width=2,uiOutput("S51")),
                                     column(width=2,uiOutput("S52")),
                                     column(width=2,uiOutput("S53"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Pizza paysanne")),
                                     column(width=2,h2("Quiche aux poireaux")),
                                     column(width=2,h2("Risotto de courge")),
                                     column(width=2,h2("Risotto de chou-fleur"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("S54")),
                                     column(width=2,uiOutput("S55")),
                                     column(width=2,uiOutput("S56")),
                                     column(width=2,uiOutput("S57"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Salade d'endive")),
                                     column(width=2,h2("Salade saucisse et olives")),
                                     column(width=2,h2("Salade de thon")),
                                     column(width=2,h2("Salade de courgette et mozzarella"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("S58")),
                                     column(width=2,uiOutput("S59")),
                                     column(width=2,uiOutput("S60")),
                                     column(width=2,uiOutput("S61"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Sandwich au thon")),
                                     column(width=2,h2("Sauce BigMac")),
                                     column(width=2,h2("Salade tomates et noix")),
                                     column(width=2,h2("Sushi"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("S62")),
                                     column(width=2,uiOutput("S63")),
                                     column(width=2,uiOutput("S64")),
                                     column(width=2,uiOutput("S65"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("tacos au boeuf")),
                                     column(width=2,h2("Salade d'avocat et bacon")),
                                     column(width=2,h2("Salade concombre et jambon")),
                                     column(width=2,h2("Sushi au thon"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("S66")),
                                     column(width=2,uiOutput("S67")),
                                     column(width=2,uiOutput("S68")),
                                     column(width=2,uiOutput("S69"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Tarte aux poireaux")),
                                     column(width=2,h2("Tarte du soleil")),
                                     column(width=2,h2("Tarte au maroilles")),
                                     column(width=2,h2("Toast thon et avocat"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("S70")),
                                     column(width=2,uiOutput("S71")),
                                     column(width=2,uiOutput("S72")),
                                     column(width=2,uiOutput("S73"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Tarte tomates et chèvre")),
                                     column(width=2,h2("Tarte potimarron")),
                                     column(width=2,h2("Tortillas")),
                                     column(width=2,h2("Toast au saumon"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("S74")),
                                     column(width=2,uiOutput("S75")),
                                     column(width=2,uiOutput("S76")),
                                     column(width=2,uiOutput("S77"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Tartiflette de navet")),
                                     column(width=2,h2("Velouté d'asperges")),
                                     column(width=2,h2("")),
                                     column(width=2,h2(""))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("S78")),
                                     column(width=2,uiOutput("S79")),
                                     column(width=2,uiOutput("S80")),
                                     column(width=2,uiOutput("S81")))),
                   tabPanel(span("Recettes sucrées",style = "font-family: 'Tahoma'"),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Barre céréales, noix de coco")),
                                     column(width=2,h2("Brownies")),
                                     column(width=2,h2("Bûche amande")),
                                     column(width=2,h2("Bûche au citron"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("D1")),
                                     column(width=2,uiOutput("D2")),
                                     column(width=2,uiOutput("D3")),
                                     column(width=2,uiOutput("D4"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Mi cheesecake mi brownies")),
                                     column(width=2,h2("Cheesecake au café")),
                                     column(width=2,h2("Cheesecake aux fruits rouges")),
                                     column(width=2,h2("Barre de chocolat blanc"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("D5")),
                                     column(width=2,uiOutput("D6")),
                                     column(width=2,uiOutput("D7")),
                                     column(width=2,uiOutput("D8"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Chocolats de Noël")),
                                     column(width=2,h2("Coockies 100% chocolat")),
                                     column(width=2,h2("Coockies fourés au beurre de cacahuète")),
                                     column(width=2,h2("Coockies amandes et noisettes"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("D9")),
                                     column(width=2,uiOutput("D10")),
                                     column(width=2,uiOutput("D11")),
                                     column(width=2,uiOutput("D12"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Cookies au beurre de cacahuète")),
                                     column(width=2,h2("Crème brûlée")),
                                     column(width=2,h2("Cupcake vanille et chocolat")),
                                     column(width=2,h2("Entremet au citron"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("D13")),
                                     column(width=2,uiOutput("D14")),
                                     column(width=2,uiOutput("D15")),
                                     column(width=2,uiOutput("D16"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Entremet au café")),
                                     column(width=2,h2("Financier")),
                                     column(width=2,h2("Flan")),
                                     column(width=2,h2("Framboisier"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("D17")),
                                     column(width=2,uiOutput("D18")),
                                     column(width=2,uiOutput("D19")),
                                     column(width=2,uiOutput("D20"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Gâteau café et épices")),
                                     column(width=2,h2("Gaufre")),
                                     column(width=2,h2("Kinder délice")),
                                     column(width=2,h2("Kinder surprise"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("D21")),
                                     column(width=2,uiOutput("D22")),
                                     column(width=2,uiOutput("D23")),
                                     column(width=2,uiOutput("D24"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Madeleine")),
                                     column(width=2,h2("Moelleux coeur chocolat")),
                                     column(width=2,h2("Moelleux au chocolat")),
                                     column(width=2,h2("Pancake"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("D25")),
                                     column(width=2,uiOutput("D26")),
                                     column(width=2,uiOutput("D27")),
                                     column(width=2,uiOutput("D28"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Panna Cotta")),
                                     column(width=2,h2("Sablés canelle coco")),
                                     column(width=2,h2("Verines chocolat citron")),
                                     column(width=2,h2("Gateau zebré"))),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,uiOutput("D29")),
                                     column(width=2,uiOutput("D30")),
                                     column(width=2,uiOutput("D31")),
                                     column(width=2,uiOutput("D32")))
                            ),
                   tabPanel(span("A boire !",style = "font-family: 'Tahoma'"),
                            fluidRow(width=12, 
                                     column(width=2),
                                     column(width=2,h2("Trois thés glacés")),
                                     column(width=2,h2("Blue Lagoon")),
                                     column(width=2,h2("Cosmopolitan")),
                                     column(width=2,h2("Rhum citron"))),
                            fluidRow(width=12, 
                                    column(width=2),
                                    column(width=2,uiOutput("B1")),
                                    column(width=2,uiOutput("B2")),
                                    column(width=2,uiOutput("B3")),
                                    column(width=2,uiOutput("B4"))))
                   
))
