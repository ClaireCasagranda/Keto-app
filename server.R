shinyServer(function(input, output) {
    reactData<-reactiveValues(list_ing=NULL,list_quant=NULL,list_unit=NULL,table_menu=NULL)
    
    #IMAGE ACCUEIL
    output$baniere <- renderImage({
        outfile <- normalizePath('www/Ban3.png')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    
    #IMAGE ACCUEIL
    output$agenda <- renderImage({
        outfile <- normalizePath('www/Resto.jpg')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    
    #IMAGE YT
    output$YT1 <- renderImage({
        outfile <- normalizePath('www/YT.PNG')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$YT <- renderUI({
        tags$a(imageOutput("YT1"),href="https://youtube.com/c/CLCuisineKeto")})
    
    #IMAGE INSTA
    output$INSTA1 <- renderImage({
        outfile <- normalizePath('www/Insta.PNG')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$INSTA <- renderUI({
        tags$a(imageOutput("INSTA1"),href="https://www.instagram.com/cetlcuisine/?hl=fr")})
    
    
    # GENERATEUR DE MENU
    output$tablemenu <- renderDataTable({
        name <- c("Repas",jour_sem[min(which(jour_sem%in%input$jour_sem)):7],rep(jour_sem,100))[0:(input$nb_jour+1)]
        tmp <- data.frame(matrix(ncol = (length(name)), nrow = 2))
        colnames(tmp) <- name
        tmp$Repas <- c('Midi','Soir')
        for(i in 1:input$nb_jour){
            tmp[1,(i+1)] <- sample(BDD_menu$Nom[BDD_menu$Repas%in%"Midi"],1)
            tmp[2,(i+1)] <- sample(BDD_menu$Nom[BDD_menu$Repas%in%"Soir"],1)}
        reactData$table_menu <- tmp
        datatable(tmp, options = list(searching = FALSE, scrollCollapse=FALSE,lengthChange=FALSE), rownames = FALSE )})
    
    #GENERATION AGAIN
    observeEvent(input$gen_men, {
        output$tablemenu <- renderDataTable({
            name <- c("Repas",jour_sem[min(which(jour_sem%in%input$jour_sem)):7],rep(jour_sem,100))[0:(input$nb_jour+1)]
            tmp <- data.frame(matrix(ncol = (length(name)), nrow = 2))
            colnames(tmp) <- name
            tmp$Repas <- c('Midi','Soir')
            for(i in 1:input$nb_jour){
                tmp[1,(i+1)] <- str_wrap(sample(BDD_menu$Nom[BDD_menu$Repas%in%"Midi"],1),15)
                tmp[2,(i+1)] <- str_wrap(sample(BDD_menu$Nom[BDD_menu$Repas%in%"Soir"],1),15)}
            reactData$table_menu <- tmp
            datatable(tmp, options = list(searching = FALSE, scrollCollapse=FALSE,lengthChange=FALSE), rownames = FALSE )})})
    
    #EXTRAIRE MENU
    output$extr_menu <- downloadHandler(
        filename = function() {
            paste("Menu",Sys.Date(),".jpeg", sep="")},
        content = function(file){
            tmp <- reactData$table_menu
            for(i in 2:ncol(tmp)){
                tmp[,i] <- str_wrap(tmp[,i], 15)}
            #grid.draw(tableGrob(tmp,theme=ttheme_minimal()))
            jpeg(file=file,width = 1400) 
            grid.draw(tableGrob(tmp,theme=ttheme_minimal()))
            dev.off()})
    
    #BOISSON 1
    output$B1tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Boissons/3thesglaces.jpg')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$B1 <- renderUI({
        tags$a(imageOutput("B1tmp"),href="https://www.youtube.com/watch?v=FF9CYxx7uaA&feature=youtu.be")})
    
    #BOISSON 2
    output$B2tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Boissons/bluelagoon.jpg')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$B2 <- renderUI({
        tags$a(imageOutput("B2tmp"),href="https://www.youtube.com/watch?v=gzHotlHULAs&feature=youtu.be")})
    
    #BOISSON 3
    output$B3tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Boissons/cosmo.jpg')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$B3 <- renderUI({
        tags$a(imageOutput("B3tmp"),href="https://www.youtube.com/watch?v=gzHotlHULAs&feature=youtu.be")})
    
    #BOISSON 4
    output$B4tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Boissons/rhumcitron.jpg')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$B4 <- renderUI({
        tags$a(imageOutput("B4tmp"))})
    
    #SALE
    output$S1tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/blinis.jpg')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$S1 <- renderUI({
        tags$a(imageOutput("S1tmp"),href="https://www.youtube.com/watch?v=FdqCe1tWc20&feature=youtu.be")})
    
    output$S2tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/boeufsauce.jpg')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$S2 <- renderUI({
        tags$a(imageOutput("S2tmp"),href="https://www.youtube.com/watch?v=cHpt8JEc5OQ&feature=youtu.be")})
    
    output$S3tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/boulette.jpg')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$S3 <- renderUI({
        tags$a(imageOutput("S3tmp"),href="https://www.youtube.com/watch?v=j1chpN3pevg&feature=youtu.be")})
    
    output$S4tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/brochette froide.jpg')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$S4 <- renderUI({
        tags$a(imageOutput("S4tmp"),href="https://www.youtube.com/watch?v=gJKr1kdmAG4&feature=youtu.be")})
    
    output$S5tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/brochette poulet.jpg')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$S5 <- renderUI({
        tags$a(imageOutput("S5tmp"))})

    output$S6tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/burger4.jpg')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$S6 <- renderUI({
        tags$a(imageOutput("S6tmp"),href="https://www.youtube.com/watch?v=gJKr1kdmAG4&feature=youtu.be")})
    
    output$S7tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/Bavette.jpg')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$S7 <- renderUI({
        tags$a(imageOutput("S7tmp"),href="https://www.youtube.com/watch?v=PXPrBfbpUqs&feature=youtu.be")})
    
    output$S8tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/Grosburger.jpg')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$S8 <- renderUI({
        tags$a(imageOutput("S8tmp"),href="https://www.youtube.com/watch?v=FS4K9-xjOpE&feature=youtu.be")})
    
    output$S9tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/burgersauce.png')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$S9 <- renderUI({
        tags$a(imageOutput("S9tmp"))})
    
    output$S10tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/caprice.jpg')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$S10 <- renderUI({
        tags$a(imageOutput("S10tmp"))})
    
    output$S11tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/cordonbleu.jpg')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$S11 <- renderUI({
        tags$a(imageOutput("S11tmp"),href="https://www.youtube.com/watch?v=ZPftyrWmTQ8&feature=youtu.be")})
    
    output$S12tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/crackers.jpg')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$S12 <- renderUI({
        tags$a(imageOutput("S12tmp"),href="https://www.youtube.com/watch?v=lpWkyzyaQa8&feature=youtu.be")})
    
    output$S13tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/doigght.jpg')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$S13<- renderUI({
        tags$a(imageOutput("S13tmp"),href="https://www.youtube.com/watch?v=cRjkhoYly5Q&feature=youtu.be")})
    
    output$S14tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/camembert.jpg')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$S14 <- renderUI({
        tags$a(imageOutput("S14tmp"),href="https://www.youtube.com/watch?v=z20Vu0rSiEA&feature=youtu.be")})
    
    output$S15tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/croquemonsieur.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S15 <- renderUI({
        tags$a(imageOutput("S15tmp"),href="https://www.youtube.com/watch?v=RB2JSymWjss&feature=youtu.be")})
    
    output$S16tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/cuillere.jpg')
        list(src = outfile,
             contentType = 'image/png',
             width="100%")}, deleteFile = FALSE)
    output$S16 <- renderUI({
        tags$a(imageOutput("S16tmp"))})
    
    output$S17tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/cabillaud.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S17 <- renderUI({
        tags$a(imageOutput("S17tmp"),href="https://www.youtube.com/watch?v=A09LthX0sCM&feature=youtu.be")})
    
    output$S18tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/crepethon.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S18 <- renderUI({
        tags$a(imageOutput("S18tmp"),href="https://www.youtube.com/watch?v=TEgaChWuUKw&feature=youtu.be")})
    
    output$S19tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/champ.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S19 <- renderUI({
        tags$a(imageOutput("S19tmp"))})
    
    output$S20tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/cheesecakecour.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S20 <- renderUI({
        tags$a(imageOutput("S20tmp"),href="https://www.youtube.com/watch?v=iSOVAYQ7kgg&feature=youtu.be")})
    
    output$S21tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/chips.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S21 <- renderUI({
        tags$a(imageOutput("S21tmp"),href="https://www.youtube.com/watch?v=Sd2QAxtXUq4&feature=youtu.be")})
    
    output$S22tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/cakebleu.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S22 <- renderUI({
        tags$a(imageOutput("S22tmp"),href="https://www.youtube.com/watch?v=Aee80MR0kKM&feature=youtu.be")})
    
    output$S23tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/flanfeta.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S23 <- renderUI({
        tags$a(imageOutput("S23tmp"),href="https://www.youtube.com/watch?v=MwTa9WMj2E4&feature=youtu.be")})
    
    output$S24tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/friteceleri.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S24 <- renderUI({
        tags$a(imageOutput("S24tmp"))})
    
    output$S26tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/galette.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S26 <- renderUI({
        tags$a(imageOutput("S26tmp"))})
    
    output$S27tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/Kitigouter.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S27 <- renderUI({
        tags$a(imageOutput("S27tmp"))})
    
    output$S28tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/hitdof.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S28 <- renderUI({
        tags$a(imageOutput("S28tmp"),href="https://www.youtube.com/watch?v=GeNEs42jrEc&feature=youtu.be")})
    
    output$S29tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/lomo.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S29 <- renderUI({
        tags$a(imageOutput("S29tmp"))})
    
    output$S30tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/muffinsthon.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S30 <- renderUI({
        tags$a(imageOutput("S30tmp"),href="https://www.youtube.com/watch?v=_8vupxrdu7g&feature=youtu.be")})
    
    output$S31tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/miniquiche.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S31 <- renderUI({
        tags$a(imageOutput("S31tmp"))})
    
    output$S32tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/oeufroti.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S32 <- renderUI({
        tags$a(imageOutput("S32tmp"))})
    
    output$S33tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/pates.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S33 <- renderUI({
        tags$a(imageOutput("S33tmp"),href="https://www.youtube.com/watch?v=Dd97MTXToew")})
    
    output$S34tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/pizzaminute.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S34 <- renderUI({
        tags$a(imageOutput("S34tmp"))})
    
    output$S35tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/pizzagorgon.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S35 <- renderUI({
        tags$a(imageOutput("S35tmp"))})
    
    output$S36tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/pain.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S36 <- renderUI({
        tags$a(imageOutput("S36tmp"),href="https://www.youtube.com/watch?v=BysSOHqksUU&feature=youtu.be")})
    
    output$S37tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/painmie.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S37 <- renderUI({
        tags$a(imageOutput("S37tmp"))})
    
    output$S38tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/painviande.png')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S38 <- renderUI({
        tags$a(imageOutput("S38tmp"),href="https://www.youtube.com/watch?v=xGBj4E5PH08&feature=youtu.be")})
    
    output$S39tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/painmozza.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S39 <- renderUI({
        tags$a(imageOutput("S39tmp"),href="https://www.youtube.com/watch?v=QlXmVmk7XQ0&feature=youtu.be")})
    
    output$S40tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/paingrilleail.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S40 <- renderUI({
        tags$a(imageOutput("S40tmp"),href="https://www.youtube.com/watch?v=4z6EgTK_WRc&feature=youtu.be")})
    
    output$S41tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/pizza2ing.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S41 <- renderUI({
        tags$a(imageOutput("S41tmp"),href="https://www.youtube.com/watch?v=48vhSCa_0sE&feature=youtu.be")})
    
    output$S42tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/fonduepoireaux.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S42 <- renderUI({
        tags$a(imageOutput("S42tmp"))})
    
    output$S43tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/frittata.png')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S43 <- renderUI({
        tags$a(imageOutput("S43tmp"),href="https://www.youtube.com/watch?v=lR98dL75t7M&feature=youtu.be")})
    
    output$S44tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/flanfeta.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S44 <- renderUI({
        tags$a(imageOutput("S44tmp"),href="https://www.youtube.com/watch?v=MwTa9WMj2E4&feature=youtu.be")})
    
    output$S45tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/gressin.png')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S45 <- renderUI({
        tags$a(imageOutput("S45tmp"),href="https://www.youtube.com/watch?v=48vhSCa_0sE&feature=youtu.be")})
    
    output$S46tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/pizzaburger.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S46 <- renderUI({
        tags$a(imageOutput("S46tmp"),href="https://www.youtube.com/watch?v=EL4AhZz6FNc&feature=youtu.be")})
    
    output$S47tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/patespouletpesto.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S47 <- renderUI({
        tags$a(imageOutput("S47tmp"),href="https://www.youtube.com/watch?v=zfdizkQ8UyQ&feature=youtu.be")})
    
    output$S48tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/pizzaasperge.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S48 <- renderUI({
        tags$a(imageOutput("S48tmp"),href="https://www.youtube.com/watch?v=etI5OMcMe1Q&feature=youtu.be")})
    
    output$S49tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/pizza4f.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S49 <- renderUI({
        tags$a(imageOutput("S49tmp"))})

    output$S50tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/pizza au fromage.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S50 <- renderUI({
        tags$a(imageOutput("S50tmp"),href="https://www.youtube.com/watch?v=X9UCPrZ6nWw&feature=youtu.be")})
    
    output$S51tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/poivroin farci.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S51 <- renderUI({
        tags$a(imageOutput("S51tmp"),href="https://www.youtube.com/watch?v=0-o4zsj79e4&feature=youtu.be")})
    
    output$S52tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/pizza poulet.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S52 <- renderUI({
        tags$a(imageOutput("S52tmp"),href="https://www.youtube.com/watch?v=MPE-7ZkXMqk&feature=youtu.be")})
    
    output$S53tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/poulet courg.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S53 <- renderUI({
        tags$a(imageOutput("S53tmp"),href="https://www.youtube.com/watch?v=5sGUUT85xYE&feature=youtu.be")})
    
    output$S54tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/pizza pays.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S54 <- renderUI({
        tags$a(imageOutput("S54tmp"))})
    
    output$S55tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/quichepoire.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S55 <- renderUI({
        tags$a(imageOutput("S55tmp"),href="https://www.youtube.com/watch?v=VjXieJWnHqo&feature=youtu.be")})
    
    output$S56tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/risottoci.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S56 <- renderUI({
        tags$a(imageOutput("S56tmp"))})
    
    output$S57tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/risottochou.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S57 <- renderUI({
        tags$a(imageOutput("S57tmp"),href="https://www.youtube.com/watch?v=p7Q5BPZUX4o&feature=youtu.be")})
    
    output$S58tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/saladeendive.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S58 <- renderUI({
        tags$a(imageOutput("S58tmp"),href="https://www.youtube.com/watch?v=md3T5FMZ7xo&feature=youtu.be")})
    
    output$S59tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/saladesauci.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S59 <- renderUI({
        tags$a(imageOutput("S59tmp"))})
    
    output$S60tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/saladethon.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S60 <- renderUI({
        tags$a(imageOutput("S60tmp"),href="https://www.youtube.com/watch?v=gDy0i5LE1Go&feature=youtu.be")})
    
    output$S61tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/saaldecourgmozza.png')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S61 <- renderUI({
        tags$a(imageOutput("S61tmp"))})
    
    output$S62tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/sandthon.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S62 <- renderUI({
        tags$a(imageOutput("S62tmp"),href="https://www.youtube.com/watch?v=BysSOHqksUU")})
    
    output$S63tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/saucebigmac.png')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S63 <- renderUI({
        tags$a(imageOutput("S63tmp"),href="hhttps://www.youtube.com/watch?v=oiLt3U80Eg4&feature=youtu.be")})
    
    output$S64tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/saladetomnoix.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S64 <- renderUI({
        tags$a(imageOutput("S64tmp"))})
    
    output$S65tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/sushi.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S65 <- renderUI({
        tags$a(imageOutput("S65tmp"),href="https://www.youtube.com/watch?v=BP1w_KTcOj8&feature=youtu.be")})
    
    output$S66tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/tacosboeuf.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S66 <- renderUI({
        tags$a(imageOutput("S66tmp"),href="https://www.youtube.com/watch?v=KisuX4puJBA&feature=youtu.be")})
    
    output$S67tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/saladeavoc.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S67 <- renderUI({
        tags$a(imageOutput("S67tmp"),href="https://www.youtube.com/watch?v=QmCyR49At2k&feature=youtu.be")})
    
    output$S68tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/saladeconco.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S68 <- renderUI({
        tags$a(imageOutput("S68tmp"))})
    
    output$S69tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/sushithon.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S69 <- renderUI({
        tags$a(imageOutput("S69tmp"),href="https://www.youtube.com/watch?v=89ppcsZ1UV0&feature=youtu.be")})
    
    output$S70tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/tartepoireauxc.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S70 <- renderUI({
        tags$a(imageOutput("S70tmp"))})
    
    output$S71tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/tartesoleil.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S71 <- renderUI({
        tags$a(imageOutput("S71tmp"),href="https://www.youtube.com/watch?v=oc9SH5JUzUk&feature=youtu.be")})
    
    output$S72tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/tartemaroilles.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S72 <- renderUI({
        tags$a(imageOutput("S72tmp"),href="https://www.youtube.com/watch?v=twqgU-hKSBA&feature=youtu.be")})
    
    output$S73tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/toasthtonavoc.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S73 <- renderUI({
        tags$a(imageOutput("S73tmp"),href="https://www.youtube.com/watch?v=BysSOHqksUU&feature=youtu.be")})
    
    output$S74tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/tartetomatechevre.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S74 <- renderUI({
        tags$a(imageOutput("S74tmp"),href="https://www.youtube.com/watch?v=MFYNxgKO-Q0&feature=youtu.be")})
    
    output$S75tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/tartepotima.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S75 <- renderUI({
        tags$a(imageOutput("S75tmp"),href="https://www.youtube.com/watch?v=5jSpZROo0Qk&feature=youtu.be")})
    
    output$S76tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/tortillas.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S76 <- renderUI({
        tags$a(imageOutput("S76tmp"),href="https://www.youtube.com/watch?v=-AKxbfYYY_Q&feature=youtu.be")})
    
    output$S77tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/toastsaumon.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S77 <- renderUI({
        tags$a(imageOutput("S77tmp"),href="https://www.youtube.com/watch?v=BysSOHqksUU&feature=youtu.be")})
    
    output$S78tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/tartiflettenavet.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S78 <- renderUI({
        tags$a(imageOutput("S78tmp"))})
    
    output$S79tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sale/velouteasperge.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$S79 <- renderUI({
        tags$a(imageOutput("S79tmp"))})
    
    #SUCRE
    output$D1tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/barrecereales.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D1 <- renderUI({
        tags$a(imageOutput("D1tmp"),href="https://www.youtube.com/watch?v=PwZW3GV695I&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=9")})
    
    output$D2tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/brownies.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D2 <- renderUI({
        tags$a(imageOutput("D2tmp"))})
    
    output$D3tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/bucheamande.png')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D3 <- renderUI({
        tags$a(imageOutput("D3tmp"),href="https://www.youtube.com/watch?v=rEjdsi0CGW8&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=4")})
    
    output$D4tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/buchecitron.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D4 <- renderUI({
        tags$a(imageOutput("D4tmp"),href="https://www.youtube.com/watch?v=Lr90TGkDxZI&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=23")})

    output$D5tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/cheesecakebrownies.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D5 <- renderUI({
        tags$a(imageOutput("D5tmp"))})

    output$D6tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/cheesecakecafe.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D6 <- renderUI({
        tags$a(imageOutput("D6tmp"),href="https://www.youtube.com/watch?v=rFtPYwxZ8js&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=6")})
    
    output$D7tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/cheesecakefr.png')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D7 <- renderUI({
        tags$a(imageOutput("D7tmp"),href="https://www.youtube.com/watch?v=rFtPYwxZ8js&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=6")})
    
    output$D8tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/chocoblanc.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D8 <- renderUI({
        tags$a(imageOutput("D8tmp"),href="https://www.youtube.com/watch?v=ojjQjkXHtGc&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=13")})
    
    output$D9tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/chocolatbnoel.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D9 <- renderUI({
        tags$a(imageOutput("D9tmp"),href="https://www.youtube.com/watch?v=UtqrwuaVWJE&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=26")})
    
    output$D10tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/coockieschoco.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D10 <- renderUI({
        tags$a(imageOutput("D10tmp"),href="https://www.youtube.com/watch?v=Idjh9ly8NAw&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=14&t=21s")})
    
    output$D11tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/coockieschocoetcahcaue.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D11 <- renderUI({
        tags$a(imageOutput("D11tmp"),href="https://www.youtube.com/watch?v=y-PMFg7DsLk&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=7")})
    
    output$D12tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/cookiesamande.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D12 <- renderUI({
        tags$a(imageOutput("D12tmp"),href="https://www.youtube.com/watch?v=tN8ABK4eCKo&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=25")})
    
    output$D13tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/cookiesbeurrecacah.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D13 <- renderUI({
        tags$a(imageOutput("D13tmp"))})
    
    output$D14tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/cremebrulee.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D14 <- renderUI({
        tags$a(imageOutput("D14tmp"),href="https://www.youtube.com/watch?v=rpHIpsM4lIk&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=24")})
    
    output$D15tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/cupcake.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D15 <- renderUI({
        tags$a(imageOutput("D15tmp"),href="https://www.youtube.com/watch?v=U6sSZ6Wyej0&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=20")})
    
    output$D16tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/dessertcitrron.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D16 <- renderUI({
        tags$a(imageOutput("D16tmp"))})
    
    output$D17tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/entremetcafe.JPG')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D17 <- renderUI({
        tags$a(imageOutput("D17tmp"),href="https://www.youtube.com/watch?v=geGVymhPoYo&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=5&t=233s")})
    
    output$D18tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/financier.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D18 <- renderUI({
        tags$a(imageOutput("D18tmp"),href="https://www.youtube.com/watch?v=Jn2LR25Rp2Y&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=10")})
    
    output$D19tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/flan.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D19 <- renderUI({
        tags$a(imageOutput("D19tmp"))})
    
    output$D20tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/framboisier.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D20 <- renderUI({
        tags$a(imageOutput("D20tmp"))})
    
    output$D21tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/gateaucafepices.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D21 <- renderUI({
        tags$a(imageOutput("D21tmp"),href="https://www.youtube.com/watch?v=ah7JYwVBt94&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=11")})
    
    output$D22tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/gaufre.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D22 <- renderUI({
        tags$a(imageOutput("D22tmp"),href="https://www.youtube.com/watch?v=vBnMG6v89Ro&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=19&t=10s")})
    
    output$D23tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/kinderdelice.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D23 <- renderUI({
        tags$a(imageOutput("D23tmp"),href="https://www.youtube.com/watch?v=24oZVbs6yDw&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=17&t=14s")})
    
    output$D24tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/kindersurprise.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D24 <- renderUI({
        tags$a(imageOutput("D24tmp"),href="https://www.youtube.com/watch?v=LseZWofK68g&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=12")})
    
    output$D25tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/Madeleine.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D25 <- renderUI({
        tags$a(imageOutput("D25tmp"),href="https://www.youtube.com/watch?v=5uk6hct3oNA&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG")})
    
    output$D26tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/melleuxoceurchoco.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D26 <- renderUI({
        tags$a(imageOutput("D26tmp"),href="https://www.youtube.com/watch?v=NUdYb3zpqf8&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=18&t=7s")})
    
    output$D27tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/moelleux chocolat.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D27 <- renderUI({
        tags$a(imageOutput("D27tmp"),href="https://www.youtube.com/watch?v=YOGr9UM-Xhk&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=21")})
    
    output$D28tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/pancake.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D28 <- renderUI({
        tags$a(imageOutput("D28tmp"),href="https://www.youtube.com/watch?v=vaMVNqIJIpI&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=8&t=1s")})
    
    output$D29tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/pannacotta.png')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D29 <- renderUI({
        tags$a(imageOutput("D29tmp"),href="https://www.youtube.com/watch?v=TNPNta4N36o&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=15&t=6s")})
    
    output$D30tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/sablecanellecoco.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D30 <- renderUI({
        tags$a(imageOutput("D30tmp"))})
    
    output$D31tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/verrinechocolatcitron.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D31 <- renderUI({
        tags$a(imageOutput("D31tmp"),href="https://www.youtube.com/watch?v=qYENCdhogns&list=PL-DhkowfZpSBIn80VkDbmY82J6s4g5TaG&index=16&t=12s")})
    
    output$D32tmp <- renderImage({
        outfile <- normalizePath('www/Recettes/Sucre/zebre.jpg')
        list(src = outfile,contentType = 'image/png',width="100%")}, deleteFile = FALSE)
    output$D32 <- renderUI({
        tags$a(imageOutput("D32tmp"))})
    
    
    })

