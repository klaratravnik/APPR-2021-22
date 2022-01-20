library(shiny)

shinyServer(function(input, output) {
  
  output$graf <- renderPlot({
    narisi_zem(input$leto1,input$vrsta.zemljisca1)
  })
})



narisi_zem = function(leto1, vrsta.zemljisca1){
    zem = tm_shape(merge(zemljevid.regije, zemljisca.za.zemljevid %>% filter(regija != "SLOVENIJA", leto == leto1, vrsta.zemljisca == vrsta.zemljisca1), by.x = "NAME_1", by.y = "regija")) + 
      tm_polygons("povrsina", style = "pretty", palette="Blues", title = vrsta.zemljisca1)
    print(zem)
}


