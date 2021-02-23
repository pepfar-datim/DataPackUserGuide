
pacman::p_load(shiny,shinyjs,shinyWidgets,magrittr,dplyr,futile.logger)

shinyServer(function(input, output, session) {

  observeEvent(input$login_button,
               {

                 tryCatch(  {  datimutils::loginToDATIM(base_url = Sys.getenv("BASE_URL"),
                                                        username = input$user_name,
                                                        password = input$password) },
                            #This function throws an error if the login is not successful
                            error=function(e) {
                              sendSweetAlert(
                                session,
                                title = "Login failed",
                                text = "Please check your username/password!",
                                type = "error")
                              flog.info(paste0("User ", input$user_name, " login failed."), name = "datapack")
                            } )

                 if ( exists("d2_default_session"))  {

                   user_input$authenticated<-TRUE
                   user_input$d2_session<-d2_default_session$clone()
                   flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged in."), name = "datapack")

                 }

               })


  observeEvent(input$logout,{
    flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
    ready$ok <- FALSE
    user_input$authenticated<-FALSE
    user_input$d2_session<-NULL
    gc()

  } )

  
  output$ui <- renderUI({
    
    if (user_input$authenticated == FALSE) {
      ##### UI code for login page
      fluidPage(
        fluidRow(
          column(width = 2, offset = 5,
                 br(), br(), br(), br(),
                 uiOutput("uiLogin"),
                 uiOutput("pass")
          )
        )
      )
    } else {
      
      addResourcePath("www","www")

      test <- tags$iframe(src = ("www/index.html"),
                          frameborder="0", style="overflow:hidden;height:100vh;width:100%")
      test

}
  })

  user_input <- reactiveValues(authenticated = FALSE,
                               status = "",
                               d2_session = NULL)

  # password entry UI componenets:
  #   username and password text fields, login button
  output$uiLogin <- renderUI({

    wellPanel(fluidRow(
      img(src='images/US-PEPFAR-Logo.png', width = "175px",
          style="display: block; margin-left: auto; margin-right: auto;"),
      h4("Welcome to the DataPack User Guide. Please login with your DATIM credentials:")
    ),
    fluidRow(
      textInput("user_name", "Username: ",width = "600px"),
      passwordInput("password", "Password:",width = "600px"),
      actionButton("login_button", "Log in!")
    ))
  })

})
