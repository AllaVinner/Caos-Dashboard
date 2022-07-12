box::use(
  shiny[bootstrapPage, moduleServer, NS, renderText, tags, textOutput, icon, observeEvent],
  shinydashboard[dashboardPage, dashboardHeader, dashboardSidebar, sidebarMenu, menuItem, tabItems, tabItem, dashboardBody]
)

box::use(
  app/view/page_butterfly
)

#' @export
ui <- function(id) {
  dashboardPage(
    dashboardHeader(title = 'My Dashboard'),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Butterfly", tabName = "butterfly", icon = icon("dove",verify_fa = FALSE),
                selected = TRUE)
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = 'butterfly', page_butterfly$ui('butterfly'))
      )
    )
  )
}


#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    tbl <- page_butterfly$server('butterfly')
    observeEvent(tbl(), print('TBL calced'))
  })
}
