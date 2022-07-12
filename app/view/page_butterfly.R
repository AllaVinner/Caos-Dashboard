box::use(
  shiny[fluidRow, column, withMathJax, reactive, req, debounce, NS, p, moduleServer],
  shinydashboard[box],
  shiny.fluent[Slider.shinyInput],
  plotly[plotlyOutput, renderPlotly, plot_ly],
  deSolve[ode],
  tibble[tibble],
  dplyr[`%>%`]
)



butterfly_formula <-"$$
 \\begin{align*}
\\frac{\\mathrm{d}X}{\\mathrm{d} x} &= aX+YZ \\\\
\\\\
\\frac{\\mathrm{d}Y}{\\mathrm{d} x} &= b(Y-Z) \\\\
\\\\
\\frac{\\mathrm{d}Z}{\\mathrm{d} x} &= -XY+cY-Z \\\\
\\end{align*}
 $$"

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(plotlyOutput(ns('out'), height = "800px"), width = 7),
    column(5,
           box(width = 12,
               column(6,
                      withMathJax(butterfly_formula)
               ),
               column(6, p('Some other text that I want to  print'))
           ),
           box(
             title = 'Equation Parameters',
             Slider.shinyInput(ns('a'), label = 'a', value=-8/3, min=-5, max=0., step = 0.01),
             Slider.shinyInput(ns('b'), label='b', value=-10, min=-15., max=0., step = 0.01),
             Slider.shinyInput(ns('c'), label='c', value=28, min=10., max=30., step = 0.01),
           ),
           box(
             title = 'Initial Conditions',
             Slider.shinyInput(ns('x'), label = 'x', value=1, min=-1, max=5., step = 0.01),
             Slider.shinyInput(ns('y'), label='y', value=1., min=-1, max=5., step = 0.01),
             Slider.shinyInput(ns('z'), label='z', value=1, min=-1, max=5., step = 0.01),
           ),
           box(
             title = 'Precision',
             Slider.shinyInput(ns('T'), label='T', value=50, min=10, max=100.),
             Slider.shinyInput(ns('log_dt'), label='log dt', value=-2, min=-4, max=0, step = 0.2)
           )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    parameters <- reactive(c(a = input$a,
                             b = input$b,
                             c = input$c))
    
    state <- reactive({
      print('State changed')
      c(X = input$x,
        Y = input$y,
        Z = input$z)
      })
    times <- reactive({
      req(input$T)
      req(input$log_dt)
      seq(0,input$T, by=10^input$log_dt)
    })
    
    tbl <- reactive({
      req(state())
      req(times())
      req(parameters())
      out <- ode(y = state(), times = times(), func = lorenz, parms = parameters())
      tibble(t = out[,1], x = out[,2], y = out[,3], z = out[,4], frame = 1:length(out[,1]) )
    })  %>% debounce(100)
    
    output$out <- renderPlotly({
      print('plotly rendered')
      plot_ly(tbl(), x = ~x, y = ~y, z = ~z, color = ~t, type = 'scatter3d', mode = 'lines',
              opacity = 1, line = list(width = 6))
    })
    
    tbl
  })
}


