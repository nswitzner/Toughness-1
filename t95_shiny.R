
library(tidyverse)

K_transition_curve <- function(T, t_91) {
  K_transition <- 20 + (10 + 70 * exp(0.019 * (T - t_91)) * (log(1 / (1 - 0.5))) ^
                          0.25) / ((log(2)) ^ (0.25))
  return(K_transition)
}

E = 29.6 #elastic modulus 29.6 Mpsa
nu = 0.3

# Define the upper shelf calculation function
K_upper_shelf <- function(T, use_05) {
  K_upper <- sqrt((4.482 * use_05 ^ 1.28) * E / (1 - nu ^ 2)) # combining the j1c and k_jc equations from API 579
  return(K_upper)
}

data <- tibble(
    seam = c(
      "SMLS",
      "SMLS",
      "ERW",
      "SMLS",
      "ERW",
      "SMLS",
      "SMLS",
      "ERW",
      "ERW",
      "SMLS",
      "SMLS",
      "ERW",
      "SMLS",
      "SMLS",
      "ERW",
      "ERW",
      "ERW",
      "ERW"
    ),
    vintage = c(
      1940,
      2010,
      1950,
      1950,
      1980,
      1970,
      2000,
      1960,
      1940,
      1980,
      1960,
      1930,
      1930,
      1990,
      2010,
      1970,
      1990,
      2000
    ),
    use_05 = c(
      28.3072020969759,
      115.934946842571,
      18.6335925258869,
      27.0143084885225,
      29.3778274754066,
      22.225579151616,
      32.1594179895802,
      24.0299176901099,
      33.3515189714759,
      33.2497044814065,
      26.3269910920978,
      36.3541942811978,
      27.7889270562995,
      28.312871344922,
      36.7486578462051,
      27.9387497282929,
      26.574096583017,
      31.66138
    ),
    use_50 = c(
      45.1253555783714,
      137.487520637994,
      31.5047762814468,
      41.7116513511896,
      50.5789078085668,
      45.4875911532776,
      96.4261096973594,
      38.6024459623636,
      35.5069864579017,
      61.7147949604177,
      37.6089133754893,
      39.3003487443459,
      48.9241089702331,
      63.9686437643993,
      45.5877201096402,
      34.8971916517968,
      58.6595491442993,
      10
    ),
    ys_95 = c(
      64.0791315816653,
      88.957048322294,
      67.9670723462844,
      67.9670723462844,
      77.1279747017296,
      73.4582129241035,
      88.2126618998835,
      75.6542391188466,
      64.0791315816653,
      77.1279747017296,
      75.6542391188466,
      52.4429751979865,
      52.4429751979865,
      82.3012493253432,
      88.957048322294,
      73.4582129241035,
      82.3012493253432,
      10
    ),
    ys_50 = c(
      52.7636102108381,
      75.9375,
      55.6752533032108,
      55.6752533032108,
      60.6802631578947,
      59.8201401869159,
      68.6609375,
      62.52064108252,
      52.7636102108381,
      60.6802631578947,
      62.52064108252,
      41.3069904341428,
      41.3069904341428,
      64.1714285714286,
      75.9375,
      59.8201401869159,
      64.1714285714286,
      10
    ),
    n = c(
      556L,
      24L,
      658L,
      288L,
      96L,
      48L,
      48L,
      920L,
      24L,
      124L,
      270L,
      28L,
      210L,
      44L,
      16L,
      138L,
      62L,
      10L
    ),
    t20_95 = c(
      81.6523982952626,
      30.2473947185066,
      112.610068503364,
      105.599887810897,
      63.557124073732,
      90.9318459885901,
      37.5814740031463,
      84.6897597169562,
      69.970968547936,
      60.39121307539,
      105.545644588776,
      51.1059228101688,
      62.166393387543,
      62.9280201624579,
      49.3250246269559,
      54.0963028981833,
      69.1282986429068,
      10
    ),
    t20_50 = c(
      35.1105062268133,
      -55.0733745183628,
      56.8731440441309,
      48.3260547284776,
      28.3681254573731,
      31.2673314294633,
      -25.3490376299079,
      21.1617236842195,
      52.1637103790835,
      20.0556807904256,
      62.8539049906021,
      35.6905158804334,
      14.433801637885,
      0.386882612805268,
      -13.9288650759036,
      20.0731791359022,
      5.24656606830614,
      10
    ),
    t91_50 = c(
      -20.0870488355649,
      -106.1355069669,
      17.3909098637278,
      -1.46312396909073,
      -21.8386694155982,
      -16.8896297310987,
      -79.8027331722256,
      -19.0079471209543,
      4.9321114604687,
      -34.8852094956489,
      23.5923629450843,
      -26.9593542235684,
      -54.8580304863242,
      -51.7124668541387,
      -45.5340665525189,
      -19.2305907215083,
      -44.9752592801573,
      -45.254663
    ),
    t91_95 = c(
      55.5918878861395,
      -5.59823387594037,
      114.894682594571,
      85.7911507361067,
      49.2406203134789,
      87.3679431109188,
      30.7855471245087,
      78.9066481875193,
      36.8202216765856,
      40.8147182049825,
      94.9442502619402,
      2.67282369854178,
      24.9841495250922,
      55.6438113932183,
      38.1434841900096,
      38.3231760739529,
      64.9107936366814,
      51.527139
    )
  )


# Define the UI
ui <- fluidPage(
  titlePanel(HTML("Fracture Toughness Calculator<br>Based on the Master Curve")),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataTable", "Select Seam Type:", choices = c("ERW", "SMLS")),
      numericInput("vintage", "Enter Vintage (Year):", value = 1950, min = 1930, max = 2010, step = 10),
      numericInput("temperature", "Enter Temperature of Interest (°F):", value = 50, min = 0, max = 100, step = 1),
      actionButton("calculate", "Calculate Toughness")
    ),
    mainPanel(
      h4("Toughness Calculation Results"),
      verbatimTextOutput("result"),
      plotOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  observeEvent(
    input$calculate,
    {
      decade = input$vintage %/% 10 * 10
      selected_data <- data %>%
        filter(seam == input$dataTable,
               vintage == decade |
               vintage == decade + 10) #performs the modulus 10 division: 1951 = 195 * 10 = 1950; # selects the decade above and below the input year
      if (nrow(selected_data) == 0) {
        output$result <- renderText("No data available for the selected seam type and vintage.")
        return()
      }
    
    use_05 <- approx(x = c(selected_data$vintage[1], selected_data$vintage[2]), y = c(selected_data$use_05[1], selected_data$use_05[2]), xout = input$vintage)$y
    t91_50 <- approx(x = c(selected_data$vintage[1], selected_data$vintage[2]), y = c(selected_data$t91_50[1], selected_data$t91_50[2]), xout = input$vintage)$y
    t91_95 <- approx(x = c(selected_data$vintage[1], selected_data$vintage[2]), y = c(selected_data$t91_95[1], selected_data$t91_95[2]), xout = input$vintage)$y
    temp <- input$temperature
    K_05 <- K_upper_shelf(temp, use_05)
    K_50 <- K_transition_curve(temp, t91_50)
    K_95 <- K_transition_curve(temp, t91_95)
    output$result <- renderText({
      paste0("For the selected seam type (", input$dataTable, ") and vintage (", input$vintage, "):\n",
             "Upper Shelf Lower 5%: ", round(K_05, 2), " ksi√in\n",
             "Transition Median 50%: ", round(K_50, 2), " ksi√in\n",
             "Transition Lower 5%: ", round(K_95, 2), " ksi√in")
    })
    plot_data <- data.frame(
      Temperature = seq(-100, 300, by = 5)
    )
    plot_data$toughness_upper_curve <- K_upper_shelf(plot_data$Temperature, use_05)
    plot_data$toughness_50_curve <- K_transition_curve(plot_data$Temperature, t91_50)
    plot_data$toughness_95_curve <- K_transition_curve(plot_data$Temperature, t91_95)
    plot_data$line_type_upper <- ifelse(plot_data$toughness_upper_curve > plot_data$toughness_95_curve, "dotted", "solid")
    plot_data$line_type_95 <- ifelse(plot_data$toughness_95_curve > plot_data$toughness_upper_curve, "dotted", "solid")
    output$plot <- renderPlot({
      ggplot(plot_data, aes(x = Temperature)) +
        geom_line(aes(y = toughness_upper_curve, linetype = line_type_upper), color = "blue", size = 1) +
        geom_line(aes(y = toughness_50_curve), color = "red", size = 1, linetype = "dotted") +
        geom_line(aes(y = toughness_95_curve, linetype = line_type_95), color = "green", size = 1) +
        scale_linetype_manual(values = c("solid" = "solid", "dotted" = "dotted")) +
        labs(title = "Fracture Toughness vs Temperature",
             x = "Temperature (°F)",
             y = "Fracture Toughness (ksi√in)",
             linetype = "Projection") +
        theme_minimal() +
        ylim(0, K_05 + 50) +
        xlim(-100,150)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)