# Load required packages
library(shiny)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(mapcan)

# Load the agreement data and create a data frame
grants <- read.csv("grants.csv")

# Select relevant columns
grants <- grants %>%
  select(recipient_legal_name,
         recipient_province,
         recipient_city, 
         recipient_postal_code,
         prog_name_en,
         agreement_type,
         agreement_title_en,
         agreement_value,
         agreement_start_date,
         agreement_end_date,
         owner_org_title)

agreement_data <- grants %>%
  select(prog_name_en,
         agreement_type,
         agreement_value,
         recipient_province)
agreement_data <- agreement_data %>%
  mutate(agreement_type = case_when(agreement_type == "G" | agreement_type == "G " | agreement_type == "GRANT" ~ "Grant",
                                    agreement_type == "C" | agreement_type == "C" | agreement_type == "CONTRIBUTION" ~ "Contribution",
                                    1 == 1 ~ agreement_type))
agreement_data <- agreement_data %>%
  mutate(recipient_province = case_when(recipient_province == "PEI" ~ "PE",
                                        recipient_province == "NL " ~ "NL",
                                        recipient_province == "AB " ~ "AB",
                                        recipient_province == "SK " ~ "SK",
                                        recipient_province == "ON " ~ "ON",
                                        1 == 1 ~ recipient_province))
agreement_df <- data.frame(prog_name_en = agreement_data$prog_name_en,
                           agreement_type = agreement_data$agreement_type,
                           agreement_value = agreement_data$agreement_value,
                           recipient_province = agreement_data$recipient_province)

# Define the Shiny app
ui <- shinyUI(fluidPage(
  
  # Add a title
  titlePanel("Agreement Data Dashboard"),
  
  # Use tabs to create two tabs
  tabsetPanel(
    
    # First tab
    tabPanel("Bar Chart",
             # Create the sidebar with the dropdown menus
             sidebarLayout(
               sidebarPanel(
                 # Dropdown for prog_name_en, with default values
                 selectInput("prog_name_en", "Program Name:",
                             unique(agreement_df$prog_name_en),
                             multiple = TRUE,
                             selected = c("Advancing Accessibility Standards Research",
                                          "(CPAF) Crime Prevention Action Fund")),
                 # Dropdown for agreement_type, with default values
                 selectInput("agreement_type", "Agreement Type:",
                             unique(agreement_df$agreement_type),
                             multiple = TRUE,
                             selected = c("Grant", "Contribution"))
               ),
               
               # Create the main panel for the bar chart
               mainPanel(
                 plotOutput("agreement_chart")
               )
             )
    ),
    
    # Second tab
    tabPanel("Map",
             sidebarLayout(
               sidebarPanel(
                 selectInput("funding_type",
                             label = "Funding type:",
                             choices = c("Grant", "Contribution", "Total transfer payments"))
               ),
               # Create the main panel for the map
               mainPanel(
                 plotOutput("agreement_map")
               )
             )
    )
  )
))

# Define the server for the Shiny app
server <- shinyServer(function(input, output) {
  
  # Filter the agreement data based on the selected prog_name_en and agreement_type
  filtered_data <- reactive({
    agreement_df %>%
      filter(prog_name_en %in% input$prog_name_en,
             agreement_type %in% input$agreement_type)
  })
  
  # Create the bar chart
  output$agreement_chart <- renderPlot({
    ggplot(filtered_data(), aes(x = prog_name_en, y = agreement_value, fill = agreement_type)) +
      geom_col(position = "dodge") +
      labs(title = "Agreement Value by Program Name and Agreement Type",
           x = "Program Name",
           y = "Agreement Value",
           fill = "Agreement Type") +
      # Format the y-axis labels as dollar amounts
      scale_y_continuous(labels = dollar) +
      # Add a legend
      scale_fill_discrete(guide = guide_legend(title = NULL)) +
      # Use a minimalistic theme
      theme_minimal() +
      # Use a color palette from the RColorBrewer package
      scale_fill_brewer(type = "seq", palette = "Blues")
  })
  
  # Create the choropleth map
  temp <- mapcan(boundaries = province,
                 type = standard) %>%
    inner_join((agreement_df %>% 
                  filter(agreement_type == "Grant") %>%
                  group_by(recipient_province) %>%
                  summarise(total_grant = sum(agreement_value))),
               by = c("pr_alpha" = "recipient_province")) %>%
    inner_join((agreement_df %>% 
                  filter(agreement_type == "Contribution") %>%
                  group_by(recipient_province) %>%
                  summarise(total_contribution = sum(agreement_value))),
               by = c("pr_alpha" = "recipient_province")) %>%
    inner_join((agreement_df %>% 
                  group_by(recipient_province) %>%
                  summarise(total_transfer_payment = sum(agreement_value))),
               by = c("pr_alpha" = "recipient_province"))
  var_to_use <- reactive({
    case_when(input$funding_type == "Grant" ~ "total_grant",
              input$funding_type == "Contribution" ~ "total_contribution",
              input$funding_type == "Total transfer payments" ~ "total_transfer_payments",
              1 == 1 ~ "total_transfer_payments")
  })
    
  output$agreement_map <- renderPlot({
    temp %>% 
    ggplot(aes(x = long, y = lat, group = group, fill = total_grant)) +
      labs(title = "Agreement Value by Province",
           x = NULL,
           y = NULL,
           fill = "Agreement Value") +
      geom_polygon() +
      coord_fixed() +
      theme_mapcan() +
      scale_fill_viridis_c(name = "Amount received")
  })
  
})

# Run the Shiny app
shinyApp(ui = ui, server = server)
