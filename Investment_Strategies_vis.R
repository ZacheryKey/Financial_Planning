##### Investment_Strategies Visualization #####

# Goal:
# to allow users to view the advantages and disadvantages of choosing certain stocks and investment strategies in order to
# create an investment portfolio that matches their expectations. 
#
# Methods: 
# create graphical and tabular visualizations to demostrate different investment strategies. Focus on dividend growth investments,
# growth investments, value investing and others

library(ggplot2)
library(tidyverse)
library(shiny)
library(scales)
library(riingo)
library(lubridate)
library(quantmod)
library(data.table)
library(dplyr)
library(readr)
library(quantmod)
library(alphavantager)
library(finreportr)
library(reshape2)
library(remotes)
library(trqwe)
library(stringdist)
library(xlsx)
library(XML)
library(stringr)
library(rvest)
library(plyr)
library(tibble)
library(stringi)
library(rlang)
library(tibble)


# require(devtools)
# install_github('ramnathv/rCharts')
# ?install_github
# @TODO: Adjust Div model to include col that has 4% of total investment value 
# setwd("C:/Users/YOUR_REPO_HERE")
# ex <- div_growth_invest(50, 50000, 25000, .025, .1, 1.02, .03, 1.02, 15, TRUE)



### ---------------------------------------- Dividend Growth Investments ---------------------------------------------###

# CONSTANT SHARE PRICE, CONSTANT DIVIDEND GROWTH RATE, CONSTANT DRIP, LOW VOLATILITY, Price averaging investment function
# given a number of years, the inflation rate, yearly income, percentage of income invested, the expected annual growth of a dividend, 
# the expected annual yield of a dividend, the expected annual growth of the stock, and the current share price of the stock

div_growth_invest <- function(years, current_income, retire_income, inflation_rate, pct_invested, growth_div, div_yield, growth_stock, crnt_stock_price, adjust_income){
  
  # Calculate your monthly income
  monthly_income = current_income/12 
  
  # Amount in dollars, which we can invest monthly to achieve our retirement goals 
  monthly_investment = monthly_income*pct_invested
  months_away = 12*years
  
  # Calculate Growth and Returns #
  crnt_stock_price_time = rep(crnt_stock_price,years)   # Current price of stock 
  div_price_time = rep(div_yield*crnt_stock_price, years) # Current dividend price
  monthly_investment_array = rep(monthly_investment, years) # Salary adjusted for inflation
  
  # Grow the share price tree AND dividend tree: p1 = initial dividend, p2 = p1*growth rate, p3 = p2*growth rate
  for(k in 2:years){
    crnt_stock_price_time[k] = crnt_stock_price_time[k-1]*growth_stock # Grow the share price at the share growth rate
    div_price_time[k] = div_price_time[k-1]*growth_div                 # Grow the dividend at the div growth rate 
    if(adjust_income){                                                 # Grow salary according to inflation if user specifies true
      monthly_investment_array[k] = monthly_investment_array[k-1]*(inflation_rate+1)
    } 
  }
  
  # Place into a data frame 
  stock_worth_df <- data.frame(seq(0,years-1,1),as.numeric(crnt_stock_price_time),as.numeric(div_price_time), as.numeric(monthly_investment_array))
  colnames(stock_worth_df)<-c("Years","Share_Value","Dividends_Paid","Monthly_Investment")
  
  # Take each of the rows in the data frame and create 11 identical rows to represent each of the months 
  stock_worth_df <- stock_worth_df[rep(seq_len(nrow(stock_worth_df)), each = 12), ]
  stock_worth_df$Dividends_Paid<-stock_worth_df$Dividends_Paid/12 # Divide yearly dividends by 12 to get monthly payout
  stock_worth_df$Months<-seq(0,months_away-1,1) # Add a column to display months 
  
  # Add a column of number of shares aquired assuming x% monthly income investment with reinvested dividends 
  stock_worth_df$Shares <- rep(monthly_investment/crnt_stock_price, nrow(stock_worth_df))
  
  for(j in 2:nrow(stock_worth_df)){
    stock_worth_df$Shares[j] = ((stock_worth_df$Monthly_Investment[j]/stock_worth_df$Share_Value[j])+stock_worth_df$Shares[j-1]) + ((stock_worth_df$Dividends_Paid[j-1]*stock_worth_df$Shares[j-1])/stock_worth_df$Share_Value[j])
  }
  
  # Add a monthly income column 
  stock_worth_df$monthly_income <- stock_worth_df$Dividends_Paid*stock_worth_df$Shares
  
  # Add a column to display the total value of the stock you have invested
  stock_worth_df$investment_total_value <- stock_worth_df$Share_Value*stock_worth_df$Shares
  
  # Add a column for the retirement income goal that you have set out to make
  for(i in 1:nrow(stock_worth_df)){
    stock_worth_df$monthly_income_goal[i] = get_income_goal(stock_worth_df[i,which(colnames(stock_worth_df)=="Years")], retire_income, inflation_rate)
  }
  
  stock_worth_df$income_invested[1] = stock_worth_df$Monthly_Investment[1]
  # Add a column for no interest savings for total amount of money you have spent on investments
  for(i in 2:nrow(stock_worth_df)){
    stock_worth_df$income_invested[i] = stock_worth_df$Monthly_Investment[i] + stock_worth_df$income_invested[i-1]
  }
  
  # Add a column with 4% of total investment value 
  stock_worth_df$four_percent_mnthly <- (stock_worth_df$investment_total_value*.04)/12
  
  return(stock_worth_df)
}

# Use current monthly income and percent investment to get future monthly income goal in terms of present day dollars 
get_income_goal <- function(years, retire_income, inflation_rate){
  # Yearly cost of standard of living in future dollars 
  Lifestyle_yearly_cost_future = present_value(retire_income, years, inflation_rate)
  # Per month cost of current lifestyle in future dollars
  Lifestyle_monthly_cost_future = Lifestyle_yearly_cost_future/12 # This is our goal to achieve in monthly dividends!!
  return(Lifestyle_monthly_cost_future)
}

# Given a current income x, inflation rate r and number of years n, returns x in terms of dollars in n years
present_value <- function(x,n,r){
  return(x/((1-r)^n))
}

# Returns the number of years necessary to meet your retirement goals using the current model you have
get_years <- function(div_model){
  for(i in 1:nrow(div_model)){
    total_val <- div_model[i,which(colnames(div_model)=="monthly_income")] + div_model[i, which(colnames(div_model)=="four_percent_mnthly")]
    if(div_model[i,which(colnames(div_model)=="monthly_income_goal")] < total_val){
      break;
    }
  }
  return(div_model[i, which(colnames(div_model)=="Years")])
}

plot_divs <- function(div_growth_model1){
  ex <- div_growth_model1[,which(colnames(div_growth_model1) %in% c("Months","four_percent_mnthly","monthly_income","monthly_income_goal"))]
  data <- rbind(data.frame(select(ex, "Months", "monthly_income"), "div. income") %>% set_colnames(c("month","val","group")), 
               data.frame(select(ex, "Months", "four_percent_mnthly"), "liq. income") %>% set_colnames(c("month","val","group")))
  data$group <- factor(data$group, levels = c("div. income","liq. income"))
  
  income_data <- ggplot() + 
    geom_area(data = data, aes(x = month, y = val, fill = group, color = group)) +
    geom_line(data = ex, aes(x = Months, y = monthly_income_goal, color = 'income goal'), size = 1.25) + 
    scale_x_continuous(limits = c(0, 600), breaks = c(0,120,240,360,480,600),labels = c(0,10,20,30,40,50)) + 
    scale_y_continuous(n.breaks = 10, labels = comma) + 
    scale_color_manual(values = c("div. income" = "#7ea3c5", "liq. income" = "#51c6df", "income goal" = "red")) + 
    scale_fill_manual(values = c("div. income" = "#7ea3c5", "liq. income" = "#51c6df"))
  
  return(
    income_data + 
      labs(color = "Legend", title = "Investment Income growth ") + 
      xlab("Years from Initial Investment") + 
      ylab("Monthly Income (USD)") + 
      guides(fill = FALSE)
  )
}

# And plot your total investment value vs. dollars invested 
plot_shares <- function(div_growth_model1){
  total_investment_graph <- ggplot(data = div_growth_model1, mapping = aes(x = Years)) + geom_line(mapping = aes(y = investment_total_value, color = "Investment Value"), size = 1.25) + geom_line(mapping = aes(y = income_invested, color = "Income Invested"), size = 1.25)+
    labs(title = "Total Portfolio Value",x = "Years from Initial Investment", y = "Total Amount invested (USD)") +
    scale_color_manual(name = "Legend", values = c("Investment Value" = "#dbabd9", "Income Invested" = "#ab37a7")) + 
    scale_x_continuous(limits = c(0, 50)) +
    scale_y_continuous(n.breaks = 10, labels = comma)
  return(total_investment_graph)
}

# Plot the type of investment vs risk level in a bar chart
plot_strat <-function(data_vals){
  strat_plot <- ggplot() + geom_bar(data = data_vals, mapping = aes(x = reorder(investment_types, rsk_vec), y = rsk_vec, group = rsk_vec, fill = color_code), stat = "identity") + 
    xlab(" ") + ylab("Level of Risk") + ggtitle("Risk Level by Investment Strategy") + 
    scale_y_continuous(breaks = c(1,2,3,4,5), labels = c("Low","Mid-Low","Mid","Mid-High","High")) + 
    scale_fill_manual(values=c("#999999", "#E69F00", "#FFD000")) +
    theme(axis.text.x = #element_blank(),
            element_text(angle = 37.5, hjust = 1),
          legend.position = "none"
          ,plot.background = element_rect(fill = "#f5f5f5")
          ,panel.background = element_rect(fill = "#f5f5f5")
          ,panel.grid.major = element_line(color = "black")
          ,panel.grid.minor = element_line(color = "black")
          ) 
  return(strat_plot)
}

# Create your shiny app User inferface to allow user to play around with the different variables in dividend growth investing 
ui<-fluidPage(
    sidebarPanel(
      titlePanel("Investment Planner"),
        fluidRow(
          column(6,
            wellPanel(
              h4("Welcome!"),
              br(),
              h4("Enter your financial information and retirement goals in the boxes to the right. This information will be used to simulate investment growth."),
              br(),
              h4("Recomended Retirement Income should not exceed current income less yearly investment."), 
              br(),
              h4("Move sliders until retirement goals have been met, indicated by green text coloring. Income goals have been met at the intersection of the red line and the blue area."), 
              br()
            )         
          ),
          column(6,
            wellPanel(
              numericInput("current_income", h4("Yearly Income"), value = 50000, step = 1000, min = 0),
              sliderInput("pct_invested", h4("Percent Income to Invest"), value = 20, step = 1, min = 0, max = 100),
              sliderInput("retire_income", h4("Retirement Income Goal"), value = 40000, step = 1000, min = 0, max = 50000),
              sliderInput("retire_age", h4("Years until Retirement"), value = 30, step = 1, min = 1, max = 50),
              checkboxInput("increase_salary", label = "Adjust Salary for Inflation", value = FALSE),
              br(),
              actionButton("save_info", label = "Save")
            )
          )
        )
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
          tabPanel(title = "Investment Simulator", 
              wellPanel(
                  fluidRow(
                      wellPanel(style = "background: #FFFFFF",
                        fluidRow(
                          div(htmlOutput("title_tab1")),
                            column(6,
                                  plotOutput("plot_div_invest", width = 575, height = 360),
                                  br(),
                                  br()
                            ),
                            column(6,
                                  plotOutput("plot_total_shares", width = 575, height = 360),
                                  br(),
                                  br()
                            ),
                            column(12,
                                  column(4,sliderInput("stock_growth",h4("Avg. Annual Portfolio Growth Rate"), value = 0.04, min = 0, max = .15, step = .01)),
                                  column(4,sliderInput("dividend_growth",h4("Avg. Annual Dividend Growth Rate"), value = 0.03, min = 0, max = .15, step = .01)),
                                  column(4,sliderInput("dividend_yield",h4("Avg. Annual Dividend Yield"), value = 0.03, min = 0, max = .15, step = .01))
                             ),
                          ),
                        ),
                      wellPanel(
                        fluidRow(
                          splitLayout(
                              htmlOutput("msg"),
                              htmlOutput("portfolio_worth"),
                              htmlOutput("dollars_invested"),
                              htmlOutput("total_monthly_income"),
                              htmlOutput("yrs_until_retire")
                          ),
                          htmlOutput("msg2")
                        )
                      )
                    )
                  )
              ),
            tabPanel(title = "Investing Strategies",
                     wellPanel(
                       div(style="line-height:100%; border: 1px; margin: 1px; padding-top: 1px;", htmlOutput("priority_title")),
                       div(style="display:inline-block; vertical-align: top; padding-left: 10px; width:275px;", htmlOutput("val_menu_txt1")),
                       div(style="display:inline-block; vertical-align: top; padding-left: 5px; width:170px;", selectInput(inputId = "val_menu1", label = NULL, choices = c("high return","high safety margin"))),
                       div(style="display:inline-block; vertical-align: top; padding-left: 5px; width:170px;", htmlOutput("val_menu_txt4")),
                       div(style="display:inline-block; vertical-align: top; padding-left: 5px; width:120px;", selectInput(inputId = "val_menu3", label = NULL, choices = c("short term","long term"))),
                       div(style="line-height:100%; border: 1px; margin: 1px; padding-left: 10px; padding-top: 1px;", htmlOutput("get_strategies")),
                       br(),
                       div(style="line-height:100%; border: 1px; margin: 1px; padding-top: 1px;", htmlOutput("pref")),
                       div(style = "padding-left:10px;",radioButtons(inputId = "strategy_button", label = NULL, choices = c("Penny Stocks","Options"), inline = TRUE)),
                       splitLayout(
                         cellWidths = c("30%","50%","20%"),
                            div(
                                br(),
                                htmlOutput("key_indices")
                                ),
                            div(style = "padding-left:10px;",
                                br(),
                                plotOutput("risk_strat_plot", width = 350, height = 320)),
                            div(
                                br(),
                                htmlOutput("get_return_cat"),
                                htmlOutput("security_lvl"),
                                htmlOutput("get_interest")
                                )
                            
                      ),
                       wellPanel(
                          htmlOutput("detail_strat")
                       )
                     )
              ),
            tabPanel(title = "Selecting Investments"),
            tabPanel(title = "My Investments",
                         wellPanel(h4("Record New Investments")),
                         wellPanel(h4("Check Existing Investments"))
              )
    )
  )
)


server <- function(input,output, session){
  
  ###########################
  ###### Sidebar Panel ######
  ###########################
  
  observeEvent(input$current_income,{
    x <- input$current_income
    y <- input$retire_income
    updateSliderInput(session = session, inputId = "retire_income", value = y, min = 0, max = x)
  })
  
  
  #################################
  ###### Investment Simulator #####
  #################################
  
  output$title_tab1 <- renderText({
    paste0("<h3><b>","Tax Free Investment Growth Simulator","</b><h3>")
  })
  
  # Create the investment dataframe in order to generate plots, etc
  generate_model <- reactive({
    model <- div_growth_invest(50, input$current_income, input$retire_income, .025, (input$pct_invested/100), (1+input$dividend_growth), input$dividend_yield, (1+input$stock_growth), 15, input$increase_salary)
    return(model)
  })
  
  # Get your income goal for retirement 
  get_goal <- reactive({
    return(as.character(round(get_income_goal(input$retire_age, input$retire_income, .025),0)))
  })
  
  # Generate monthly income plot 
  output$plot_div_invest <- renderPlot({
    model <- generate_model()
    plot_divs(model) + geom_vline(xintercept = 12*input$retire_age, size = 1, linetype="dashed")
  })
  
  # Generate total portfolio plot 
  output$plot_total_shares <- renderPlot({
    model <- generate_model()
    plot_shares(model) + geom_vline(xintercept = input$retire_age, size = 1, linetype="dashed")
  })
  
  # Generate earnings a month colored text
  output$total_monthly_income <- renderText({
    data <- generate_model()
    total_val <- data[((input$retire_age*12)),which(colnames(data)=="monthly_income")] + data[((input$retire_age*12)),which(colnames(data)=="four_percent_mnthly")]
    earnings <- as.character(format(round(total_val,0),big.mark = ",", scientific = F))
    
    if(total_val >= data[((input$retire_age*12)), which(colnames(data)=="monthly_income_goal")]){
      paste0("<h1><b>","<font color=\"#40e32d\">", "$", earnings,"</b></h1></font>", "<h5><b>","total earnings / month","</b></h5>")
    }else{
      paste0("<h1><b>","<font color=\"#FF0000\">", "$", earnings,"</b></h1></font>", "<h5><b>","total earnings / month","</b></h5>")
    }
  })
  
  # Generate net value colored text
  output$portfolio_worth <- renderText({
    data <- generate_model()
    worth <- as.character(format(round(data[((input$retire_age*12)),which(colnames(data)=="investment_total_value")],0), big.mark = ",", scientific = F))
    paste0("<h1><b>","<font color=\"#40e32d\">", "$", worth,"</b></h1></font>", "<h5><b>"," net value of investments","</b></h5>")
  })

  # Generate total dollars invested color text
  output$dollars_invested <- renderText({
    data <- generate_model()
    invested <- as.character(format(round(data[((input$retire_age*12)),which(colnames(data)=="income_invested")],0), big.mark = ",", scientific = F))
    paste0("<h1><b>","<font color=\"#40e32d\">", "$", invested,"</b></h1></font>", "<h5><b>"," dollars / invested","</b></h5>")
  })
  
  # Generate years until retirement text 
  output$yrs_until_retire <- renderText({
    data <- generate_model()
    years <- as.character(get_years(data))
    if(years>=49){
      years = paste0(">50")
    }
    
    mnthInc = data[((input$retire_age*12)), which(colnames(data)=="monthly_income")] + data[((input$retire_age*12)),which(colnames(data)=="four_percent_mnthly")]

    if(mnthInc >= data[((input$retire_age*12)),which(colnames(data)=="monthly_income_goal")]){
      paste0("<h1><b>","<font color=\"#40e32d\">", years,"</b></h1></font>", "<h5><b>"," yrs. to income goal","</b></h5>")
    }else{
      paste0("<h1><b>","<font color=\"#FF0000\">", years,"</b></h1></font>", "<h5><b>"," yrs. to income goal","</b></h5>")
    }
  })
  
  # Genrate subplot text additional info, disclaimers and such 
  output$msg <- renderText({
    paste("<h2><b>","  Investment", "<br>" ,"Outlook:","</b></h2>")
  })
  
  output$msg2 <- renderText({
    paste("<h5>"," *Calculations performed in terms of present day dollars, valuations calculated over a ", input$retire_age, " yr. period.",
          "Earnings per month calculated assuming 4% yearly liquidation of assets and dividend reinvestment. ", #"<br>",
          "Model Estimations assumes constant inflation rate of 2.5% annually with all numbers above calculated to predict value of investment at time of retirement year goal.", #"<br>",
          "</h5>")
  })
  
  #####################################
  ##### Investment Strategies Tab #####
  #####################################
  
  # Get all of the text under the My investment preferances title
  output$priority_title <- renderText({
    paste0("<h5>","<b>","<u>","My Investment Preferences:","</u>","</b>","</h5>")
  })
  
  # Get Text output evaluating your prefferred method of investing
  output$val_menu_txt1 <- renderText({
    paste0("<h5>","It is important that my investments have a ","</h5>")
  })
  
  output$val_menu_txt2 <- renderText({
    paste0("<h5>"," I plan to take on a(n) ","</h5>")
  })
  
  output$val_menu_txt3 <- renderText({
    paste0("<h5>"," management approach to investing. ","</h5>")
  })
  
  output$val_menu_txt4 <- renderText({
    paste0("<h5>",". I plan on investing for the ","</h5>")
  })
  
  # Set up variables to monitor the status of dropdowns for return/safety and length of term
  high_return <- reactiveVal(TRUE)
  short_term <- reactiveVal(TRUE)
  
  # Update variables as input variables are changed by user 
  observeEvent(input$val_menu3,{
    if(input$val_menu3=="long term"){
      short_term(FALSE)
    }else{
      short_term(TRUE)
    }
  })
  
  observeEvent(input$val_menu1,{
    if(input$val_menu1=="high safety margin"){
      high_return(FALSE)
    }else{
      high_return(TRUE)
    }
  })
  
  output$get_strategies <- renderText({
    if(high_return() & short_term()){
      paste0("<p>","Investors prioritizing","<b>"," high returns","</b>"," and looking for","<b>"," short term","</b>"," investments will need to maintain an", "<b>"," active", "</b>"," management strategy", "</p>",
             "<p>","by committing","<b>"," more time","</b>"," to monitoring their investments and may wish to consider the following","<b>"," very risky","</b>"," investment strategies","</p>",
             "<p>","expecting potentially high returns.","</p>")
    }else if(!high_return() & !short_term()){
      paste0("<p>","Investors prioritizing","<b>"," high safety margins","</b>"," and looking for","<b>"," long term","</b>"," investments can maintain a", "<b>"," passive", "</b>"," management strategy", "</p>",
             "<p>","by committing","<b>"," less time","</b>"," to monitoring their investments and may wish to consider the following","<b>"," somewhat safe","</b>"," investment strategies","</p>",
             "<p>","expecting","<b>"," mid to high returns.","</b>","</p>")
    }else if(high_return() & !short_term()){
      paste0("<p>","Investors prioritizing","<b>"," high returns","</b>"," and looking for","<b>"," long term","</b>"," investments can maintain an", "<b>"," active", "</b>"," management strategy", "</p>",
             "<p>","by committing","<b>"," more time","</b>"," to monitoring their investments and may wish to consider the following","<b>"," somewhat risky","</b>"," investment strategies","</p>",
             "<p>","expecting potentially high returns.","</p>")
    }else{
      paste0("<p>","Investors prioritizing","<b>"," high safety margins","</b>"," and looking for","<b>"," short term","</b>"," investments can maintain a", "<b>"," passive", "</b>"," management strategy", "</p>",
             "<p>","by committing","<b>"," less time","</b>"," to monitoring their investments and may wish to consider the following","<b>"," very safe","</b>"," investment strategies","</p>",
             "<p>","expecting relatively","<b>"," low returns.","</b>","</p>")
    }
  })
  
  # Update radio buttons when either the return or period dropdown menu is altered
  observeEvent(
    {input$val_menu1 
    input$val_menu3},
    
    {if(high_return() & short_term()){
      choice_vec <- c("Options","Day Trading","Penny Stocks")
    }else if(high_return() & !short_term()){
      choice_vec <- c("Value Investing","Growth Investing")
    }else if(!high_return() & short_term()){
      choice_vec <- c("Savings Account","CDs","Money Market Accounts","Short-term Corporate Bonds")
    }else{
      choice_vec <- c("Dividend Growth Investing","Indexing / ETFs","Long-term Treasury Bonds","Blue Chip Stocks")
    }
      updateRadioButtons(session = session, inputId = "strategy_button", label = NULL,  choices = choice_vec, inline = TRUE)
  })
  
  output$pref <- renderText({
    paste0("<h5>","<b>","<u>","Investment Strategies based on your investing preferences:","</u>","</b>","</h5>")
  })
  
  # Get a summary of each investmen strategy from under the barplot
  output$detail_strat <- renderText({
    if(input$strategy_button == "Options"){
      paste0("<h3>","Options Trading","</h3>",
             "<p>","<b>","Description:","</b>"," Options are paid contracts that allow owners to buy or sell an asset at a certain price or by a certain date. ","</p>",
             "<p>","<b>","Benefits: ","</b>"," Buying options can provide investors a 'hedge' against negative market trends on stocks they own by essentially 'freezing' the price of the stock and can reduce the amount of money an investor needs to initially purchase an investment.","</p>",
             "<p>","<b>","Risks: ","</b>"," Because options are contracted, the option buyer must pay a fee to enter the agreement. The options seller carries the risk of losing all of their investment without the power to withdraw from the contract. 
                The limitation on the time the both parties have agreed upon on the contract also reduces the potential earnings an options contract can secure. ","</p>"
             )
    }else if(input$strategy_button == "Day Trading"){
      paste0("<h3>","Day Trading","</h3>",
             "<p>","<b>","Description:","</b>"," Buying and selling an asset in the same day with the expectation of generating immediate returns. ","</p>",
             "<p>","<b>","Benefits: ","</b>"," Day trading provides the highly active investor with immediate income and the highest possible rate of return. Good bullish day traders can expect very high yields when the market is up and good bearish day traders can make money in options when the market declines.","</p>",
             "<p>","<b>","Risks: ","</b>"," Day trading is only for investors who wish to invest great amounts of time and energy in market research and are willing to shoulder substantial risk from the days their investments lose significant value. Day trading is highly speculative by nature and as such is one of the most volatile and risky investment strategies.   ","</p>")
    }else if(input$strategy_button == "Penny Stocks"){
      paste0("<h3>","Penny Stocks","</h3>",
             "<p>","<b>","Description:","</b>"," Investing in low-value stocks (<$5/share) with the expectation of recieving high returns if/when an asset increases considerably in value. ","</p>",
             "<p>","<b>","Benefits: ","</b>"," Penny stocks offer low cost entry into the stock market with the possibility of generating passive returns if a company takes off and starts increasing its market share. Penny Stocks are also very accessible and a very diverse portfolio can be built relatively cheaply. ","</p>",
             "<p>","<b>","Risks: ","</b>"," Buying Penny Stocks shoulders significant risks of losing money as low value stocks are extremely volatile and often unlikely to ever grow significantly in value. Penny stocks are never a good idea for beginning investors and should not be considered without considerable amounts of market research.  ","</p>")
    }else if(input$strategy_button == "Value Investing"){
      paste0("<h3>","Value Investing","</h3>",
             "<p>","<b>","Description:","</b>"," Value Investors look to purchase good stocks at a discounted price in order to recieve a larger return in the future. ","</p>",
             "<p>","<b>","Benefits: ","</b>"," Value investing has the ability to provide investors with a substantial amount of wealth over the long run. Value investing in good quality large companies during a recession will allow you to get stock 'on sale'.","</p>",
             "<p>","<b>","Risks: ","</b>"," The single largest risk with value investing is correctly determining which companies will be able to recover from a financial setback. This means that value investors should be prepared to spend a decent amount of time determining a company's eligibility by their financial statements. ",
             "Investing in a 'value' stock that continues downward can greatly decrease investment value.","</p>")
    }else if(input$strategy_button == "Growth Investing"){
      paste0("<h3>","Growth Investing","</h3>",
             "<p>","<b>","Description:","</b>"," Growth Investors look to purchase stocks from growing companies that are expected to greatly increase in value over the term of investment.","</p>",
             "<p>","<b>","Benefits: ","</b>"," Growth investing can result in high returns on companies that are moving into new market sectors or have introduced new and useful technologies. Investing in a company's IPO (initial public offering, or when a company first decides to sell stock to the public) is an example of growth investing.","</p>",
             "<p>","<b>","Risks: ","</b>"," Risks in growth investing include over-speculation on a company's performance, high volatility, and over-valuation for relatively young organizations. Many Companies may look appealing on paper or in theory, but because growth stocks are composed of younger companies they are more volatile than other more established organizations. Most Growth stocks do not pay dividends as the focus of the compnay is on increasing total company value instead of creating shareholder value.","</p>")
    }else if(input$strategy_button == "Savings Account"){
      paste0("<h3>","Savings Account","</h3>",
             "<p>","<b>","Description:","</b>"," Placing your money in a bank or related finacial institution with relatively low interest rates, minimal risk and high liquidity.","</p>",
             "<p>","<b>","Benefits: ","</b>"," Savings accounts are backed by the FDIC up to 250k and offer a virtually risk free investment. The benefits to having a savings account are high liquidity, FDIC insured money, and lack of external risk.","</p>",
             "<p>","<b>","Risks: ","</b>"," The primary risk of placing money in a savings account is value erosion due to inflation (most banks do not offer an interest rate greater than 2.5%). ","</p>")
    }else if(input$strategy_button == "CDs"){
      paste0("<h3>","CDs","</h3>",
             "<p>","<b>","Description:","</b>"," Certificates of Deposit are fixed-time investments made with banks and credit unions. ","</p>",
             "<p>","<b>","Benefits: ","</b>"," CD's (like savings accounts) are backed by the FDIC up to 250k and offer a virtually risk free investment. In exchange for providing a set amount of money for a fixed term, banks and financial institutions offer higher interest rates for CD's than savings or checking accounts.
                Longer terms of deposit and larger principal investments will recieve greater asset appreciation.","</p>",
             "<p>","<b>","Risks: ","</b>"," The single greatest risk with CDs happens in the event the money is withdrawn prior to the date agreed upon starting the investment. Withdrawing prior to the contract's expiration encurs significant penalties and as such, CDs should never be used to supplement cash or other liquid investments.","</p>")
    }else if(input$strategy_button == "Money Market Accounts"){
      paste0("<h3>","Money Market Accounts","</h3>",
             "<p>","<b>","Description:","</b>"," High interest bearing accounts with checking and debit abilities as well as some restrictions.","</p>",
             "<p>","<b>","Benefits: ","</b>"," Higher interest rates than savings accounts, checking and debit abilities, FDIC protection. ","</p>",
             "<p>","<b>","Risks: ","</b>"," Minimum Balance, Account Fees and Limited number of transfers and payments per month. ","</p>")
    }else if(input$strategy_button == "Short-term Corporate Bonds"){
      paste0("<h3>","Short-term Corporate Bonds","</h3>",
             "<p>","<b>","Description:","</b>"," Bonds issued by companies to fund their investments with maturity of less than 5 years.","</p>",
             "<p>","<b>","Benefits: ","</b>","  High Liquidity, can be bought or sold any time the stock market is open. Higher interest rates than savings, checking or money market accounts. Since the term these bonds are held is so small, there is a very low risk of rising interest rates decreasing the value of the bond.  ","</p>",
             "<p>","<b>","Risks: ","</b>"," Short term corporate bonds have relatively low risk, but are more volatile than any of the other high security low risk short term investments in this section. Also, corporate bonds have a greater risk of default than investing in bonds issued by the government.","</p>")
    }else if(input$strategy_button == "Dividend Growth Investing"){
      paste0("<h3>","Dividend Growth Investing","</h3>",
             "<p>","<b>","Description:","</b>"," Dividend Growth Investments are high div. yield/high div. growth stocks that pay regular dividends to shareholders.","</p>",
             "<p>","<b>","Benefits: ","</b>"," This style of investing will hedge against negative stock market flucations and prevent excessive withdrawal from investment accounts during retirement, thus allowing continued investment growth. Generally speaking, this method
                  exposes investors to owning portions of large high quality businesses with steady growth that assists in compounding wealth over time. The largest benefit to using this method is the creation of future income flow and reducing dependency on speculative performance of the stock.","</p>",
             "<p>","<b>","Risks: ","</b>"," Some high div. growth / yield investments are not always reliable choices and can quickly destroy value if the focus of the business is on providing dividends and not on creating value. It is generally a bad idea to select a stock with dividend payouts over 50% of net income.","</p>")
    }else if(input$strategy_button == "Indexing / ETFs"){
      paste0("<h3>","Indexing / ETFs","</h3>",
             "<p>","<b>","Description:","</b>"," Index Funds / ETFs are investment 'buckets' that contain a large number of related stocks.","</p>",
             "<p>","<b>","Benefits: ","</b>"," Index funds and exchange traded funds (ETFs) are a great way to diversify your investment. Both of these types of investments are also 'self-cleansing' and underperforming stocks are booted out of the fund, automatically preventing large losses.","</p>",
             "<p>","<b>","Risks: ","</b>"," Because some ETF or index funds are comprised of shares in a similar market sector, when a portion of the market loses value, the ETF tracking it will also decrease in value. Some of these investments also encure a small investing fee (<1%) that will reduce the total value of your investment.","</p>")
    }else if(input$strategy_button == "Long-term Treasury Bonds"){
      paste0("<h3>","Long Term Treasury Bonds","</h3>",
             "<p>","<b>","Description:","</b>"," Buying long-term debt from the government with very low risk of default and a low but steady interest rate.","</p>",
             "<p>","<b>","Benefits: ","</b>"," Very low risk of default, high security investments that hedge against deflation. Some bonds providing tax advantages.","</p>",
             "<p>","<b>","Risks: ","</b>"," Buying large bonds in unstable governments have a risk of default. Lower interest rates on treasury bonds present an oppurtunity cost of money that could have been invested at higher rates of return. US Treasury bonds have been decreasing in interest rates since 1980 and the trend is likely to continue.","</p>")
    }else if(input$strategy_button == "Blue Chip Stocks"){
      paste0("<h3>","Blue Chip Stocks","</h3>",
             "<p>","<b>","Description:","</b>"," Invest in large market cap companies with well-established reputations.","</p>",
             "<p>","<b>","Benefits: ","</b>"," Investing in Blue Chip Companies reduces the uncertainty and volatility of your investment. Larger corporations move realtivly slowly, allowing investors more time to add or subtract from their holdings. Growth in these stocks is stable, preventing large flucations. ","</p>",
             "<p>","<b>","Risks: ","</b>"," Blue Chip Companies are incredibly stable in the short term but often face competition from emerging organizations or improving technologies. Consider the fact that the Dow Jones (group of top 30 performing stocks in US) has changed 55 times since 1896.","</p>")
    }else{}
  })
  
  # Create a dataframe with risk, return and color for barchart based on type of investment 
  get_col <- reactive({
    rsk_vec <- c(4,5,5,3,3,.5,1,.5,1.5,2,2,1,2)
    ret_vec <- c(4,5,5,4,4,1,1,1,2,4,4,2,4)
    tme_vec <- c(4,5,5,4,4,1,1,1,1,3,3,1,3)
    investment_types <- c("Options","Day Trading","Penny Stocks","Value Investing","Growth Investing","Savings Account","CDs","Money Market Accounts","Short-term Corporate Bonds","Dividend Growth Investing","Indexing / ETFs","Long-term Treasury Bonds","Blue Chip Stocks") %>% factor()
    return_pct <- c(NA, NA, NA, NA, NA, .001, .004, .0011, .025, .10, .10, .02, .10)
    compare_types <- data.frame(investment_types, return_pct, ret_vec, rsk_vec, tme_vec)
    
    if(high_return() & short_term()){
      Vec_Color <- c(1,1,1,0,0,0,0,0,0,0,0,0,0)
    }else if(!high_return() & !short_term()){
      Vec_Color <- c(0,0,0,0,0,0,0,0,0,1,1,1,1)
    }else if(high_return() & !short_term()){
      Vec_Color <- c(0,0,0,1,1,0,0,0,0,0,0,0,0)
    }else{
      Vec_Color <- c(0,0,0,0,0,1,1,1,1,0,0,0,0)
    }
    
    if(input$strategy_button == "Options"){
      Vec_Color[1] = 2
    }else if(input$strategy_button == "Day Trading"){
      Vec_Color[2] = 2
    }else if(input$strategy_button == "Penny Stocks"){
      Vec_Color[3] = 2
    }else if(input$strategy_button == "Value Investing"){
      Vec_Color[4] = 2
    }else if(input$strategy_button == "Growth Investing"){
      Vec_Color[5] = 2
    }else if(input$strategy_button == "Savings Account"){
      Vec_Color[6] = 2
    }else if(input$strategy_button == "CDs"){
      Vec_Color[7] = 2
    }else if(input$strategy_button == "Money Market Accounts"){
      Vec_Color[8] = 2
    }else if(input$strategy_button == "Short-term Corporate Bonds"){
      Vec_Color[9] = 2
    }else if(input$strategy_button == "Dividend Growth Investing"){
      Vec_Color[10] = 2
    }else if(input$strategy_button == "Indexing / ETFs"){
      Vec_Color[11] = 2
    }else if(input$strategy_button == "Long-term Treasury Bonds"){
      Vec_Color[12] = 2
    }else if(input$strategy_button == "Blue Chip Stocks"){
      Vec_Color[13] = 2
    }else{}
    
    data_frame_strat <- mutate(compare_types, color_code = factor(Vec_Color))
    return(data_frame_strat)
  })
  
  # Plot the barchart colored based on user selection 
  output$risk_strat_plot <- renderPlot({
    plot_strat(get_col())
  })
  
  # Get large yellow font data on risk, management and return pct 
  output$get_interest <- renderText({
    compare_types <- get_col()
    interest_rate <- compare_types[which(compare_types$investment_types == input$strategy_button),2]
    
    if(!is.na(interest_rate)){
        paste0("<h2><b>","<font color=\"#FFD000\">", 100*interest_rate, "%", "</font>","</b></h2>", 
                  "<h5>", "avg. annual return", "</h5>")
    }else{
      paste0("<h2><b>","<font color=\"#FFD000\">", ">15%", "</font>","</b></h2>", 
             "<h5>", "expected annual return", "</h5>")
    }
  })
  
  output$get_return_cat <- renderText({
    compare_types <- get_col()
    return_rate <- as.numeric(compare_types[which(compare_types$investment_types == input$strategy_button),3])
    if(0<return_rate & return_rate<=1){
         paste0("<h2><b>","<font color=\"#FFD000\">", "Low", "</font>","</b></h2>", "<h5>", "invest. management", "</h5>")
    }else if(1<return_rate & return_rate<=2){
        paste0("<h2><b>","<font color=\"#FFD000\">", "Low-Mid", "</font>","</b></h2>", "<h5>", "invest. management", "</h5>")
    }else if(2<return_rate & return_rate<=3){
        paste0("<h2><b>","<font color=\"#FFD000\">", "Mid-Range", "</font>","</b></h2>", "<h5>", "invest. management", "</h5>")
    }else if(3<return_rate & return_rate<=4){
        paste0("<h2><b>","<font color=\"#FFD000\">", "Mid-High", "</font>","</b></h2>", "<h5>", "invest. management", "</h5>")
    }else{
        paste0("<h2><b>","<font color=\"#FFD000\">", "High", "</font>","</b></h2>", "<h5>", "invest. management", "</h5>")
    }
  })
  
  output$security_lvl <- renderText({
    compare_types <- get_col()
    risk_rate <- as.numeric(compare_types[which(compare_types$investment_types == input$strategy_button),4])
    if(0<risk_rate & risk_rate<=1){
      paste0("<h2><b>","<font color=\"#FFD000\">", "Low", "</font>","</b></h2>", "<h5>", "invest. risk level", "</h5>")
    }else if(1<risk_rate & risk_rate<=2){
      paste0("<h2><b>","<font color=\"#FFD000\">", "Low-Mid", "</font>","</b></h2>", "<h5>", "invest. risk level", "</h5>")
    }else if(2<risk_rate & risk_rate<=3){
      paste0("<h2><b>","<font color=\"#FFD000\">", "Mid-Range", "</font>","</b></h2>", "<h5>", "invest. risk level", "</h5>")
    }else if(3<risk_rate & risk_rate<=4){
      paste0("<h2><b>","<font color=\"#FFD000\">", "Mid-High", "</font>","</b></h2>", "<h5>", "invest. risk level", "</h5>")
    }else{
      paste0("<h2><b>","<font color=\"#FFD000\">", "High", "</font>","</b></h2>", "<h5>", "invest. risk  level", "</h5>")
    }
  })
  
  # Tell Users about investment indices 
  output$intro_indices <- renderText({
    paste0("<h3>","Key Metrics to Consider:","</h3>")
  })
  
  # Key indices to consider based on user selected investment model 
  output$key_indices <- renderText({
    if(input$strategy_button == "Options"){
      
    }else if(input$strategy_button == "Day Trading"){
      
    }else if(input$strategy_button == "Penny Stocks"){
      
    }else if(input$strategy_button == "Value Investing"){
      
    }else if(input$strategy_button == "Growth Investing"){
      
    }else if(input$strategy_button == "Savings Account"){
      
    }else if(input$strategy_button == "CDs"){
      
    }else if(input$strategy_button == "Money Market Accounts"){
      
    }else if(input$strategy_button == "Short-term Corporate Bonds"){
      
    }else if(input$strategy_button == "Dividend Growth Investing"){
      paste0("<h4><b>", "Dividend Yield: >3%", "</b></h4>", 
             "<p>","div.price / share price ","</p>",
             "<h4><b>", "Dividend Growth: positive", "</b></h4>", 
             "<p>","div.price / time","</p>",
             "<h4><b>", "P/E Ratio: >20","</b></h4>", 
             "<p>","share price / earnings ","</p>",
             "<h4><b>", "EPS Growth: positive" ,"</b></h4>",
             "<p>","earnings per share / time","</p>",
             "<h4><b>", "Sales Growth: positive" ,"</b></h4>",
             "<p>","net sales income / time","</p>"
             )
    }else if(input$strategy_button == "Indexing / ETFs"){
      paste0("<h4><b>", "Fees/Expense Ratio: < 1%", "</b></h4>", 
             "<p>","cost to run fund","</p>",
             "<h4><b>", "Diversification: High", "</b></h4>", 
             "<p>","variability of tracked assets","</p>",
             "<h4><b>", "EPS Growth: positive" ,"</b></h4>",
             "<p>","earnings per share / time","</p>",
             "<h4><b>", "Sales Growth: positive" ,"</b></h4>",
             "<p>","net sales income / time","</p>")
    }else if(input$strategy_button == "Long-term Treasury Bonds"){
      
    }else if(input$strategy_button == "Blue Chip Stocks"){
      paste0("<h4><b>", "Market Cap: >10 billion USD", "</b></h4>", 
             "<p>","est market size","</p>",
             "<h4><b>", "Volatility: <3% monthly", "</b></h4>", 
             "<p>","distribution of returns","</p>",
             "<h4><b>", "P/E Ratio: >20","</b></h4>", 
             "<p>","share price / earnings ","</p>",
             "<h4><b>", "EPS Growth: positive" ,"</b></h4>",
             "<p>","earnings per share / time","</p>",
             "<h4><b>", "Sales Growth: positive" ,"</b></h4>",
             "<p>","net sales income / time","</p>")
    }else{}
  })
  
}

# Launch the shiny application itself 
shinyApp(ui = ui, server = server)



# More information in the model: 
# Calculate when you can live off of withdrawal of 4% of your investments to reach your retirement goals. 
# Do not make color change on div income. Continue to make color change on years until you can retire. 
# Adjust years to income goal to be based on liquidation of 4% of your stocks 

### FinViz Stock Screener API  ###
library(rvest)
library(htmltools)

# Get the number of Pages of data there is available for a particular URL
get_no_pages <- function(tables){
  # Get vec of all of the page numbers 
  page_nos <- as.character(tables[[16]]$X4)
  # Sub out the word page and get a character vector of all of the page nos
  a <- strsplit(gsub("Page", "",  page_nos), split = " ")[[1]] 
  # Get the number of pages by splitting on / and taking second value in the list 
  no_pages <- as.numeric(strsplit(a[2], split = "/")[[1]][2])
  return(no_pages)
}

# Get all of the data for a URL filter for FinViz.com 
get_finViz_data <- function(url_val){
  
  # Get the first table from base URL page
  tables <- read_html(url_val) %>% 
    html_table(fill = TRUE)
    
  # Get the number of pages to sweep through 
  a <- get_no_pages(tables)
  
  # Iterate through all URLS and add tables to a list
  dat <- lapply(
      # Create URL based on web page number 
      paste0(url_val, "&r=", 2*(1:a),"1"), 
      # Read html tables, return the 17th table each time 
      function(url){
        tables1 <- read_html(url) %>%
          html_table(fill = TRUE)
        return(data.frame(tables1[[17]][-1,]))
      })
  
  # Add first page to list of tables
  dat[[length(dat)+1]] <- data.frame(tables[[17]][-1,])
  
  # Set Column names based on filters in url
  if(grepl("v=111", url_val)){
    dat = rbindlist(dat)[,-1] %>% set_colnames(c("Ticker", "Company_Name","Industry","Sector","Country","Market_Cap","P/E","Share_price","Change","Volume"))
    dat = dat[order(dat$Ticker),]
  }else{
    dat = rbindlist(dat)[,-1]
  }
  
  # Combine first table with all other tables in the dats list 
  return(dat)
}

# @TODO: Function to generate a url to use for the get_finviz_data based on user input to your shiny app
generate_finViz_URL <- function(){}

# Get all stocks that are large cap, on nasdaq exchange and part of s&p 500 index
a_list <- get_finViz_data("https://www.finviz.com/screener.ashx?v=111&f=cap_large,exch_nasd,idx_sp500")



### EPS / NET earnings Stock Information Searcher ###
# Purpose: to generate a viable list of all of the stocks and etfs that pay dividends. This list will then be created into a dataframe 
# by searching for more information about the shares such as what their dividend yield is, what their dividend growth has been like (5yr, 10yr, 20yr),
# how often they pay dividends, and the volatility of the stock as a whole. This information will be captured in a dataframe and filtered to meet the 
# users demands based on their entries in the dividend investment model.
# The different dividend stocks will be presented in a variety of comparitive and analytical manners, chiefly among which are 
# stock and dividend growth over time (and number of years in which the stock has kept growing), volatility and stabilty score,
# quadrant analysis of 

# Access Tiingo API account 
api_key <- "1cebbebc528db0995412994d0ea022a074f8fe71" 
riingo_set_token(api_key)

# Import all of the tickers in all of the markets that you care about
all_tickers <- supported_tickers() 

# Remove mutual funds, markets not using USD, and stocks/etfs that have an end date greater than 4 days prior to todays date
tickers_info <- na.omit(test_tickers[-c(which(test_tickers$exchange %in% c("SHE","SHG")), which(test_tickers$priceCurrency %in% c("CNY")), which(test_tickers$assetType %in% c("Mutual Fund")), which(test_tickers$endDate < Sys.Date()-4)),])
tickers <- tickers_info$ticker 

## Now let us create a function to just return all of the stocks that currently pay dividends from a list stocks 
get_div_stocks <- function(tickers){
  
  # Last item in the list to check 
  end = as.numeric(length(tickers))
  
  # Get date information to collect recent dividends 
  current_date = Sys.Date()
  last_year_date = paste0(year(current_date)-1,"-",month(Sys.Date()),"-",day(Sys.Date()))
  
  # Create a character vector to append all of the dividend paying stocks to 
  stocks_vec <- vector(mode = "character")
  
  # Try to get the stock info from yahoo finance. If you cannot (usually error HTTP 422) then move to next stock 
  for (k in 1:end){ 
    tryCatch(
      expr = {
        stock_name = tickers[k]
        data_divs = getDividends(stock_name, from = last_year_date, to = current_date)
        if(nrow(data_divs)!=0){
          stocks_vec = append(stocks_vec, stock_name)
        }
      }
      ,finally = message(paste0('Trying ticker no. ', k, " in list."))
      ,error = function(e){
        closeAllConnections()
      })
  }
  return(stocks_vec)
}

# Get comprehensive list of all of the stocks that have paid dividends in the period (7/26/2019 - 7/26/2020)
div_data1 <- get_div_stocks(tickers[1:1000])
div_data2 <- get_div_stocks(tickers[1001:2000])
div_data3 <- get_div_stocks(tickers[2001:3000])
div_data4 <- get_div_stocks(tickers[3001:5000])
div_data5 <- get_div_stocks(tickers[5001:8000])
div_data6 <- get_div_stocks(tickers[8001:10000])
div_data7 <- get_div_stocks(tickers[10001:14000])
div_data8 <- get_div_stocks(tickers[14001:18000])
div_data9 <- get_div_stocks(tickers[18000:21000])
div_data10 <- get_div_stocks(tickers[21000:length(tickers)])

# Combine into one character vector
div_data <- c(div_data1,div_data2,div_data3,div_data4,div_data5,div_data6,div_data7,div_data8,div_data9,div_data10)

# Create a csv file to store all of the tickers of the dividend paying stocks 
write.csv(div_data, file = "current_dividend_tickers.csv")

# Open the csv with all dividend stocks, name the cols and omit the first row and first col
div_ticks <- data.frame(read_csv("current_dividend_tickers.csv", col_names = c("x","tickers"), skip = 1)[,2], stringsAsFactors = FALSE)

# Get dividend yield and dividend growth avg for past 5 years 
# Add past 5 years earnings per share and net sales to the dataframe 

# ---------------------------------- Financial Analysis ----------------------------------------- #

# Parse CIK number from sec website from HTML using rvest
get_CIK <- function(ticker){
  require(dplyr)
  require(rvest)
      # Get CIK number
      url2 <- paste0("https://sec.report/Ticker/",tolower(as.character(ticker)))
      cik <- read_html(url2) %>%
              html_nodes(xpath = '/html/body/div/div/h2[1]') %>%
              html_text
      words <- strsplit(cik," ")[[1]]
  return(as.numeric(words[3]))
}

# Parse company name from sec website from HTML using rvest 
get_company_name <- function(ticker){
  require(dplyr)
  require(rvest)
    # Get company names
    url2 <- paste0("https://sec.report/Ticker/",tolower(as.character(ticker)))
    comp_name <- read_html(url2) %>%
      html_nodes(xpath = '/html/body/div/div/h1') %>%
      html_text
    words <- strsplit(comp_name, " ")[[1]]
  return(paste(words[-c(1,2)], collapse = ' '))
}

# Get filing info for current year
get_accession_no <- function(ticker, year, foreign_bool){
  require(finreportr)
  require(lubridate)
  require(dplyr)
  require(stringr)
    repo <- AnnualReports(ticker, foreign_bool)
    idx <- which(as.numeric(year(repo$filing.date)) == year & repo$filing.name == "10-K")[1]
    filling_no <- repo$accession.no[idx] %>% str_remove_all("-")
  return(filling_no)
}


# Get a list of dataframes with tickers, categorical variabes and example values. 
# This function serves the purpose of getting an example of every format variables on 
# income sheets may take. Takes in as parameters whether or 
# not you want to evaluate foreign tickers and the first n tickers in the list you 
# want to evaluate. If left empty, n will default to all of the tickers.
get_fin_list <- function(year = 2015, foreign_bool = FALSE, tickers_list){
  require(data.table)
  require(finreportr)
  require(xlsx)
  require(stringdist)
  require(lubridate)

  prev_years <- seq(year, year-4, -1)
  l <- list()
  
  for(i in 1:length(tickers_list)){
    # Get tickers from the ticker to cik list 
    ticker = tolower(as.character(tickers_list[i]))
    n <- as.numeric(length(tickers_list))
    message(paste0("Evaluating no.",i, " out of ", n, " tickers."))
    
    # Attempt to get the financial information from the tickers 
    tryCatch(
      expr = {
        # Get the company name scraped off of sec website
        name_comp <- get_company_name(ticker)
        
        # Get the report number and the filing number to form URL to get data
        filing_no <- get_accession_no(ticker, prev_years[1], foreign_bool)
        CIK_NO <- get_CIK(ticker)
        
        # Download the data and save to computer after trying URL
        url_b <- paste0("https://www.sec.gov/Archives/edgar/data/",CIK_NO,"/",filing_no,"/Financial_Report.xlsx")
        destfile <- "C:/Users/Zachery Key/Desktop/Financial_Project/output.xlsx"
        download.file(url_b, destfile, mode = "wb")
        
        # Get the workbook sheet idx with name closest to that which appears in the target 
        names_sheets <- loadWorkbook(destfile) %>% getSheets() %>% names()
        names_sheets
        
        # Target represents the docs you want, x target are the documents you explicitly do not want
        target <- c("consolidated statements of oper","consolidated statements of earn", "consolidated statements of inc", 
                    "consolidated statements of earnings","consolidated statements of income","consolidated statements of operations",
                    "statement of consolidated operations","statements of income", "consolidated income statements",
                    "consolidated_statements_of_ope","consolidated_statements_of_inc","consolidated_statements_of_ear",
                    "consolidated and combined state","condensed_consolidated_stateme","consolidated_income_statement",
                    "consolidated_and_combined_stat")
        
        # If any of the following appear exactly as below, delete them from the matrix because we dont want these statements 
        x_target <- c("consolidated_statements_of_cas","consolidated statements of cash","consolidated_statement_of_cash",
                      "consolidated_statement_of_comp","consolidated_statements_of_cha")
        x_matrix <- stringdistmatrix(tolower(names_sheets), x_target, useNames = TRUE)
        x_idx = unique(which(x_matrix <= 1, arr.ind = TRUE)[,1])
        
        # Set up a matrix to evaluate the difference between the actual sheetnames and the target sheetnames.
        # Choose the one of minimal difference. Remove all of the items you do not want to have as possible 
        # sheetnames from the matrix. 
        matrix <- stringdistmatrix(tolower(names_sheets), target, useNames = TRUE)
        if(length(na.omit(x_idx)) > 0){
          matrix <- matrix[-(na.omit(x_idx)),]
        }
        sheet_name <- rownames(which(matrix == min(matrix), arr.ind = TRUE))[1]
        sheet_num <- which(tolower(names_sheets) == sheet_name)
        
        # Open and store data in R dataframe
        info <- read.xlsx(destfile, sheetIndex = sheet_num, as.data.frame = TRUE)
        
        # Get the units and type of information of the sheet: 
        units1 <- colnames(info)[1]
        units2 <- info[1,1]
        
        # Include Appropriate columns in info.
        # We want to get all of the cols with about the same number of NAs (no more than 1/3 occupancy of col)
        # and greater idx than the col with the label 'X12 months ended' or 'X11 months ended' 
        n = which(colnames(info) %in% c("X11.Months.Ended","X12.Months.Ended"))[1]
        nas_info = as.numeric(apply(info, 2, function(x){sum(is.na(x))}))[n:ncol(info)]
        vec_diff = nas_info - nas_info[1]
        idxs =  which( vec_diff < (1*nrow(info)/3) ) + n - 1
        
        if(n %in% idxs){
          info <- info[,c(1,idxs)]
        }else{
          info <- info[,c(1,n,idxs)]
        }
        
        # Store the ticker, the data and the units the data is held in/type of form being evaluated
        income_state2 <- data.frame(ticker, name_comp, info, units1, units2)
        
        # Store into list 
        l[[i]] <- income_state2
        
        # Print out success message
        message(paste0(ticker,": success!"))
      },
      error = function(e){
        closeAllConnections()
        # Print out failure message 
        message(paste0(ticker,": invalid ticker, year or foreign logical"))
      }
    )
    closeAllConnections()
  }
  
  return(l)
} 

# Create a function to iterate through your ex_format dataframe ticker by ticker and have the user enter the 
# rownumber of the row that best meets the metric they are lookng to capture upon input
set_metrics <- function(metric, data_source){
  require(trqwe)
  metric_aliases <- vector(mode = "character")
  n = length(unique(data_source$ticker))
  for(i in 1:n){
    # Get all of the rows with the same ticker and print them out
    tick = unique(data_source$ticker)[i]
    data = ex_forma_full
    subset <- data.frame(data_source[which(data_source$ticker==tick),], 1:nrow(data_source[which(data_source$ticker == tick),])) %>% 
            set_colnames(c("ticker","name","metric","col1","col2","col3","units1","units2","row_no"))
    print(subset[,c(1,3,9)])
    message(paste0("printing ",i," of ",n," unique tickers"))
    
    # Collect the user input number row matching the metric you are looking for 
    line_no <- as.numeric(readline(prompt = paste0("Enter number of metric best matching ", metric, ": ", 
                                                 '\n',"(enter nothing to exit program)", '\n',
                                                  "(enter 0 to skip current ticker)")))
  
    # Select and store all of the aliases for the metric into a character vector
    if(!is.na(line_no) & line_no > 0){
      metric_aliases[i] <- subset[which(subset$row_no==line_no), which(colnames(subset)=="metric")]
    }else if(!is.na(line_no) & line_no <= 0){
      next
    }else{
      break
    }
    closeAllConnections()
  }
  return(metric_aliases)
}

# Take in a data.frame with eight cols (ticker, company_name, metric, y1, y2, y3, unit1, unit2) and selects which 
# rows best correspond to net_income metric. This function will always return a best guess
get_net_income <- function(data_source, tick){

  # Set up a list of all of the possible aliases for net income
  net_inc_alts <-c("net income","net loss income","net income (loss)","net earnings","net earnings (loss)","net income attributable to common stockholders",
                   "net income attributable to the stockholders of","net income attributable to","net income attributable to common shareholders",
                   "net income attributable to shareholders", "net earnings including noncontrolling interests")
  
  # Get the subset of the data for all rows with the same ticker, calc closest resemblance to a string in net_inc_alias, get the index of the row
  subset1 <- data_source[which(data_source$ticker==tick),]
  mat <- stringdistmatrix(na.omit(tolower(subset1$metric)), net_inc_alts, useNames = TRUE)
  idx <- which(mat == min(mat), arr.ind = TRUE)[1]
  
  # Get the name of the metric as appears in the actual dataframe. Append new metric name and return 
  namer <- rownames(mat)[idx]
  dat <- data.frame(subset1[which(tolower(subset1$metric)==namer)[1],])
  dat$metric <- "net_income"
  return(dat[,which(colnames(dat) %in% c("ticker","name","metric","y1","y2","y3","units1","units2"))])
}

# Move through datasource and identify the row corresponding to eps. This function will only return a best guess if there is a row 
# in the data frame that meets the criteria. Weights derived from hand selecting eps over 100 companies, and seeing which words most
# often appear for earnings per share data. 
get_eps <- function(data_source, tick){
  
  # Constants used to compare rows for eps data
  hits_eps <- c("share","per","basic","earnings","dollars","net","common","income","total","dividends","dividend")
  weights_hits_eps <- c(7,6,9,8,3,2,4,1,5,-20,-20)
  eps_words_weights <- data.frame(as.character(hits_eps),weights_hits_eps)
  
  # Subset the datasource to only have a single ticker, remove na vals and create a list of lists of metric data
  subset1 <- data_source[which(data_source$ticker==tick),]
  subset <- subset1[which(!(is.na(subset1$y1) & is.na(subset1$y2) & is.na(subset1$y3))),]
  parse_metrics <- tolower(subset$metric) %>% str_replace_all("[^0-9A-Za-z///' ]","") %>% strsplit(split = ' ')
  weights_score <- vector()
  
  # Iterate through the list of metric info and calculate the sum of weighted words in each row. Add to a vector 
  for(i in 1:length(parse_metrics)){
    ex_line <- parse_metrics[[i]]
    score <- sum(eps_words_weights[which(as.character(eps_words_weights[,1]) %in% ex_line),2])
    weights_score <- append(weights_score, score)
  }
  weights_score
  # Add a eps likelihood col to the dataframe 
  subset$eps_score = weights_score
  
  # Ok now that we have the scores corresponding to each row in subset, lets further narrow down the candidates by checking to see which of the 
  # weights score has a lower then 100 value for y1/y2/y3. Now take the max value  of eps_score from here and that is your row!
  order_subset <- subset[which(rowMeans(abs(data.frame(as.numeric(subset$y1),as.numeric(subset$y2),as.numeric(subset$y3))), na.rm = TRUE) < 200), ]
  order_subset$metric <- "eps"
  return(order_subset[which.max(order_subset$eps_score), which(colnames(order_subset) %in% c("ticker","name","metric","y1","y2","y3","units1","units2"))])
}

# Takes the units from units col 1 and units col 2. Parses the share and stock multipliers and returns 
# a vector with two strings: one of stock multiplier and the other of share multiplier IN THAT ORDER. 
parse_units <- function(units1, units2){
  
  # Parse both units 1 and units 2 for unit multiplier
  unit_m <- c(parse_units2(units2)[1], parse_units1(units1)[1])
  unit_multiplier <- unit_m[!is.na(unit_m)][1]
  
  # Parse both units 1 and units 2 for share multiplier 
  shar_m <- c(parse_units2(units2)[2], parse_units1(units1)[2])
  shar_multiplier <- shar_m[!is.na(shar_m)][1]
  
  return(c(unit_multiplier,shar_multiplier))
}

parse_units1 <- function(units1){
  # Split on '.' char, lowercase all letters and remove all words up to and including USD
  ex_split1 <- stri_remove_empty(strsplit(tolower(units1), split = '\\.')[[1]])
  ex_split1 <- ex_split1[-(1:which(ex_split1=="usd"))]
  
  share1_id = which(ex_split1 %in% c("shares","share"))[1]
  idxs1 = which(ex_split1 %in% key_multiplier_phrases)
  
  # if the word shares is present and multiple key_multipliers appear after it, then the closest idx to shares 
  # is the shr_multipler and the farthest is the val_multiplier. If share does not appear, and there is only one
  # key mutiplier, then that is the val_multiplier. If there is nothing after usd, no data is available in that col
  
  if(!is.na(share1_id)){
    shr_multiplier1 <- ex_split1[idxs1[which.min(abs(share1_id - idxs1))]]
    val_multipler1 <- ex_split1[idxs1[which.max(abs(share1_id-idxs1))]]
  }else if(is_empty(ex_split1)){
    shr_multiplier1 <- NA
    val_multipler1 <- NA
  }else if(is.na(share1_id) & !is_empty(ex_split1)){
    shr_multiplier1 <- NA
    val_multipler1 <- ex_split1[idxs1]
  }
  return(c(val_multipler1,shr_multiplier1))
}

parse_units2 <- function(units2){
  # Remove punctuation, split string on white space into character array  
  ex_split <- strsplit(str_replace_all(tolower(units2), "[[:punct:]]", ""), split = ' ')[[1]]
  
  # Set up the key words we are looking for in the units2 col
  key_multiplier_phrases <- c("thousands", "millions")
  key_share_phrases <- c("shares","share")
  
  # Find the indices of the key words we are looking for in the character arra5
  except_id = which(ex_split == "except")
  share_id = which(ex_split == "share")
  first_in_id = which(ex_split == "in")[1]
  sec_in_id = which(ex_split == "in")[2]
  idxs = which(ex_split %in% key_multiplier_phrases)
  words_idxs = ex_split[idxs]
  
  # Get the overall unit multiplier. If it is to the right of the first 'in' and before the word except AND share then it is good
  val_multipler = words_idxs[which(idxs < except_id & idxs < share_id & idxs > first_in_id)]
  if(is_empty(val_multipler)){
    val_multipler = NA
  }
  
  # Get the share multiplier
  shr_multiplier = words_idxs[which(idxs > except_id & idxs > share_id & idxs > sec_in_id)]
  if(is_empty(shr_multiplier)){
    shr_multiplier = NA
  }
  return(c(val_multipler, shr_multiplier))
}

# Function to return all valid entries from a financial data_frame
combine_data_list<-function(data){
  
  # Get the dimension of the data in terms of number of columns 
  data_dim <- as.numeric(lapply(data, function(x){ncol(data.frame(x))}))
  
  # Get all of the data with 8 cols, convert all cols to character, bind dataframes together and rename columns 
  data_forma_full <- data[which(data_dim==8)] %>% lapply(function(x){lapply(x, function(y){as.character(y)})}) %>%
    rbindlist() %>% data.frame() %>% set_colnames(c('ticker','name','metric','y1','y2','y3','units1','units2'))
  
  # Same as above, but for data with only 7 cols 
  data_forma_partial <- data[which(data_dim==7)] %>% lapply(function(x){lapply(x, function(y){as.character(y)})}) %>% 
    rbindlist() %>% data.frame() %>% add_column(NA, .after = 5) %>% set_colnames(c('ticker','name','metric','y1','y2','y3','units1','units2'))
  
  # Combine two dataframes and return as one
  return(rbind(data_forma_full, data_forma_partial))
}

# Function to return a dividend data frame with ticker, yearly div payout per share, yearly div growth, yearly div yield 
get_divs <- function(tick){
  require(quantmod)
  require(data.table)
  # Get dividend info for tick 
  div_data <- data.frame(getDividends(as.character(tick)))
  
  # Get stock price data in prep to compute yield 
  stock_data <- data.frame(getSymbols(tick, auto.assign = FALSE))
  stock_data <- data.frame(stock_data[,6], as.factor(year(as.character(rownames(stock_data))))) %>% set_colnames(c("value","year"))
  median_price <- as.numeric(by(
                        data = stock_data$value
                        ,INDICES = stock_data$year
                        ,FUN = function(x){
                            median(x)
                        }))
  
  stock_data <- data.frame(median_price, as.character(levels(stock_data$year))) %>% 
                  set_colnames(c("stock_price","year"))
  
  stock_data <- mutate(stock_data, stock_growth = ((stock_data$stock_price - lag(stock_data$stock_price))/lag(stock_data$stock_price)))
                  
  
  # Bin the data by year and get a vec of unique years 
  div_data$year <- factor(year(as.character(rownames(div_data))))
  years_vec <- as.character(levels(div_data$year))
  
  # Get the sum of the dividend payouts in a year 
  divs_count <- as.numeric(by(
                             data = div_data[,1]
                            ,INDICES = div_data$year 
                            ,FUN = function(x){
                                    sum(x)
                            }))
  
  # Remove first and last entries (which may be incomplete) and turn into data.frame
  div_data <- data.frame(divs_count[-c(1, length(divs_count))], 
                         years_vec[-c(1, length(years_vec))]
                         ) %>% set_colnames(c("dividend","year"))
  
  # Fill in all of the years that are missing from first to last row and assign them 0 vals in the div column. 
  # Iterate through the years col and identify all rows that are not consecutive years 
  div_dat2 <- mutate(div_data, year_diff = (as.numeric(div_data$year) - lag(as.numeric(div_data$year), default = as.numeric(div_data$year[1]))))
  
  # Fill in every year that is missing from the start to the end date in div_data
  my_df_list <-list()
  my_df_list[[1]] <- div_dat2
  n = 2
  for(i in 1:nrow(div_dat2)){
    # Check to see if yeardiff is greater than 1
    if(div_dat2$year_diff[i] > 1){
      # Create new rows with 
      years_cont <- seq(as.numeric(div_dat2$year[i-1])+1, as.numeric(div_dat2$year[i])-1, 1)
      # Add the data frames to a list 
      my_df_list[[n]] <- data.frame(0, years_cont, 1) %>% set_colnames(c("dividend","year","year_diff"))
      n = n+1
    }
  }
  # Order, remove year_diff col and add tick
  div_dat2 <- data.frame(rbindlist(my_df_list) %>% setorder(year))
  div_dat2$ticker <- tick
  div_dat2 <- div_dat2[,-as.numeric(which(colnames(div_dat2)=="year_diff"))]
  
  # Calculate yearly dividend growth rate
  div_dat2$div_growth <- (div_dat2$dividend / lag(div_dat2$dividend, default = 0) - 1)
  div_dat2$div_growth[is.na(div_dat2$div_growth)] <- 0
  div_dat2$div_growth[is.infinite(div_dat2$div_growth)] <- NA
  
  # Join stock data with div_dat2 by year
  div_dat2 <- div_dat2 %>% left_join(stock_data, by = "year") 
  div_dat2 <- mutate(div_dat2, div_yield = (div_dat2$dividend/div_dat2$stock_price))
  
  # Return data frame with yearly dividend, ticker, yearly div_growth and div_volatility
  return(div_dat2)
}


# Take in a dataframe constructed by get_divs and spits out standard deviation of 90% of data closest to median as 
# well as average growth percentage over time since start of data 
get_divs_stats<-function(div_dat2){
  # Cut top 5% and bottom 5% 
  five_pct = ceiling(nrow(div_dat2)*.05)
  a <- setorder(div_dat2, div_growth)
  div_dat2 <- a[-c(1:five_pct, (nrow(a)-five_pct+1):nrow(a)),]
  
  # Calulate standard deviation and average div_growth for stock
  avg_growth <- sum(div_dat2$div_growth, na.rm = TRUE) / length(div_dat2$div_growth)
  stand_dev <- sd(div_dat2$div_growth, na.rm = TRUE)
  return(data.frame(c("div_volatility","div_growth"),c(stand_dev, avg_growth)) %>% set_colnames(c("metric", "value")))
}



# # Write Div_ticks to csv. Write 
# write.csv(div_ticks, "C:\\Users\\Zachery Key\\Desktop\\Financial_Project\\div_ticks.csv", row.names = FALSE)

# # Get a list with the first 1000 domestic stocks financial info 3 years prior to 2015
# first_1000_stocks <- get_fin_list(2015, FALSE, tickers_list = div_ticks$tickers[1:1000]) %>% combine_data_list()
# write.csv(first_1000_stocks, "C:\\Users\\Zachery Key\\Desktop\\Financial_Project\\Financial_Planning\\first_1000_stock_data.csv", row.names = FALSE)

# # Get next 1000 tickers (188 total non null entries)
# second_1000_stocks <- get_fin_list(2015, FALSE, tickers_list = div_ticks$tickers[1001:2000]) %>% combine_data_list()
# write.csv(second_1000_stocks, "C:\\Users\\Zachery Key\\Desktop\\Financial_Project\\Financial_Planning\\second_1000_stock_data.csv", row.names = FALSE)

# # Get next 1000 tickers 
# third_1000_stocks <- get_fin_list(2015, FALSE, tickers_list = div_ticks$tickers[2001:3000]) %>% combine_data_list()
# write.csv(third_1000_stocks, "C:\\Users\\Zachery Key\\Desktop\\Financial_Project\\Financial_Planning\\third_1000_stock_data.csv", row.names = FALSE)

# # Get next 1000 tickers 
# fourth_1000_stocks <- get_fin_list(2015, FALSE, tickers_list = div_ticks$tickers[3001:4000]) %>% combine_data_list()
# write.csv(fourth_1000_stocks, "C:\\Users\\Zachery Key\\Desktop\\Financial_Project\\Financial_Planning\\fourth_1000_stock_data.csv", row.names = FALSE)

# # Get next 1000 tickers 
fifth_1000_stocks <- get_fin_list(2015, FALSE, tickers_list = div_ticks$tickers[4001:length(div_ticks$tickers)]) %>% combine_data_list()
write.csv(fifth_1000_stocks, "C:\\Users\\Zachery Key\\Desktop\\Financial_Project\\Financial_Planning\\fifth_1000_stock_data.csv", row.names = FALSE)

# Load all past work into R workspace 
first_1000_stocks <- read.csv('first_1000_stock_data.csv')
second_1000_stocks <- read.csv('second_1000_stock_data.csv')
third_1000_stocks <- read.csv('third_1000_stock_data.csv')
fourth_1000_stocks <- read.csv('fourth_1000_stock_data.csv')
fifth_1000_stocks <- read.csv('fifth_1000_stock_data.csv')
div_ticks <- read.csv('div_ticks.csv')

# The best guess for eps per our algorithmn 
get_net_income(first_1000_stocks, 'cci')

# The best guess for eps per our algorithmn with weighted values for eps_words 
get_eps(first_1000_stocks, 'cci')

# Get the dividends and a measure of their volatility and growth 
ticker = 'LOW'
stock_divs <- get_divs(ticker)
get_divs_stats(stock_divs)

yearly_div_plot <- ggplot(stock_divs, aes(x = as.numeric(year), y = dividend, group = 1)) + 
                      geom_line(size = 1.5, color = 'red') +
                      xlab("Year") + ylab("Yearly dividend payments (USD/share)") + ggtitle(paste0(stock_divs$ticker[1], " yearly dividend growth")) + 
                      scale_x_continuous(n.breaks = 10) + 
                      scale_y_continuous(n.breaks = 10)

yearly_div_plot



# Loop through all of ex_forma_full and check to see if our algorithmns are working. TryCatch the eps and net_income in the event that they are 
# not available 
storage_list <- list()
for(i in 1:length(unique(first_1000_stocks$ticker))){
  flag <- TRUE
  tick = unique(first_1000_stocks$ticker)[i]
  tryCatch(
    # Attempt to set up an expression to get the eps and the net income. If it doesn't work, just get the net income
    expr = {
      eps_getter = data.frame(get_eps(first_1000_stocks, as.character(tick)))
      net_getter = data.frame(get_net_income(first_1000_stocks, as.character(tick)))
      storage_list[[i]] <- data.frame(rbind(eps_getter, net_getter))
      }
   ,error = function(e){
     flag <<- FALSE
    }
  )
  if(!flag){
    print(i)
    net_getter = data.frame(get_net_income(first_1000_stocks, as.character(tick)))
    storage_list[[i]] <- data.frame(net_getter)
  }
}

# Bind and place all the information on eps and net income into a data frame. Save to csv 
eps_and_net <- rbindlist(storage_list)
write.csv(eps_and_net, 'eps_and_net.csv', row.names = FALSE)

# Manipulate the data in the eps_and_net data frame by setting it up so that 
div_info <- data.frame(read.csv('eps_and_net.csv'))
colnames(div_info)[which(colnames(div_info) %in% c("y1","y2","y3"))] <- c(2015, 2014, 2013)


# Need to check out what the deal is with these
'apog'
'ato'
'cato'



# Dont think I like this that much 
strategies <- c("Indexing","Income Investing","REITs","Growth Investing","Value Investing","Penny Stocks","Futures and Options")
variables <- c("Risk", "Short term return", "Long term return", "Supervision", "Volatility")

fig <- plot_ly(
  type = 'scatterpolar',
  fill = 'toself',
  mode = 'markers'
) 
fig <- fig %>%
  add_trace(
    r = c(1,1,5,2,1),
    theta = variables,
    name = strategies[1]
  ) 
fig <- fig %>%
  add_trace(
    r = c(1,2,5,2,2),
    theta = variables,
    name = strategies[2]
  )
fig <- fig %>%
  add_trace(
    r = c(2,2,4,2,2),
    theta = variables,
    name = strategies[3]
  )
fig <- fig %>%
  add_trace(
    r = c(2.5,4,4,3,3),
    theta = variables,
    name = strategies[4]
  )
fig <- fig %>%
  add_trace(
    r = c(2,2,5,4,3),
    theta = variables,
    name = strategies[5]
  )
fig <- fig %>%
  add_trace(
    r = c(4,4,3,5,4),
    theta = variables,
    name = strategies[6]
  )
fig <- fig %>%
  add_trace(
    r = c(5,5,2,5,5),
    theta = variables,
    name = strategies[7]
  )
fig <- fig %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,5)
      )
    )
  )

fig

# Quandl Datatable 
library(Quandl)
q_api_key <- "N-Yrjym9xQS8iGyHZyx9" #QUandl API key 
Quandl.api_key(q_api_key)
# tickers_select = c('AAPL','JPM')
data <- Quandl.datatable("SHARADAR/SF1") 
#                          calendardate.gte = start_date, 
#                          ticker = tickers_select, 
#                          qopts.columns=c('ticker', 'calendardate', 'netinc', 'divyield', 'ncfdiv', 'eps', 'pe', 'revenue'))

### --------------------------------- Depricated Code -------------------------------- ###

### TRYCATCH EXAMPLE 1.) Try until sucessful!! ###
# for (i in 1:10){ 
#   flag <- TRUE
#   tryCatch(
#     # Step 1. Here is where you put the thing you are trying to do that may cause errors 
#     expr = {
#       #print(2*i)              # When uncommented, will not result in an error, thus cutting the loop short 
#       #print(2*last_year_date) # When uncommented, Will result in an error, thus continuing the loop
#     }
#     
#     # Step 2. Here is where you put the thing to be done regardless of presence of errors 
#     ,finally = print(paste0('Trying ticker no. ', i))
#     
#     # Step 3. Here is where you handle errors that may occur in step 1. of trycatch. 
#     # In this example, if there has been an error then update flag to be false. This will allow you to 
#     # continue looping. The double arrow <<- is used to modify a variable in a higher scope than the current
#     # expression. (AKA outside the tryCatch function)
#     ,error = function(e){flag <<- FALSE})
#   
#   # If there has been an error, try step 1 again, print out a message confirming failure
#   if(flag == FALSE){
#     print(paste("Failed attempt on ticker no.", i))
#     next
#     # Break the loop on the first successful attempt of step 1, and print out a message confirming success
#   }else{
#     print("Succesful!")
#     break
#   }
# }


# Function to return a dataframe of all of the stocks that pay dividends from the 
# list of stocks passed in as a parameter using yahoo finance api. Checks to see if the stocks 
# listed are valid AND have dividends, with information displayed as warning messages. 
# get_crnt_divs<-function(tickers){
#   
#   # Last item in the list to check 
#   end = as.numeric(length(tickers))
#   
#   # Get date information to collect recent dividends 
#   current_date = Sys.Date()
#   last_year_date = paste0(year(current_date)-1,"-",month(Sys.Date()),"-",day(Sys.Date()))
#   
#   # Set initial dividend information 
#   for (k in 1:end){ 
#     flag <- TRUE
#     tryCatch(
#       # Step 1. Here is where you put the thing you are trying to do that may cause errors 
#       expr = {
#         stock_name = tickers[k]
#         data_divs = data.frame(getDividends(stock_name, from = last_year_date, to = current_date))
#         data_divs$date <- date(rownames(data_divs))
#         data_divs$ticker <- as.character(tickers[k])
#         colnames(data_divs)<-c("vals","date","tickers")
#       }
#       
#       # Step 2. Here is where you put the thing to be done regardless of presence of errors 
#       ,finally = message(paste0('Trying ticker no. ', k, " in list."))
#       
#       # Step 3. Here is where you handle errors that may occur in step 1. of trycatch. 
#       # In this example, if there has been an error then update flag to be false. This will allow you to 
#       # continue looping. The double arrow <<- is used to modify a variable in a higher scope than the current
#       # expression. (AKA outside the tryCatch function)
#       ,error = function(e){
#         flag <<- FALSE
#         closeAllConnections()
#       })
#     
#     # If there has been an error, try step 1 again, print out a message confirming failure
#     if(!flag){
#       message(paste0("Failed attempt on ticker ", tickers[k],"."))
#       next
#       # Break the loop on the first successful attempt of step 1, and print out a message confirming success
#     }else{
#       message("Success!")
#       cat('\n')
#       break
#     }
#   }
#   
#   # Get all of the rest of the dividends appended to dataframe IF they have paid dividends in the last year 
#   for(i in k:end){
#     tryCatch({
#       stock_name = tickers[i]
#       divs1 = data.frame(getDividends(stock_name, from = last_year_date, to = current_date))
#       #message("* Valid ticker: ", tickers[i])
#       
#       if(nrow(divs1)!=0){
#         data_divs1 = data.frame(divs1[,1], date(row.names(divs1)), stock_name)
#         colnames(data_divs1) <- c("vals","date","tickers")
#         data_divs<-rbind(data_divs,data_divs1)
#         message(tickers[i], " has paid dividends in past 12 months.")
#         cat('\n')
#       }else{
#         # message("* ",tickers[i]," has no recorded dividends for past 12 months.")
#       }
#     },
#     error = function(e){
#       closeAllConnections()
#       # message("* Warning! Invalid ticker: ", tickers[i])
#     })
#     
#     #print(paste0("Evaluating ticker no. ",i,". of ",end))
#     
#   }
#   closeAllConnections()
#   return(data_divs)
# }

# # Income of 40K, planning for retirement income of 30K, invest 30% monthly for x years, inflation of 3%, 
# # dividend growth of 1.6%, dividend yield of 3.5%, stock growth of 4%, stock price of 46.31. 
# # 26 years to save enough to live on dividends. 
# 
# # Set up global variables used to determine an individuals investing goals 
# 
# # Set the user - defined variables 
# Current_income = 50000     # Current Income
# Future_income = 25000      # Minimum Retirement Income you are comfortable living on, in todays dollars. 
# years_away = 50            # Years until retirement
# investment_pct = .2        # User selected percentage of income to invest
# 
# # Set the stock and economy - defined variables 
# inflation_rate = .025         # Predicted rate of inflation (actually closer to 2.5% but we rounded up for safety)
# dividend_growth_rate = 1.03   # Rate at which the dividend will grow over time 
# stock_growth_rate = 1.05      # Rate at which stock grows over time
# dividend_yield = .03          # Percentage of share each dividend payout will be worth
# share_price = 15.00           # Present Day Value of one single share 
# 
# # Project Future earnings from dividend investments (in current US dollars)
# # years, current income income, pct_invested, inflation_rate, growth_div, div_yield, growth_stock, crnt_stock_price
# div_growth_model1 <- div_growth_invest(years_away, Current_income, Future_income, inflation_rate,investment_pct, dividend_growth_rate, dividend_yield, stock_growth_rate, share_price, FALSE)
# div_growth_model2 <- div_growth_invest(years_away, Current_income, Future_income, inflation_rate,investment_pct, dividend_growth_rate, dividend_yield, stock_growth_rate, share_price, TRUE)
# 
# # Get the year at which you have achieved your minimum retirement income 
# reached_goal_year <- get_years(div_growth_model1)
# 
# # Get the actual projected income for retirement AND the goal for retirement for comparison 
# print(div_growth_model1[nrow(div_growth_model1),which(colnames(div_growth_model1)=="monthly_income")])
# print(get_income_goal(years_away, Future_income, inflation_rate))
# print(get_years(20, 40000, 30000, .3, .025, 1.016, .035, 1.04, 46.00))

# library(alphavantager)
# library(Quandl)
# library(riingo)
# 
# # Access Tiingo API account 
# tiingo_api_key <- "1cebbebc528db0995412994d0ea022a074f8fe71" #Tiingo API key
# av_api_key("C3X342P84O5D2QGN")      #Alphavantage API key 
# q_api_key <- "N-Yrjym9xQS8iGyHZyx9" #QUandl API key 
# av_get(symbol = "AAPL", av_fun = "OVERVIEW")
# 
# start_date <- paste0(year(current_date)-3,"-",'01','-','01')
# 
# # TIINGO (pay to get more stocks, currently limited to DJIA 30 stocks)
# riingo_set_token(api_key)
# data_available<-riingo_fundamentals_definitions()
# data_me <- riingo_fundamentals_statements("APPL", start_date = start_date, end_date = Sys.Date())

# # QUANDL: (pay to get more stocks, very basic info only here)
# Quandl.api_key(q_api_key)
# tickers_select = c('AAPL','JPM')
# data <- Quandl.datatable("SHARADAR/SF1", 
#                          calendardate.gte = start_date, 
#                          ticker = tickers_select, 
#                          qopts.columns=c('ticker', 'calendardate', 'netinc', 'divyield', 'ncfdiv', 'eps', 'pe', 'revenue'))

# SEC: Gets income statements from company from past three years successfully BUT... takes forever 

# library(stringdist)
# library(finreportr)
# a <- GetIncome("AAPL", year)
# b <- GetIncome("TSLA", year)
# c <- GetIncome("A",year)
# d <- GetIncome("XOM",year)
# 
# data = d
#
#        which(!grepl("diluted",tolower(data$Metric)) & stringdist("earnings per share, basic",tolower(data$Metric)) < 16 & date(data$endDate) - date(data$startDate)>300)),
#                        -c(which(colnames(data) %in% c("Units","startDate")))],ticker_name) %>% 
#   set_colnames(c("Metric","Amount","Date","Ticker")) %>%  
#   melt(c("Ticker","Date","Metric"),"Amount") %data = d
# ticker_name = "XOM"
# year = 2015
# 
# dat <- data.frame(data[c(which((stringdist("weighted average number of shares outstanding, basic",tolower(data$Metric)) < 20) & (!grepl("diluted",tolower(data$Metric))),date(data$endDate) - date(data$startDate)>300),
#                   >% 
#   dcast(Ticker + Date ~ Metric) %>% 
#   set_colnames(c("ticker","date","eps","num_shares"))

#
# # RUN ONLY ONCE: Get conversion list of all of the SEC codes for tickers with CIK encoding 
# tick_to_CIK <- data.frame(read.table(url("https://www.sec.gov/include/ticker.txt"))) %>% set_colnames(c("ticker","cik"))
# write.csv(tick_to_CIK, file = "cik_2_tick.csv")
# 
# # User set variables of ticker and whether or not it is a foreign stock
# ticker = "aa"
# foreign_bool = FALSE
# year = 2020
# prev_years <- seq(year-2, year-4, -1)
# 
# # Get the report number and the filing number to form URL to get data
# repo <- AnnualReports(ticker, foreign = foreign_bool)
# filling_no <- repo$accession.no[2] %>% str_remove_all("-")
# CIK_NO <- tick_to_CIK[which(tick_to_CIK$ticker==ticker),2]
# 
# # Download the data and save to computer
# url_b <- paste0("https://www.sec.gov/Archives/edgar/data/",CIK_NO,"/",filling_no,"/Financial_Report.xlsx")
# destfile <- "C:/Users/Zachery Key/Desktop/Financial_Project/output.xlsx"
# download.file(url_b, destfile, mode = "wb")
# 
# # Open and store data in R dataframe
# info <- data.frame(read.xlsx(destfile, sheetIndex = 2))
# 
# y3 = which(grepl(as.character(prev_years[1]),as.character(info[1,])))
# y2 = which(grepl(as.character(prev_years[2]),as.character(info[1,])))
# y1 = which(grepl(as.character(prev_years[3]),as.character(info[1,])))
# 
# # Transform data to desired format by transposing cols and rows 
# info <- na.omit(info[,c(1,y1,y2,y3)])
# 
# # Find the closest possible match to the know aliases for EPS, net income and no of shares in the first column of the info df  
# eps_row = stringdistmatrix(c("earnings per common share (dollars)",
#                              "basic (in dollars per share)",
#                              "Net income per share - basic",
#                              "basic"),
#                            tolower(info[,1]))
# 
# shr_row = stringdistmatrix(c("number of shares outstanding",
#                              "basic (in shares)"),
#                            tolower(info[,1]))
# 
# inc_row = stringdistmatrix(c("net income attributtable to parent",
#                              "net income (loss) attributable to",
#                              "net income"),
#                            tolower(info[,1]))
# 
# # Row numbers for the most likely candidates for eps, net income and number of shares outstanding 
# ids <- c(which(eps_row == min(eps_row), arr.ind = TRUE)[2],
#          which(shr_row == min(shr_row), arr.ind = TRUE)[2],
#          which(inc_row == min(inc_row), arr.ind = TRUE)[2]
# )
# 
# # Combine the rows of data that you are 
# info[ids,]

# 
# d <- GetIncome("XOM",year)
# ticker_name = "XOM"
# year = 2015
# 
# dat <- data.frame(data[c(which((stringdist("weighted average number of shares outstanding, basic",tolower(data$Metric)) < 20) & (!grepl("diluted",tolower(data$Metric))),date(data$endDate) - date(data$startDate)>300),
#                          which(!grepl("diluted",tolower(data$Metric)) & stringdist("earnings per share, basic",tolower(data$Metric)) < 16 & date(data$endDate) - date(data$startDate)>300)),
#                        -c(which(colnames(data) %in% c("Units","startDate")))],ticker_name) %>%
#   set_colnames(c("Metric","Amount","Date","Ticker")) #%>%
# #melt(c("Ticker","Date","Metric"),"Amount") %>%
# #dcast(Ticker + Date ~ Metric) %>%
# #set_colnames(c("ticker","date","eps","num_shares"))
# 
# 
# #   set_colnames(c("ticker","date","eps","num_shares"))

# # SEC Data using our own coding and a bit of finreportR
# a <- GetIncome("AAPL", year)
# b <- GetIncome("TSLA", year)
# c <- GetIncome("A",year)
# d <- GetIncome("XOM",year)
# 
# data = d
# ticker_name = "XOM"
# year = 2015
# 
# # Row ids
# nshares <- stringdist(c("weighted average number of shares outstanding, basic",
#                         "number of shares outstanding"),
#                       tolower(data[,1]))
# 
# eps <- stringdist(c("earnings per share, basic",
#                     "earnings per common share (dollars)"),
#                   tolower(data[,1]))
# 
# rev <- stringdist(c("net income for the year",
#                     "net income"),
#                   tolower(data[,1]))
# 
# x_dilu <- !grepl("diluted",tolower(data$Metric))
# x_qtrs <- date(data$endDate) - date(data$startDate) > 300
# 
# # Column ids
# x_cols <- colnames(data) %in% c("Units","startDate")
# 
# # format data  
# #set_colnames(c("Metric","Amount","Date","Ticker")) %>%
# #melt(c("Ticker","Date","Metric"),"Amount") %>%
# #dcast(Ticker + Date ~ Metric)
# 
# data

# library(GetEdgarData)
# 
# # One way to do this for free is to manually download and subset data from the SEC. 
# # However, like most government work, you have to cut through miles of red tape and 
# # it takes FOREVER to do.
# my_year = 2018
# type_form = '10-K'
# 
# df_info <- get_info_companies(years = my_year, 
#                               type_data = 'yearly', 
#                               type_form = type_form, 
#                               cache_folder = 'C:/Users/Zachery Key/Desktop/Financial_Project/cache'
# )
# 
# my_company <- 'APPLE INC'
# my_years <- 2013:2018
# type_data <- 'yearly'
# 
# df_fin_reports <- get_edgar_fin_data(companies = my_company,
#                                      years = my_years,
#                                      type_data = type_data)
# 
# # Remove unneccesary columns and manipulate data 
# data_aapl<- df_fin_reports[,-c(2:7,9,11,12,13,15:17)] %>% 
#   set_colnames(c("company","year","metric","value")) %>% 
#   reshape2::melt(c("company","year","metric"),"value") %>%
#   reshape2::dcast(company + year ~ metric)
# 
# metrics_list <- c("EarningsPerShareBasic","CommonStockDividendsPerShareDeclared","GrossProfit","NetIncomeLoss")
# data_aapl <- data_aapl[,c(which(colnames(data_aapl) %in% c("company","year")), which(colnames(data_aapl) %in% metrics_list))]
# 
# head(data_aapl)
# Helper function to get_valid_tickers to check if a URL is valid or not

# valid_url <- function(url_in, t=2){
#   con <- url(url_in)
#   check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
#   suppressWarnings(try(close.connection(con),silent=T))
#   ifelse(is.null(check),TRUE,FALSE)
# }
# 
# # Function to return a subset of the parameter tickers_list based on whether or not the sec
# # has data corresponding to the tickers in the list for the given year
# get_valid_tickers <- function(year = 2015, foreign_bool = FALSE, tickers_list){
#   list_valid_tickers <- vector()
#   for(i in 1:as.numeric(length(tickers_list))){
#     n <- as.numeric(length(tickers_list))
#     ticker <- tolower(as.character(tickers_list[i]))
#     message(paste0('\n',"Trying ticker ",i," of ",n,"."))
#     tryCatch(
#       expr = {
#         # Try the following functions
#         repo <- AnnualReports(ticker, foreign = foreign_bool)
#         idx = which(as.numeric(year(repo$filing.date)) == year)
#         filling_no <- repo$accession.no[idx] %>% str_remove_all("-")
#         CIK_NO <- get_CIK(ticker)
#         
#         # Trying the URL to see if valid
#         url_b <- paste0("https://www.sec.gov/Archives/edgar/data/",CIK_NO,"/",filling_no,"/Financial_Report.xlsx")
#         
#         # If the url is valid, append it to the list of valid tickers
#         if(valid_url(url_b)){
#           list_valid_tickers <- append(list_valid_tickers,ticker)
#           message(paste0("Successly processed ticker ",ticker))
#         }
#         
#       },
#       error = function(e){
#         closeAllConnections()
#         # Print out failure message 
#         message(paste0(ticker,": invalid ticker, year or foreign logical"))
#       }
#     )
#     closeAllConnections()
#   }
#   return(list_valid_tickers)
# }
# Get a character vector containing the set of tickers that a.) pay dividends, b.) are listed in SEC for 
# 2015 and c.) are domestic. Do so by subsetting div_ticks character vector.
# all_sec_div_tickers <- get_valid_tickers(year = 2015, foreign = FALSE, tickers_list = div_ticks$tickers)


# # Combine data from ex_format_list2 which is not missing any columns 
# data_dim <- as.numeric(lapply(ex_format_list2, function(x){ncol(data.frame(x))}))
# ex_forma_full <- ex_format_list2[which(data_dim==8)] %>% rbindlist() %>% data.frame() %>% set_colnames(c('ticker','name','metric','y1','y2','y3','units1','units2'))
# # That which is missing 1 column but underlying data is still good
# ex_forma_partial <- ex_format_list2[which(data_dim==7)] %>% rbindlist() %>% data.frame() %>% add_column(NA, .after = 5) %>% set_colnames(c('ticker','name','metric','y1','y2','y3','units1','units2'))
# 
# # Combine the two together to get the final dataframe to be parsed for metrics 
# ex_forma_full <- rbind(ex_forma_full,ex_forma_partial)
# write.csv(ex_forma_full, file = "fin_data.csv")
# 
# # Combine list dataframes with rbindlist 


# DEPENDENT ON EX_FORMAT: set the aliases for net income for the first 232 stocks you have
# by iterating through all of the available data and hand selecting the best metric
# net_income_names <- set_metrics("net_income")


# Test out your parse units functions. First item in character array returned is the unit multipliers and 
# the second item returned is the share multiplier. Check the function by looking at the units1 and units2
# columns with the index i. 
# i = 38
# parse_units(ex_format_data$units1[i], ex_format_data$units2[i])
# 
# # Get some of the indicators for headers above eps data 
# eps_headers <- set_metrics("eps_header",ex_forma_full)
# headers_clue <- na.omit(eps_headers) %>% tolower() %>% str_replace_all("[^0-9A-Za-z///' ]","") %>% unique()
# 
# # Get some of the direct (basic) eps indicators (stopped on 119 of 263)
# eps_alias <- set_metrics("eps_basic",ex_forma_full)
# eps_clue <- na.omit(eps_alias) %>% tolower() %>% str_replace_all("[^0-9A-Za-z///' ]","") %>% unique()
# eps_clue
# 
# # Get a character vector with ALL of the words in each of the eps_clues 
# all_words <- vector()
# for(i in 1:length(eps_clue)){
#   str <- strsplit(eps_clue, ' ')[[i]]
#   for(l in 1:length(str)){
#     all_words <- append(all_words, str[l])
#   }
# }
# 
# # Get a data frame with all of the eps_words and the number of their occurences in all_words 
# eps_words <- unique(all_words)
# n <- length(eps_words)
# for(i in 1:n){
#   count_total[i] <- sum(str_count(all_words, eps_words[i]))
# }
# 
# # Add a col for number of lines in which 
# eps_words_data <- data.frame(eps_words, count) 
# eps_words_data <- eps_words_data[order(eps_words_data$count, decreasing = TRUE),]
# # Take out the word 'in' and empty char as well as anything with count less than 5.
# eps_words_data <- eps_words_data[-c(which(eps_words_data$eps_words %in% c("in","")),which(eps_words_data$count < 5)),]
# eps_words_data
# 
# # Take the top 4 of eps_words and place in a character vector. Add the word total to the list as well, exclude 'inc'.
# hits_eps <- c(as.character(eps_words_data[which(eps_words_data[c(1:9),1]!="inc"),1]),"total","dividends","dividend")
# hits_eps

# # See how many of the lines of eps_clue contain one of the words in hit_eps
# for(i in 1:length(eps_clue)){
#   print(paste0(eps_clue[i], ": ", sum(str_detect(as.character(strsplit(eps_clue[i]," ")), hits_eps))))
# }
# Looks like we have covered all of the stocks! (119 at least). This is a good seed for our algorithmn to 
# find all of the eps rows in the dataframe. 

# Search through a subset of the data to find 5 rows with absolute value minimum vals 
#i = 20
#tick = unique(data_source$ticker)[i]
#tick = 'aapl'
#data_source = ex_forma_full
#subset[order(abs(as.numeric(subset$y1)), decreasing = FALSE), 4]

# # Order the subset dataframe by the rowwise mean of the three year variables.
# # Using the mean allows us to ignore NA values in our computations, thus allowing even a single row occurence in one of 
# # the three columns to be picked up on. Likewise, any row where the three cols are NA will be placed at the bottom of the frame. 
# order_subset <- subset[order(rowMeans(abs(data.frame(as.numeric(subset$y1),as.numeric(subset$y2),as.numeric(subset$y3)))), decreasing = FALSE), ]
# order_subset[c(1:10),]

# Combine data from ex_format_list2 which is not missing any columns
# data = ex_format_data1
# data_dim <- as.numeric(lapply(data, function(x){ncol(data.frame(x))}))
# 
# ex_forma_full <- data[which(data_dim==8)] %>% rbindlist() %>% data.frame() %>% set_colnames(c('ticker','name','metric','y1','y2','y3','units1','units2'))
# ex_forma_partial <- data[which(data_dim==7)] %>% rbindlist() %>% data.frame() %>% add_column(NA, .after = 5) %>% set_colnames(c('ticker','name','metric','y1','y2','y3','units1','units2'))
# 
# # Combine the two together to get the final dataframe to be parsed for metrics
# ex_forma_full <- rbind(ex_forma_full,ex_forma_partial)
# write.csv(ex_forma_full, file = "fin_data.csv")

# # Get all of the tickers in the first 1000 stocks (240 in total)
# ex_format_list <- compact(ex_format_list)
# ticks_name <- vector()
# for(i in 1:length(ex_format_list)){
#   df <- ex_format_list[[i]]
#   ticker = df$ticker[1]
#   ticks_name <- append(ticks_name, ticker)
# }
# 
# # First 15 valid tickers sample of data formatted correctly
# ex_format_data <- get_fin_list(2015, FALSE, tickers_list = ticks_name[1:15]) %>% 
#   rbindlist() %>% 
#   set_colnames(c("ticker","company_name","Metric","Y1","Y2","Y3","units1","units2"))

# for(i in 1:1000){
#   tryCatch(
#     expr = {
#       year <- 2015
#       tick <- as.character(div_ticks$tickers[as.numeric(i + 3000)])
#       filing_no <- get_accession_no(tick, year, FALSE)
#       message(paste0("Ticker ", tick, " processed successfully for accession number."))
#       message(paste0("Number: ", filing_no))  
#     },
#     error = function(e){
#       closeAllConnections()
#       message(paste0("Ticker ", tick, " failed to retrieve acc. number for ", year,"."))
#     }
#   )
#   closeAllConnections()
# }

#div_yield_data <- inner_join(a, div_data, by = "dates") %>% set_colnames(c("stock_value","dates","dividend")) 
#div_yield_data$div_yield = div_yield_data$dividend / div_yield_data$stock_value
#div_yield_data <- data.frame(div_yield_data, tick)


# Now plot your monthly income dividend growth over time
# plot_divs <- function(div_growth_model1){
#   monthly_income_graph <- ggplot(data = div_growth_model1, mapping = aes(x = Years)) + geom_line(mapping = aes(y = monthly_income, color = "Dividend Income"),size = 1) +
#     geom_line(mapping = aes(y = monthly_income_goal, color = "Income Goal"),size = 1) + geom_line(mapping = aes(y = four_percent_mnthly, color = "4% Stock Liq."), size = 1) + 
#     labs(title = "Monthly Income from investments",x = "Years from Initial Investment", y = "Monthly Income") +
#     #scale_color_manual(name = "Legend", values = c("Dividend Income" = "#0acc00", "Income Goal" = "#e63535", "4% Stock Liq." = "#e65656")) + 
#     scale_x_continuous(limits = c(0, 50)) + 
#     scale_y_continuous(limits = c(0,20000), labels = comma)
#   return(monthly_income_graph)
# }

# # DEPRICATED SHINY SERVER CODE 
# output$monthly_invest <- renderText({
#   data <- generate_model()
#   paste0(
#     " In order to meet your retirement income goal ",
#     as.character(input$retire_age),
#     " years from now, you will need at least ",
#     as.character(round(data[((input$retire_age*12)-1),which(colnames(data)=="monthly_income_goal")],0)),
#     " dollars a month to achieve a yearly purchasing power equal to the ",
#     as.character(input$retire_income),
#     " in today's dollars."
#   )
# })
# # DEPRICATED
# output$monthly_invest2 <- renderText({
#   data <- generate_model()
#   paste0(
#     "Your investment of ",
#     as.character(round((input$current_income/12)*(input$pct_invested/100),0)),
#     " dollars every month will yield ",
#     as.character(round(data[((input$retire_age*12)),which(colnames(data)=="monthly_income")],0)),
#     " dollars a month in dividend payments in ",
#     as.character(input$retire_age),
#     " years."
#   )
# })
# # DEPRICATED 
# output$div_monthly_income <- renderText({
#   data <- generate_model()
#   earnings <- as.character(format(round(data[((input$retire_age*12)),which(colnames(data)=="monthly_income")],0),big.mark = ",", scientific = F))
#   
#   if(data[((input$retire_age*12)),which(colnames(data)=="monthly_income")] > 0){
#     paste0("<h1><b>","<font color=\"#40e32d\">", "$", earnings,"</b></h1></font>", "<h5><b>"," div. earnings / month","</b></h5>")
#   }else{
#     paste0("<h1><b>","<font color=\"#FF0000\">", "$", earnings,"</b></h1></font>", "<h5><b>"," div. earnings / month","</b></h5>")
#   }
# })


# output$pic_strat <-renderPlot({
#   if(input$strategy_button == "Options"){
#     
#   }else if(input$strategy_button == "Day Trading"){
#     
#   }else if(input$strategy_button == "Penny Stocks"){
#     
#   }else if(input$strategy_button == "Value Investing"){
#     
#   }else if(input$strategy_button == "Growth Investing"){
#     
#   }else if(input$strategy_button == "Savings Account"){
#     
#   }else if(input$strategy_button == "CDs"){
#     
#   }else if(input$strategy_button == "Money Market Accounts"){
#     
#   }else if(input$strategy_button == "Short-term Corporate Bonds"){
#     
#   }else if(input$strategy_button == "Dividend Growth Investing"){
#     
#   }else if(input$strategy_button == "Indexing / ETFs"){
#     
#   }else if(input$strategy_button == "Long-term Treasury Bonds"){
#     
#   }else if(input$strategy_button == "Blue Chip Stocks"){
#     
#   }else{}
# })