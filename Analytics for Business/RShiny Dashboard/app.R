library(shiny)
library(DT) 
library(ggplot2)
library(cluster)
library(stats)
library(caret)
library(arules)
library(rpart)
library(rpart.plot)
library(plyr)
library(curl)
library(arulesViz)
library(datasets)
library(car)
library(e1071)
library(broom)
library(psych)
library(PerformanceAnalytics)
library(neuralnet)
library(aod)
library(klaR)
library(MASS)
library(lattice)
library(ggplot2)
library(png)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  headerPanel("Valve Corporation 2018 Video Game Production Analysis"),
  #sidebarLayout that will be present on the left of each tab
  sidebarPanel(
    # When About tab selected the following output will appear in the side panel ----
    conditionalPanel(
      'input.tabs === "About"',
      strong("The purpose of this capstone project is for students to explore business analytics problem solving by developing
      business analytics solution to a real-world problem. Once the problem is selected all elements of the business analytics problem 
      must be worked throug using the INFORMS methodology.")
      ),
    # When Data tab selected the following output will appear in the side panel ----
    conditionalPanel(
      'input.tabs === "Data"',
      checkboxGroupInput("variables", "Columns in Dataset to Show",
                         colnames(CompleteDataset), selected = colnames(CompleteDataset))
    ),
    # When Clustering Method tab selected the following output will appear in the side panel ----
    conditionalPanel(
      'input.tabs === "Clustering Method"',
      strong("Choosing a Clustering Algorithm"),
      helpText("Now that the distance matrix has been calculated,
               it is time to select an algorithm for clustering. While many algorithms that can handle a 
               custom distance matrix exist, partitioning around medoids (PAM) will be used here. If you 
               know the k-means algorithm, this might look very familiar. In fact, both approaches are identical, 
               except k-means has cluster centers defined by Euclidean distance (i.e., centroids), while cluster 
               centers for PAM are restricted to be the observations themselves (i.e., medoids)."),
      # Output: Header + explanation on why Gower's method was chosen ----
      h3("Calculating Distance"),
      helpText("In order for a yet-to-be-chosen algorithm to group observations 
                        together, we first need to define some notion of (dis)similarity 
                        between observations. A popular choice for clustering is Euclidean distance. 
                        However, Euclidean distance is only valid for continuous variables, 
                        and thus is not applicable here. In order for a clustering algorithm 
                        to yield sensible results, we have to use a distance metric that can handle 
                        mixed data types. In this case, we will use something called Gower distance. 
                        The Gower distance fits well with the k-medoids algorithm. k-medoid is a classical 
                        partitioning technique of clustering that clusters the data set of n objects into 
                        k clusters known a priori. To execute Gower distance, we used the daisy function."),
      
      # Output: Secondary Header + summary of distance ----
      strong("Gower's Dissimilarity Distance:"),
      verbatimTextOutput("gowerplot"),
      
      # Output: Header + explanation on why PAM method was chosen ----
      h3("Selecting the Numbers of Clusters"),
      helpText("A variety of metrics exist to help choose the number of clusters to be extracted 
                        in a cluster analysis. We will use silhouette width, an internal validation metric 
                        which is an aggregated measure of how similar an observation is to its own cluster 
                        compared its closest neighboring cluster. The metric can range from -1 to 1, where 
                        higher values are better."),
      # Output: Secondary Header + plot of suggested clusters using sil_width() ----
      strong("Clustering with PAM using Silhouette Width:"),
      plotOutput("silwdplot"),
      helpText("After calculating silhouette width for clusters ranging 
                        from 2 to 10 for the PAM algorithm, we see that 5 clusters yields the highest value."),
      # Output: Header + recommendation from clustering method ----
      h3("Cluster Method Recommendation"),
      strong("A video game launch with a similar gameplay as Cluster 3 would prove successful for Valve
              Corporation. Cluster 3 is a video game rated M for mature. It is an Action focused gaming experience
              with a Science Fiction based setting and narrative, played from the First-Person Perspective.")
      ),
    # When Correlation Pearson Method tab selected the following output will appear in the side panel ----
    conditionalPanel(
      'input.tabs === "Correlation Pearson Method"',
      h3("Model Question:"),
      helpText("What is the correlation between sales, tournaments and scores (by critics and users)? Correlation 
               Pearson method was performed to address this question."),
      # Output: Second Header + recommendation from correlation pearson method ----
      h3("Correlation Pearson Method Recommendation"),
      strong("Since tournament Money awarded is fairly correlated with total tournament players, it’s recommended
              to consider this issue when developing the new game. It will be helpful to design the game in a way
             to be compatible with tournaments and tournaments awards. It’s recommended to put into consideration 
              critics score as it’s fairly correlated with sales and also number of players (in tournaments).")
      ),
    # When Decision Tree Method tab selected the following output will appear in the side panel ----
    conditionalPanel(
      'input.tabs === "Decision Tree Method"',
      h3("Model Questions"),
      radioButtons("question", label = NULL, list("How is global sales affected by genres, ESRB ratings, number of 
                                                   tournaments and number of players?" = "Q1","How is the number of 
                                                   tournaments global sales affected by genres, 
                                                   ESRB ratings, number of tournaents and number of players, and total 
                                                   money awarded (during tournaments) regardless of genre?" = "Q2"), 
                                                   selected = "Q1"),
      # Output: Second Header + summary of Decision Tree Model ----
      h3("Decision Tree Model Summary"),
      textOutput("Summary"),
      br(),
      verbatimTextOutput("dt_summary")
    ),
    # When Poisson Regression Method tab selected the following output will appear in the side panel ----
    conditionalPanel(
      'input.tabs === "Poisson Regression Method"',
      h3("Model Question:"),
      helpText("How does games’ genres effect number of tournaments players? Poisson Regression method 
               was performed to address this question."),
      h3("Poisson Regression Model Summary"),
      verbatimTextOutput("Pos_Sum")
      ),
    # When ANOVA Method Method tab selected the following output will appear in the side panel ----
    conditionalPanel(
      'input.tabs === "ANOVA Method"',
      h3("Model Question:"),
      helpText("Do different types of genres achieve the same average of global sales? ANOVA method 
               was performed to address this question."),
      # Output: Fifth Header + recommendation from ANOVA method ----
      h3("ANOVA Method Recommendation"),
      strong("For the new developed game, it’s recommended to consider the genres of First-Person
             Perspective, Strategy, Fighting, Platform, and Beat.emUp (beat them up) to achieve higher sales.")
    ),
    # When Conclusions tab selected the following output will appear in the side panel ----
    conditionalPanel(
      'input.tabs === "Conclusions"',
      h3("Genre Model Cycle"),
      strong('To maintain the model’s benefits, it’s important to have a defined model life cycle for the variables 
             related to genres and tournaments. Model life cycles are created for the continuous collection of video 
             game data and the testing of old models, in order to see if they are still working or not. If the old models 
             are not working then it’s important to analyze the new data and extract new models that would help in understanding 
             how to achieve higher sales and game players. To achieve this, the Genres Model Cycle was created to help produce 
             new models and suggest the genres of future sequel releases of a video game.'),
      tags$img(src='GenreModel.png'),
      br(),
      helpText('By collecting data of other games, it will help valve corporation to test the old model, and see if it’s still 
              working or not. If the model is still working then it’s important to continue in acquiring data to keep checking periodically 
              if the models still stand or not. If the models are not working then it’s important to analyze the new data of games and 
              check if there are new models that suggests different genres. The new suggested genres will take part in forming new sequel 
              of the game, and this will help in maintaining and achieving higher sales.'),
      h3("Tournament Model Cycle"),
      strong('Designing and maintaining interesting tournaments for video games is important in order to achieve high sales and high number 
            of tournament players. Decision trees models help to understand how sales are affected by number of tournaments, players, and money 
             awarded. It’s important to have a model life cycle in order to evaluate the models over time, in order to know what’s the best method 
             in managing tournaments. The model life cycle is illustrated in Figure 5.'),
      tags$img(src='TournamentModel.png'),
      br(),
      helpText('By collecting data of other games, it will help Valve Corporation to test the old model, and see if it’s still working or not. 
              If the model is still working, then it’s important to continue in acquiring data, to keep checking periodically, in order to see 
              if the models still stand or not. If the models are not working, then it’s important to analyze the new data of games and check if 
              there are new models that suggests new tournament management. The new models will help in deciding number of tournaments and players, 
              in addition to money awarded that achieve higher sales.')
    )
    ),
  #-----------------------------------------#
  # Main panel for displaying outputs ----
  mainPanel(
    # Layout format with tabs ----
    tabsetPanel(
      id = 'tabs',
      # Call for each tab in the panel ----
      tabPanel("About",
               #Output: Header to announce more info below + selection list ----
               h3("Scroll to Learn More About This Project"),
               tags$img(src='TitlePg.png'),
               tags$img(src='ValveCorp.png'),
               tags$img(src='Overview.png'),
               tags$img(src='BusProb.png'),
               tags$img(src='AnaProb.png'),
               tags$img(src='Objectives.png'),
               tags$img(src='Method.png')
      ),
      tabPanel("Data", 
               # Output: Header + view CompleteDataset.csv file ----
               h3("Complete Dataset"),
               p('The CompleteDataset 
                 focuses on variables that may give potential insight into player 
                 experience when playing a video game. There are 171 rows of variables, 
                 referring to video game totles and 32 columns referring to unique 
                 information about each  video game. A summary of the dataset can be seen below.'),
               
               #display full CompleteDataset datable ----
               DT::dataTableOutput("CD"),
               
               # Output: Header + summary of CompleteDataset ----
               h3("Summary"),
               verbatimTextOutput("sumCompPlot"),
               tags$head(tags$style("#clickGene{color:red; font-size:12px; font-style:italic; 
                overflow-y:scroll; max-height: 20px; background: ghostwhite;}"))
      ),
      tabPanel("Clustering Method",
               # Output: Header + summary of each cluster ----
               h3("Summary of 5 Clusters"),
               helpText('Choose a Cluster Summary for Review.'),
               numericInput("choice",label = NULL, list("1"="1","2"="2","3"="3","4"="4","5"="5"), min = 1, max = 5),
               verbatimTextOutput("sil_sum"),
               
               # Output: Second Header + performance of each cluster verses dataset as a whole ----
               h3("Cluster Interpretation"),
               helpText("Here is a side-by-side comparison of the five clusters versus the complete dataset"),
               tags$img(src='Inter.png')
      ),
      tabPanel("Correlation Pearson Method",
               # Output: Header + explanation on why PAM method was chosen ----
               h3("Variables"),
               helpText("The variables: UserScore, CriticScore, GlobalSales, TournamentMoneyAwarded, TournamentTotalPlayers,
                TotalTournaments were selected for this method."),
               
               # Output: Secondary Header + plots of Correlation Pearson Method Results) ----
               h3("Correlation Pearson Method Results"),
               helpText('Click "Correlations" or "Pairs" below for visualization of model results'),
               radioButtons("click", label = NULL, list("Pairs"="corr_plot", "Correlation"="corr"), selected="corr_plot"),
               plotOutput("corr_results"),
               helpText("With p-value less than the significant alpha (0.05) and with consideration of coefficient r, 
                the correlation plot suggests strong association between:
                        • Strong positive correlation between tournament money awarded and total tournament; r= 0.5
                        •	Medium strength of positive correlation between total tournaments and total tournament players; r=0.44
                        •	Medium strength of positive correlation between tournament Money awarded and total tournament players; r=0.35
                        •	Medium strength of positive correlation between critics score and global sales; r=0.35
                        •	Medium strength of positive correlation between critics score and tournaments total players; r=0.31
                        .")
      ),
      tabPanel("Decision Tree Method",
               # Output: Header + list of variables used ----
               h3("Variables"),
               p('The variables: ESRB ratings, 22 types of Genres (logical variables), TournamentTotalPlayers, TotalTournaments
                 were selcted for this method.'),
               
               # Output: Secondary Header + plots of Decision Tree Model ----
               h3("Decision Tree Model Results"),
               plotOutput("dt_tree"),
               textOutput("SizeExp"),
               plotOutput("dt_trees"),
               textOutput("TreeExp"),
               br(),
               # Output: Third Header + recommendation from decision tree method ----
               h3("Decision Tree Method Recommendation"),
               tags$img(src='DT.png')
      ),
      tabPanel("Poisson Regression Method",
               # Output: Header + list of variables used ----
               h3("Variables"),
               p('The TournamentTotalPlayers variable was set as the response. The explanatory variables are the 22 types of 
                 Genres (logical variables).'),
               
               # Output: Secondary Header + plots of Poisson Regression Model ----
               h3("Poisson Regression Model Results"),
               p('With p-value less than the significant alpha (0.05), the model suggest that number of tournament players 
                 is affected by some genres. Genres that have positive relationship with number of total players (in tournament) 
                 are (descending order by number of coefficient):'),
               tags$img(src='Pos.png'),
               p('Genres that have negative relationship with number of total players (in tournament) are (descending order by 
                  number of coefficient):'),
               tags$img(src='Neg.png'),
               h3("Plot of Sales"),
               plotOutput("Pos_Plots"),
               # Output: Fourth Header + recommendation from poisson regression method ----
               h3("Poisson Regression Method Recommendation"),
               tags$img(src='PosReg.png')
      ),
      tabPanel("ANOVA Method",
               # Output: Header + list of variables used ----
               h3("Variables"),
               p('The Global_Sales variable was set as the response. The explanatory variables are the 22 types of 
                 Genres (logical variables).'),
               h3("ANOVA Model Summary"),
               verbatimTextOutput("ANOVA_Sum"),
               tags$img(src='ANOVA.png')
      ),
      tabPanel("Conclusions",
               # Output: Fifth Header + recommendation from ANOVA method ----
               h3("Genre Findings:"),
               tags$img(src='GenreFind.png'),
               h3("Genre Results:"),
               tags$img(src='GenreResults.png'),
               br(),
               h3("ESRB Findings & Results:"),
               tags$img(src='ESRB Results.png'),
               br(),
               h3("Recommended Video Game Profile:"),
               tags$img(src='Conclusion.png')
      )
      )
      )
      )

# Define server logic to summarize and view dataset ----
server <- function(input, output) {
  
  # Display of CompleteDataset ----
  Complete = CompleteDataset[sample(nrow(CompleteDataset), 100),]
  output$CD <- DT::renderDataTable({
    DT::datatable(Complete[,input$variables, drop = FALSE])
  })
  # Function to perform Gower's Distance + print summary ----
  gower <- function(){
    summary(gd)
  }
  output$gowerplot <- renderPrint({
    print(gower())
  })
  # Function to view summary of Complete3 dataset ----
  sumComp <- function(){
    summary(Complete3)
  }
  output$sumCompPlot <- renderPrint({
    print(sumComp())
  })
  # Function to calculate silhouette width on Complete3 dataset ----
  silwd <- function(){
    plot(1:10, sil_width, xlab = "Number of Clusters", ylab = "Silhouette Width")
    lines(1:10, sil_width)
  }
  output$silwdplot <- renderPlot({
    print(silwd())
  })
  #Function to view a different cluster summary one at a time----
  output$sil_sum <-renderPrint({
    choice <- switch(input$choice, "1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5)
      results$the_summary[[(choice)]]
  })
  # Function to view text explanation of summary output ----
  Q_Sum <- function(){
    if(input$question == "Q1"){
      print.noquote('Importance of variables in this decision tree are: 1st - TotalTournaments, 2nd - TounamentTotalPlayers, 
                  3rd - ESRB Ratings, 4th - Fighting (genre), 5th - Simulator (genre).')
    }else{
      print.noquote('Importance of variables in this decision tree are: 1st - CriticScore, 2nd - TournamentMoneyAwarded, 3rd - TournamentTotalPlayers,
                    4th - UserScore, 5th - TotalTournaments.')
    }
  }
  output$Summary <- renderText({
    print(Q_Sum())
})
#Function to view summary of Decision Tree Model---- 
  dt_sum <- function(){
    if(input$question == "Q1"){
      summary(tree_sales)
    }else{
      summary(tree_sales2)
    }
  }
  output$dt_summary <- renderPrint({
    print(dt_sum())
})
# Function to view text explanation of tree size output ----
  Q_Size <- function(){
    if(input$question == "Q1"){
      print.noquote('The CP plot indicates that optimal size of the decision tree is 6.')
    }else{
      print.noquote('The CP plot indicates that optimal size of the decision tree is 8.')
    }
  }
  output$SizeExp <- renderText({
    print(Q_Size())
})
  #Function to view the tree size for the Decision Tree Model---- 
  dt_plot <- function(){
    if(input$question == "Q1"){
      plotcp(tree_sales)
    }else{
      plotcp(tree_sales2)
    }
  }
  output$dt_tree <- renderPlot({
    print(dt_plot())
})
# Function to view text explanation of tree size output ----
  Q_Tree <- function(){
    if(input$question == "Q1"){
      print.noquote('The decision tree above suggests that with low number of tournaments (<18 tournaments), there is still 
                 chance to achieve higher global sales by choosing simulator genre. With higher number of tournaments 
                    (≥18 tournaments), there is a chance to achieve higher global sales with ESRB ratings of E(Everyone) and 
                    M(Mature). If ESRB ratings of T(teen) was chosen, then there is a chance to have lower global sales.')
    }else{
      print.noquote('The decision tree above suggests that with critic score less than 88, there is still chance to achieve 
                    higher global sales by having a high tournament money awarded (>= 1,200,000). If tournament money awarded 
                    is less (<1,200,000), having ESRB ratings of E (everyone) and M(Mature) will help in having higher global sales. 
                    While having T(teen) rating will decrease sales.With higher critic score (≥88 score), there is a chance to achieve 
                    higher global sales specially with having more players (>=81 players) in tournaments.')
    }
  }
  output$TreeExp <- renderText({
    print(Q_Tree())
})
  #Function to view the decision tree plots for the Decision Tree Model---- 
  dt_plots <- function(){
    if(input$question == "Q1"){
      rpart.plot::rpart.plot(tree_sales, type = 4)
    }else{
      rpart.plot::rpart.plot(tree_sales2, type = 4)
    }
  }
  output$dt_trees <- renderPlot({
    print(dt_plots())
})
  #Function to view the Correlation Model Pairs plotted or Correlation Matrix---- 
  output$corr_results <- renderPlot({
    click <- switch(input$click, "corr" = chart.Correlation(cor,method="pearson",histogram = TRUE, pch=16), 
                    "corr_plot" = pairs(data = CompleteDataset, ~UserScore+CriticScore+Global_Sales+
                                    TournamentMoneyAwarded+TournamentTotalPlayers+TotalTournaments))
      results$click[[]]
  })
  #Function to view the Poisson Regression Model Summary---- 
  Pos_Reg <- function(){
    summary(lm_sales)
  }
  output$Pos_Sum <- renderPrint({
    print(Pos_Reg())
  })
  #Function to view the Poisson Regression Model plots---- 
  Pos_Reg2 <- function(){
    plot(lm_sales)
  }
  output$Pos_Plots <- renderPlot({
    print(Pos_Reg2())
  })
  #Function to view the ANOVA Model summary---- 
  A_Sum <- function(){
    summary(anova_sales_genre)
  }
  output$ANOVA_Sum <- renderPrint({
    print(A_Sum())
  })
}
# Create Shiny app ----
shinyApp(ui, server)


