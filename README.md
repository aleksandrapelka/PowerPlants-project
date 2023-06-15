# PowerPlants-project
Geoinformatics application showing the location of power plants in a selected country and in the world.

### The application is also available at: https://power-plants.shinyapps.io/power-plants/
#

The data used in the application has been collected from the following sources:

* https://ourworldindata.org/grapher/electricity-prod-source-stacked
(energy production in the world over the years)

* https://datasets.wri.org/dataset/globalpowerplantdatabase
(power, type, location of power plants)

##### The aim of this project 
was to create an application presenting information on various types of power plants, 
using the RShiny library, as well as the use of several reactive functions:

1. render()
2. reactive()
3. isolate()
4. observe()
5. observeEvent()
6. eventReactive()
7. reactiveValues()
8. updateSelectInput()

which allowed the displayed results to be updated depending on the user's choices.
