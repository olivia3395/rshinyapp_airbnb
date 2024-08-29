
# Airbnb Exploration - Find Your Ideal Home

![Airbnb Logo](./airbnb logo.jpg)

## Overview

The **Airbnb Exploration App** is an interactive tool designed to help users discover the perfect Airbnb property based on various customizable filters. Developed using **R Shiny**, this app integrates dynamic user interface elements and server-side logic to provide real-time data interaction and visualization. Whether you're searching by location, price range, or property type, this app offers a comprehensive solution to finding your ideal Airbnb stay.

![Airbnb Exploration](./airbnb_guide.jpg)

## Features

- **Dynamic Search Filters:** Customize your search based on location, price range, property type, and more.
- **Interactive Maps:** Explore rental properties geographically with an intuitive map interface.
- **Real-Time Data Visualization:** Visualize booking trends and other relevant data with interactive charts powered by `ggplot2` and `plotly`.
- **Scalable Performance:** Efficient handling of large Airbnb datasets using `dplyr` and `data.table` for optimized server-side computations.
  

## Installation

### Prerequisites

Ensure you have the following installed:

- [R](https://cran.r-project.org/)
- [RStudio](https://rstudio.com/)
- Shiny package: `install.packages("shiny")`
- Additional packages: `ggplot2`, `plotly`, `dplyr`, `data.table`, `leaflet`

### Setup

1. Clone this repository to your local machine:
   ```bash
   git clone https://github.com/yourusername/airbnb_exploration_app.git
   ```
2. Open the project in RStudio.

3. Install the required packages by running:
   ```r
   install.packages(c("shiny", "ggplot2", "plotly", "dplyr", "data.table", "leaflet"))
   ```

4. Run the Shiny app:
   ```r
   shiny::runApp('path_to_your_app_directory')
   ```



