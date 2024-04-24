This project investigates the relationship between public housing projects and crime rates in New York City. We leverage geospatial analysis techniques and machine learning models to explore potential geographical clusters of crime incidents and their association with public housing locations.

Data Sources:

    NYC Open Data:
        Public Housing Authority (NYCHA) building locations
        Crime incident data (including type, location, date)
    Additional geospatial data (optional):
        Neighborhood boundaries
        Socioeconomic indicators (poverty rates, median income)

Methodology:

    Data Preprocessing:
        Clean and format both datasets for spatial analysis in R.
        Geocode crime incident addresses if necessary.
        Prepare spatial data of NYC housing projects and potentially relevant neighborhood boundaries.
    Geospatial Analysis:
        Visualize the spatial distribution of crime incidents and public housing locations on a map.
        Utilize K-Nearest Neighbors (KNN) clustering to identify potential hotspots of crime activity.
    Machine Learning Modeling:
        Develop a Random Forest model to predict crime rates based on proximity to public housing projects and other relevant features (if available, like socioeconomic indicators).
        Evaluate the model's performance using appropriate metrics (accuracy, precision, recall).
    Analysis and Insights:
        Analyze the KNN clusters and identify any spatial patterns associated with crime hotspots.
        Interpret the results of the Random Forest model to assess the significance of public housing proximity on crime rates.
        Consider incorporating additional socioeconomic data to enhance the model's explanatory power (optional).
    Visualization and Communication:
        Create compelling visualizations (maps, charts) to effectively communicate the findings.
        Prepare a clear and concise report summarizing the methodology, results, and key takeaways.

Expected Outcomes:

    Identify potential spatial clusters of crime activity in NYC.
    Evaluate the association between public housing projects and crime rates.
    Gain insights into the geographical distribution of crime in relation to housing projects and potentially other socioeconomic factors (optional).
    Develop a predictive model to estimate crime risk based on location data.

Software and Tools:

    R programming language
    Geospatial libraries (e.g., sp, rgdal)
    Data visualization libraries (e.g., ggplot2)
    Machine learning libraries (e.g., caret, randomForest)

Project Significance:

This project can provide valuable insights for policymakers and law enforcement agencies in NYC. Understanding the spatial distribution of crime and its potential relationship with housing projects can inform targeted crime prevention strategies and resource allocation.
