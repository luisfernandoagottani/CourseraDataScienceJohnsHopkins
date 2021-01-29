SlideDeck
========================================================
author: Luis Fernando Rodrigues Agottani
date: November 11th, 2019
autosize: true

Introduction
========================================================

The final project from Johns Hopkins  Data Science Specialization from Coursera. The goal here is to build an App that predict the next word while you type. The data are  from blogs, twitter and news, the link for the Data is [HERE](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip). The data were made avaible by the course.

Loading Data, Exploratory Data Analysis, Cleaning and n-grams.
========================================================

The data was loaded from the link avaible and the Exploratory Data Analysis was made to understand the volume of the data. 
To cleaning and create the n-grams, first were made samples files with 1% of the each data type (Twitter, Blogs and news) to make it possible, because the PC couldn't run the full data.
The cleaning were made to remove ponctuation, put every letter to lower case, removed numbers and striped white spaces, this cleaning is very helpful to count n-grams and get a result with more precision.
The n-grams are bags of words, were created one gram, bigrams, trigrams and tetragram for the prediciton mode.

The Prediction
========================================================

The prediction algorithm is the Stupid Back Off model, The system works with the 3 last words of the sentence typed from client and find the most probable fourth word in the fourgram data, if it doesn't match, it goes to the threegram, with the last 2 words, than, if it doesn't match, it goes to the bigram with the last word, if it doesn't match, it goes to the one gram and takes the most useble word.

How the App works
========================================================

The apps is simple, the client just type the words in the box and it gives the most problable word right in the side of the box. I did it simple because it need to be simple, the goal is to see clearly what word has more probability to be the next one when we are typing, so I haven't done anything more than that. I hope you enjoy.

The app is hosted in the following link:
[ShinyServer](https://luisfernandoagottani.shinyapps.io/CapstoneProject/)

The application can be found in the following link:
[GitHubRepository](https://github.com/luisfernandoagottani/CapstoneProject)


