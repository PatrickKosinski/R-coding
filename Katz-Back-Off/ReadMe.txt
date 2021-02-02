Patrick Kosinski 2021/02/01

This github-repository shows every part of the implementation of the Katz-Back-Off word prediction model. This was part of coursera's data science capstone.
Due to copyright concerns, I decided to not upload the data. For some theoretical background on Katz-Back-Off, please refer to https://rpubs.com/PatrickKosinski91/719470

The shiny app can be found under the following link:

https://patrickkosinski91.shinyapps.io/Katz_Back_Off_Patrick_Kosinski/

The repository is structured as follows:

- 1_0_exploratory_analysis: Here, the data has been uploaded into R and cleaned. Afterwards basic statistics have been calculated.

- 2_0_prediction_model: After cleaning and processing (i.e. forming unigrams, bigrams and trigrams) the prediction model has been implemented on the training data set.

- 3_0_validation: The third step was to do a quick validation of the model parametrization. Here, I used the test data set. 

- 4_0_shiny_app: Here, "everything has been put together". You find the implementation of the final data product. Please find the link to the data product above.


