# install devtools if you don't have it
if (!require('devtools')) install.packages('devtools')
# check error messages and ensure that devtools installed properly. 

# install MLWIC2 from github
devtools::install_github("mikeyEcology/MLWIC2") 
# This line might prompt you to update some packages. It would be wise to make these updates.

# load this package
library(MLWIC2)

# For loading in Tensor Flow, you need to load in version 1.14 ideally (greater than 1.8 and less than 2.0)
# Make sure you are using a Python 3.6._ version, for example I used 3.6.8.
# call the following command in the cmd console:
# python -m pip install tensorflow===1.14

# Setup the environment for using MLWIC2
MLWIC2::runShiny('setup')
MLWIC2::setup(python_loc = 'C:/Users/Geoffrey/anaconda3')

classify(path_prefix = "", # path to where your images are stored
         data_info = "", # path to csv containing file names and labels
         model_dir = "", # path to the helper files that you downloaded in step 3, including the name of this directory (i.e., `MLWIC2_helper_files`)
         python_loc = "", # location of python on your computer
         save_predictions = "model_predictions.txt", # how you want to name the raw output file
         make_output = TRUE, # if TRUE, this will produce a csv with a more friendly output
         output_name = "MLWIC2_output.csv", # if make_output==TRUE, this will be the name of your friendly output file
         num_cores = parallel::detectCores() - 2 # the number of cores you want to use on your computer. Try runnning parallel::detectCores() to see what you have available. You might want to use something like parallel::detectCores()-1 so that you have a core left on your machine for accomplishing other tasks. 
)