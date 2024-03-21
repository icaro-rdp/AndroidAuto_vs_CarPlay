# Here there will be the code for analyzing the gaze data 
import os
import pandas as pd

file_paths = "gaze_analysis/data"

for file in os.listdir(file_paths):
    if file.endswith(".txt"):
        gaze_data = pd.read_table(os.path.join(file_paths, file), sep = "\t")


