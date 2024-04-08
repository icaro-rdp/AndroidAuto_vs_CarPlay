# Here there will be the code for analyzing the gaze data 
import os
import pandas as pd

# Analyzing the gaze data to determine if the gaze is distracted or not based on the gaze value and a threshold value, if the gaze value is greater than the threshold value then the gaze is not distracted else it is distracted
def gaze_analysis(gaze_data,threshold):
    gaze_data["distracted"] = gaze_data["gaze_value"].apply(lambda x: "no" if x>threshold else "yes")
    return gaze_data

# Counting the number of distracting gazes for the entire video by counting the number of times the gaze switches from distracted to not distracted or vice versa
def count_distracting_gazes(gaze_analysis_data):
    count = 0
    prev = gaze_analysis_data["distracted"][0] 
    for i in range(len(gaze_data)):
        if gaze_analysis_data["distracted"][i] != prev:
            count += 1
            prev = gaze_analysis_data["distracted"][i]
    return count

def structure_games_file():
    
    return


file_paths = "gaze_analysis/data"
threshold = 0.9
final_csv = pd.DataFrame()
for file in os.listdir(file_paths):
    if file.endswith(".txt"):
        gaze_data = pd.read_table(os.path.join(file_paths, file), sep = "," ,names=["frame_id","gaze_value"])
        
        gaze_analysis_data = gaze_analysis(gaze_data,threshold)
        distracting_gazes_count = count_distracting_gazes(gaze_analysis_data)
        
        summary_df = gaze_analysis_data["gaze_value"].describe()[["mean","std","min","25%","50%","75%","max"]].to_frame().transpose()
        id = file.split(".")[0]
        summary_df.insert(0, "player_ID", id.split("_")[0])
        summary_df.insert(1, "platform_ID", id.split("_")[1])
        summary_df.insert(2, "task_ID", id.split("_")[2])
        summary_df.insert(3,"distracting_gazes_count", distracting_gazes_count)

        final_csv = pd.concat([final_csv, summary_df])
games_paths = "gaze_analysis/games_data"
for file in os.listdir(games_paths):
     if file.endswith(".txt"):
         print(file)

final_csv.to_csv("gaze_analysis/gaze_analysis_summary.csv", index=False)
    


        

    
    
