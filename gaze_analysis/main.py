# Here there will be the code for analyzing the gaze data 
import os
import pandas as pd
import json
import questionnaire

# Analyzing the gaze data to determine if the gaze is distracted or not based on the gaze value and a threshold value, if the gaze value is greater than the threshold value then the gaze is not distracted else it is distracted
def gaze_analysis(gaze_data,threshold):
    gaze_data["distracted"] = gaze_data["gaze_value"].apply(lambda x: "no" if x>threshold else "yes")
    return gaze_data

# Counting the number of distracting gazes for the entire video by counting the number of times the gaze switches from distracted to not distracted or vice versa
def count_distracting_gazes(gaze_analysis_data):
    count = 0
    prev = gaze_analysis_data["distracted"][0] 
    for i in range(len(gaze_analysis_data)):
        if gaze_analysis_data["distracted"][i] != prev:
            count += 1
            prev = gaze_analysis_data["distracted"][i]
    return count

def main():
    # Reading the game data from the files and analyzing the game data
    games_paths = "gaze_analysis/games_data"
    games_df = pd.DataFrame()
    for file in os.listdir(games_paths):
        if file.endswith(".json"):
            with open(os.path.join(games_paths, file)) as json_file:
                game_data = json.load(json_file)
                game_data.popitem()
                game_df = pd.DataFrame.from_dict(game_data, orient='index').T
                # uppercase of the taskID
                game_df["taskID"] = game_df["taskID"].apply(lambda x: x.upper())
                games_df = pd.concat([games_df, game_df])

    # Reading the gaze data from the files and analyzing the gaze data             
    file_paths = "gaze_analysis/gaze_data"
    threshold = 0.9
    gazes_df = pd.DataFrame()
    for file in os.listdir(file_paths):
        if file.endswith(".txt"):
            gaze_data = pd.read_table(os.path.join(file_paths, file), sep = "," ,names=["frame_id","gaze_value"])
            
            gaze_analysis_data = gaze_analysis(gaze_data,threshold)
            distracting_gazes_count = count_distracting_gazes(gaze_analysis_data)
            percentage_of_distracting_gazes = gaze_analysis_data["distracted"].value_counts(normalize=True)["yes"].round(3)
            amount_of_seconds_distracted = percentage_of_distracting_gazes * len(gaze_data) / 30
            
            
            summary_df = gaze_analysis_data["gaze_value"].describe()[["mean","std","min","25%","50%","75%","max"]].to_frame().transpose()
            file_id = file.split(".")[0]
            summary_df.insert(0, "subjectID", file_id.split("_")[0])
            summary_df.insert(1, "platformID", file_id.split("_")[1])
            summary_df.insert(2, "taskID", file_id.split("_")[2])
            summary_df.insert(3,"video_duration", len(gaze_data)/30)
            summary_df.insert(4,"distracting_gazes_count", distracting_gazes_count)
            summary_df.insert(5, "percentage_of_distracting_gazes", percentage_of_distracting_gazes)
            summary_df.insert(6, "amount_of_seconds_distracted", amount_of_seconds_distracted)

            gazes_df = pd.concat([gazes_df, summary_df])
            

    summary_df = pd.merge(gazes_df, games_df, on=["subjectID","platformID","taskID"], how="left").drop_duplicates(subset=["subjectID","platformID","taskID"]).sort_values(by=["subjectID","platformID","taskID"])
    summary_df.to_csv("gaze_analysis/analysis_outputs/gaze_analysis_data.csv", index=False)
            
if __name__ == "__main__":
    main()
    


        

    
    
