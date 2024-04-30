import pandas as pd
import math

# Function to transform possible NaN values in a column to the average of the column if the column is numeric
def transform_NaN_to_col_avg(df, col_name):
    if pd.api.types.is_numeric_dtype(df[col_name]):
        col_avg = math.floor(df[col_name].mean())
        df[col_name] = df[col_name].fillna(col_avg)
    return df

# Function to transform the questionnaire with headers to a new questionnaire with the columns transformed to the correct format
def transform_questionnaire_df(questionnaire_df):
    
    participants_columns = ["Name","Surname","Age","Gender","Phone_type","license_years","AA_fam","CP_fam",]
    
    participants_df = questionnaire_df.iloc[:, 3:11]
    participants_df.columns = participants_columns

    
    aa_columns = ["AA_eou_1","AA_eou_2","AA_eou_3","AA_sus_1","AA_sus_2","AA_sus_3","AA_sus_4","AA_sus_5","AA_sus_6","AA_sus_7","AA_sus_8","AA_sus_9","AA_sus_10"]
    cp_columns = ["CP_eou_1","CP_eou_2","CP_eou_3","CP_sus_1","CP_sus_2","CP_sus_3","CP_sus_4","CP_sus_5","CP_sus_6","CP_sus_7","CP_sus_8","CP_sus_9","CP_sus_10"]
    android_df = questionnaire_df.iloc[:, 11:24]
    android_df.columns = aa_columns 
    apple_df = questionnaire_df.iloc[:, 24:37]
    apple_df.columns = cp_columns

   
    new_questionnaire_df = pd.concat([participants_df, android_df, apple_df], axis = 1)

    for col in new_questionnaire_df.columns:
        new_questionnaire_df = transform_NaN_to_col_avg(new_questionnaire_df, col)
        
    return new_questionnaire_df

def main():
    questionnaire_paths = "gaze_analysis/questionnaire_data/questionnaire.csv"
    questionnaire_df = pd.read_csv(questionnaire_paths, sep = ",")

    transformed_questionnaire_df = transform_questionnaire_df(questionnaire_df)

    transformed_questionnaire_df.to_csv("gaze_analysis/analysis_outputs/transformed_questionnaire.csv", index=False)
    

if __name__ == "__main__":
    main()

