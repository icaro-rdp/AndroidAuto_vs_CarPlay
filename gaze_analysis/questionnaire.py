import pandas as pd


# Function to transform possible NaN values in a column to the average of the column if the column is numeric
def transform_NaN_to_col_avg(df, col_name):
    if pd.api.types.is_numeric_dtype(df[col_name]):
        col_avg = df[col_name].mean()
        df[col_name] = df[col_name].fillna(col_avg)
    return df

# Function to transform the questionnaire with headers to a new questionnaire with the columns transformed to the correct format
def transform_questionnaire_df(questionnaire_df):
    
    participants_columns = ["Name","Surname","Age","Gender","Phone_type","license_years","AA_fam","CP_fam",]
    
    participants_df = questionnaire_df.iloc[:, 3:11]
    participants_df.columns = participants_columns

    
    platforms_columns = ["eou_1","eou_2","eou_3","sus_1","sus_2","sus_3","sus_4","sus_5","sus_6","sus_7","sus_8","sus_9","sus_10"]

    android_df = questionnaire_df.iloc[:, 11:24]
    android_df.columns = platforms_columns
    apple_df = questionnaire_df.iloc[:, 24:37]
    apple_df.columns = platforms_columns

   
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

