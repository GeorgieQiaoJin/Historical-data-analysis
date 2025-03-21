import pandas as pd

# Change these file names as needed:
INPUT_CSV = "data.csv"
OUTPUT_CSV = "new_idle_res_full.csv"

def get_idle_string(time_diff, idle_threshold_seconds=120):
    """
    Convert the time difference (in seconds) into a descriptive idle string.
    - For the first event (NaN time_diff) or gaps less than the threshold, 
      return "0, > 0 s".
    - For gaps between idle_threshold_seconds (default 120 s) and 300 s, return "1, > 2 min".
    - For gaps 300 s or more, return "1, > 5 min".
    """
    if pd.isna(time_diff):
        return "0, > 0 s"
    if time_diff < idle_threshold_seconds:
        return "0, > 0 s"
    elif time_diff < 300:
        return "1, > 2 min"
    else:
        return "1, > 5 min"

def apply_idle_detector(input_csv, output_csv, idle_threshold_seconds=120):
    # Read the CSV file into a DataFrame.
    df = pd.read_csv(input_csv)
    
    # Convert the "Time" column to datetime objects.
    df['Time'] = pd.to_datetime(df['Time'])
    
    # Sort by session and then by time to ensure proper order.
    df = df.sort_values(by=['Session Id', 'Time'])
    
    # Compute the difference (in seconds) between consecutive events within each session.
    df['time_diff'] = df.groupby('Session Id')['Time'].diff().dt.total_seconds()
    
    # Instead of marking Idle as True/False, map time differences to descriptive strings.
    df['Idle'] = df['time_diff'].apply(lambda x: get_idle_string(x, idle_threshold_seconds))
    
    # Optionally remove the temporary 'time_diff' column.
    df.drop(columns=['time_diff'], inplace=True)
    
    # Write the output CSV file with the additional "Idle" column.
    df.to_csv(output_csv, index=False)
    print(f"Idle detection applied. Output written to {output_csv}")

if __name__ == "__main__":
    apply_idle_detector(INPUT_CSV, OUTPUT_CSV)
