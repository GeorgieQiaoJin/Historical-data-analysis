import pandas as pd
import math
from datetime import datetime


# Tunable constants and parameters
WINDOW_SIZE = 6
THRESHOLD = 1
MASTERY_THRESHOLD = 0.8
BKT_PARAMS = {
    "p_transit": 0.2,
    "p_slip": 0.1,
    "p_guess": 0.2,
    "p_know": 0.25  # default initial p_know
}
WHEEL_SPINNING_ATTEMPT_THRESHOLD = 10
SEED_TIME = 25  # seconds

# ---------------------------
# Helper functions
# ---------------------------

def update_skill_levels_attempts(onboardSkills, skill, curr_step_count, skillLevelsAttempts):
    """
    For a given skill, update the count and current p_know estimate.
    """
    if skill in skillLevelsAttempts:
        if curr_step_count == 1:
            skillLevelsAttempts[skill][0] += 1
        # Update the current p_know from onboardSkills.
        skillLevelsAttempts[skill][1] = onboardSkills[skill]["p_know"]
    else:
        if skill in onboardSkills:
            skillLevelsAttempts[skill] = [1, onboardSkills[skill]["p_know"]]
    return skillLevelsAttempts

def detect_wheel_spinning(skillLevelsAttempts):
    """
    Returns True if for any skill the attempt count is at least the wheel-spinning threshold
    and the current p_know is below the mastery threshold.
    """
    for skill, (attempt_count, p_know) in skillLevelsAttempts.items():
        if attempt_count >= WHEEL_SPINNING_ATTEMPT_THRESHOLD and p_know < MASTERY_THRESHOLD:
            # Detected wheel spinning on this skill.
            return True
    return False

def update_BKT(onboardSkills, skill, outcome):
    """
    Update the BKT estimate for the given skill based on whether the outcome was "correct" or not.
    """
    # Initialize the skill if not seen before.
    if skill not in onboardSkills:
        onboardSkills[skill] = BKT_PARAMS.copy()
    p_know = onboardSkills[skill]["p_know"]
    p_slip = onboardSkills[skill]["p_slip"]
    p_guess = onboardSkills[skill]["p_guess"]
    p_transit = onboardSkills[skill]["p_transit"]
    if outcome == "correct":
        numerator = p_know * (1 - p_slip)
        denominator = numerator + ((1 - p_know) * p_guess)
    else:
        numerator = p_know * p_slip
        denominator = numerator + ((1 - p_know) * (1 - p_guess))
    p_know_given_obs = numerator / denominator if denominator != 0 else 0
    new_p_know = p_know_given_obs + (1 - p_know_given_obs) * p_transit
    # Round down to two decimals.
    onboardSkills[skill]["p_know"] = math.floor(new_p_know * 100) / 100.0
    return onboardSkills

# ---------------------------
# Session processing
# ---------------------------

def process_session(session_df):
    """
    Process one session (group of events with the same "Session Id").
    Returns a list of descriptive detector strings (one per row) indicating the critical struggle state.
    """
    # Initialize state variables for the session.
    attemptWindow = [0] * WINDOW_SIZE  # sliding window: 1 if wheel-spinning detected, else 0.
    skillLevelsAttempts = {}  # dict mapping skill to [attempt count, current p_know]
    onboardSkills = {}        # dict mapping skill to its BKT parameters (updated over time)
    stepCounter = {}          # dict mapping step (e.g. "Step Name") to the count of events for that step.
    
    # The detector's internal state is a string that starts with "0, > 0 s, "
    detector_value = "0, > 0 s, "
    initTime = None
    elaborationString = " "

    # List to store the output for each event in this session.
    critical_flags = []

    # Process events in order (session_df is assumed sorted by Time)
    for idx, row in session_df.iterrows():
        current_time = row["Time"]
        
        # Skip events where the "Selection" indicates the end (e.g. "Done Button")
        if str(row["Selection"]).strip().lower() == "done button":
            critical_flags.append("0, > 0 s, ")
            continue

        # Retrieve current step and skill.
        currStep = row["Step Name"]
        skill = row["KC_Model(MATHia)"]

        # Normalize the outcome.
        outcome_raw = str(row["Outcome"]).strip().lower()
        if outcome_raw in ["ok", "correct"]:
            outcome = "correct"
        elif outcome_raw in ["hint", "jit"]:
            outcome = "hint"
        else:
            outcome = "incorrect"

        # BKT update: if this is the first event for this step, update the skill's BKT estimate.
        if currStep not in stepCounter:
            onboardSkills = update_BKT(onboardSkills, skill, outcome)
        # Update the step counter.
        stepCounter[currStep] = stepCounter.get(currStep, 0) + 1

        # Update the skill levels attempts for this skill.
        skillLevelsAttempts = update_skill_levels_attempts(onboardSkills, skill, stepCounter[currStep], skillLevelsAttempts)

        # Detect wheel spinning.
        isWheelSpinning = detect_wheel_spinning(skillLevelsAttempts)

        # Update the sliding window: remove the oldest entry and append current value.
        attemptWindow.pop(0)
        attemptWindow.append(1 if isWheelSpinning else 0)
        sumAskTeacherForHelp = sum(attemptWindow)

        # Set elaboration string based on the sliding window.
        if sumAskTeacherForHelp >= THRESHOLD:
            elaborationString = "slow to master some skills"
        else:
            elaborationString = " "

        # Update the detector state based on whether a struggle is detected.
        if sumAskTeacherForHelp >= THRESHOLD:
            if detector_value.startswith("0"):
                # Transition: first time detecting struggle.
                initTime = current_time
            # If already struggling, update based on elapsed time.
            if initTime is not None:
                time_diff = (current_time - initTime).total_seconds()
                if time_diff > (600 - SEED_TIME):
                    detector_value = "1, > 10 min, " + elaborationString
                elif time_diff > (300 - SEED_TIME):
                    detector_value = "1, > 5 min, " + elaborationString
                elif time_diff > (120 - SEED_TIME):
                    detector_value = "1, > 2 min, " + elaborationString
                elif time_diff > (60 - SEED_TIME):
                    detector_value = "1, > 1 min, " + elaborationString
                elif time_diff > (45 - SEED_TIME):
                    detector_value = "1, > 45 s, " + elaborationString
                else:
                    detector_value = "1, > " + str(SEED_TIME) + " s, " + elaborationString
            else:
                detector_value = "1, > " + str(SEED_TIME) + " s, " + elaborationString
        else:
            detector_value = "0, > 0 s, " + elaborationString
            initTime = None

        # Append the detector string for this event.
        critical_flags.append(detector_value)

    return critical_flags


# CSV processing
def process_csv(input_csv, output_csv):
    # Read the CSV file.
    df = pd.read_csv(input_csv)
    # Convert the "Time" column to datetime objects.
    df["Time"] = pd.to_datetime(df["Time"])
    # Sort by Session Id and Time.
    df = df.sort_values(by=["Session Id", "Time"]).reset_index(drop=True)

    all_flags = []
    # Process each session (grouped by "Session Id").
    for session, group in df.groupby("Session Id", sort=False):
        group = group.copy()
        flags = process_session(group)
        all_flags.extend(flags)
    
    # Add the new "Critical Struggle" column to the DataFrame.
    df["Critical Struggle"] = all_flags

    # Write the output CSV file (same format as input with the additional column).
    df.to_csv(output_csv, index=False)
    print(f"Critical struggle detection applied. Output written to {output_csv}")

if __name__ == "__main__":
    # Change these file names as desired.
    INPUT_CSV = "data.csv"
    OUTPUT_CSV = "new_crit_struggle_res_full.csv"
    process_csv(INPUT_CSV, OUTPUT_CSV)
