import pandas as pd
import math
from datetime import datetime

# Tunable parameters
windowSize = 10
threshold = 3
seedTime = 25  # in seconds

# BKT parameters (default)
BKTparams = {
    "p_transit": 0.2,
    "p_slip": 0.1,
    "p_guess": 0.2,
    "p_know": 0.25  # default initial p_know
}

# Helper functions (translated from JS)
def lastActionIsHint(help_vars):
    return help_vars["lastAction"] == "hint"

def lastActionIsError(help_vars):
    return help_vars["lastAction"] == "error"

def seenAllHintLevels(e, help_vars):
    if e["data"]["tutor_data"]["action_evaluation"].lower() == "hint":
        sel = e["data"]["tutor_data"]["selection"]
    else:
        sel = e["data"]["tool_data"]["selection"]
    return help_vars["seenAllHints"].get(sel, False)

def secondsSinceLastAction(current_time, last_action_time):
    if last_action_time is None:
        return float('inf')
    return (current_time - last_action_time).total_seconds()

def isDeliberate(e, help_vars, current_time):
    errorThreshold = 2
    newStepThreshold = 1
    last_hint_length = float(help_vars["lastHintLength"]) if help_vars["lastHintLength"] != "" else 0
    hintThreshold = (last_hint_length / 600)*60 if last_hint_length > 0 else 0
    diff = secondsSinceLastAction(current_time, help_vars["lastActionTime"])
    if lastActionIsError(help_vars):
        return diff > errorThreshold
    elif lastActionIsHint(help_vars):
        return diff > hintThreshold
    else:
        return diff > newStepThreshold

def isFamiliar(onboardSkills):
    familiarityThreshold = 0.4
    for _, params in onboardSkills.items():
        if params["p_know"] <= familiarityThreshold:
            return False
    return True

def hintIsHelpful():
    return True

def lastActionUnclearFix(help_vars):
    return not help_vars["lastSenseOfWhatToDo"]

def senseOfWhatToDo(onboardSkills):
    senseThreshold = 0.6
    for _, params in onboardSkills.items():
        if params["p_know"] <= senseThreshold:
            return False
    return True

def evaluateAction(e, help_vars, onboardSkills, current_time):
    outcome = e["data"]["tutor_data"]["action_evaluation"].lower()
    if outcome == "hint":
        if isDeliberate(e, help_vars, current_time):
            if (not seenAllHintLevels(e, help_vars)) and (
                (not isFamiliar(onboardSkills)) or
                (lastActionIsError(help_vars) and lastActionUnclearFix(help_vars)) or
                (lastActionIsHint(help_vars) and not hintIsHelpful())
            ):
                return "preferred/ask hint"
            elif (isFamiliar(onboardSkills) and (not senseOfWhatToDo(onboardSkills))) or lastActionIsHint(help_vars):
                return "acceptable/ask hint"
            else:
                return "not acceptable/hint abuse"
        else:
            return "not acceptable/hint abuse"
    else:
        if isDeliberate(e, help_vars, current_time):
            if (isFamiliar(onboardSkills) and not (lastActionIsError(help_vars) and lastActionUnclearFix(help_vars))) or \
               (lastActionIsHint(help_vars) and hintIsHelpful()):
                return "preferred/try step"
            elif seenAllHintLevels(e, help_vars) and not (lastActionIsError(help_vars) and lastActionUnclearFix(help_vars)):
                return "preferred/try step"
            elif outcome == "correct":
                return "acceptable/try step"
            elif seenAllHintLevels(e, help_vars):
                if lastActionIsError(help_vars) and lastActionUnclearFix(help_vars):
                    return "ask teacher for help/try step"
            else:
                return "not acceptable/hint avoidance"
        else:
            return "not acceptable/not deliberate"

def updateHistory(e, help_vars, current_time, onboardSkills):
    help_vars["lastActionTime"] = current_time
    outcome = e["data"]["tutor_data"]["action_evaluation"].lower()
    if outcome == "hint":
        help_vars["lastAction"] = "hint"
        tutor_advice = e["data"]["tutor_data"].get("tutor_advice", "")
        help_vars["lastHintLength"] = str(len(tutor_advice.split()))
        sel = e["data"]["tutor_data"]["selection"]
        if sel not in help_vars["seenAllHints"]:
            help_vars["seenAllHints"][sel] = (e["data"]["tutor_data"].get("current_hint_number", 0) == e["data"]["tutor_data"].get("total_hints_available", 0))
    elif outcome == "incorrect":
        help_vars["lastAction"] = "error"
    elif outcome == "correct":
        help_vars["lastAction"] = "correct"
    help_vars["lastSenseOfWhatToDo"] = senseOfWhatToDo(onboardSkills)

def updateBKT(skill, outcome, onboardSkills):
    if skill not in onboardSkills:
        onboardSkills[skill] = BKTparams.copy()
    p_know = onboardSkills[skill]["p_know"]
    p_slip = onboardSkills[skill]["p_slip"]
    p_guess = onboardSkills[skill]["p_guess"]
    p_transit = onboardSkills[skill]["p_transit"]
    if outcome == "correct":
        p_know_given_obs = (p_know * (1-p_slip)) / ((p_know*(1-p_slip)) + ((1-p_know)*p_guess))
    else:
        p_know_given_obs = (p_know * p_slip) / ((p_know*p_slip) + ((1-p_know)*(1-p_guess)))
    new_p_know = p_know_given_obs + (1 - p_know_given_obs)*p_transit
    onboardSkills[skill]["p_know"] = math.floor(new_p_know * 100) / 100.0

# Main processing per session
def process_session(session_df):
    # Initialize state for the session
    attemptWindow = [1] * windowSize
    onboardSkills = {}  # keys: skill names
    stepCounter = {}    # keys: step id
    help_vars = {
        "lastAction": "null",
        "lastActionTime": None,
        "seenAllHints": {},
        "lastHintLength": "",
        "lastSenseOfWhatToDo": False
    }
    # The detector's internal state as a descriptive string, starting with "0, > 0 s, "
    detector_value = "0, > 0 s, "
    initTime = None

    struggle_results = []

    # Process rows sequentially (assumed sorted by Time)
    for idx, row in session_df.iterrows():
        current_time = row["Time"]  # already a datetime

        # Skip if Selection indicates end of session (e.g., "Done Button")
        if str(row["Selection"]).strip().lower() == "done button":
            struggle_results.append("0, > 0 s, ")
            continue

        # Build event dictionary similar to JS e
        e = {
            "data": {
                "actor": "student",
                "tool_data": {
                    "selection": row["Selection"],
                    "action": row["Action"]
                },
                "tutor_data": {
                    "action_evaluation": row["Outcome"].lower() if isinstance(row["Outcome"], str) else "incorrect",
                    "selection": row["Selection"],
                    "step_id": row["Step Name"],
                },
                "tutor_data_skills": [row["KC_Model(MATHia)"]]
            }
        }
        outcome_raw = e["data"]["tutor_data"]["action_evaluation"]
        if outcome_raw in ["ok", "correct"]:
            outcome = "correct"
        elif outcome_raw in ["jit", "hint"]:
            outcome = "hint"
        else:
            outcome = "incorrect"
        e["data"]["tutor_data"]["action_evaluation"] = outcome

        currStep = row["Step Name"]
        skill = row["KC_Model(MATHia)"]

        # Update stepCounter and BKT for new step
        if currStep in stepCounter:
            stepCounter[currStep] += 1
        else:
            stepCounter[currStep] = 1
            updateBKT(skill, outcome, onboardSkills)

        # Evaluate help model output
        if help_vars["lastAction"] != "null":
            help_model_output = evaluateAction(e, help_vars, onboardSkills, current_time)
        else:
            help_model_output = "preferred"
        
        # Update attempt window based on outcome (ignore hint requests if all hints seen)
        if not (outcome == "hint" and seenAllHintLevels(e, help_vars)):
            attemptCorrect = 1 if outcome == "correct" else 0
            attemptWindow.pop(0)
            attemptWindow.append(attemptCorrect)
        if help_model_output == "ask teacher for help/try step":
            attemptWindow = [0] * windowSize

        sumCorrect = sum(attemptWindow)

        # Set elaboration string based on help model output and sumCorrect
        if sumCorrect <= threshold:
            if help_model_output == "ask teacher for help/try step":
                elaborationString = "hints aren't helping"
            elif help_model_output == "not acceptable/hint avoidance":
                elaborationString = "not using hints"
            else:
                elaborationString = "lots of errors"
        else:
            elaborationString = " "

        # Update detector state: if not struggling and sumCorrect is low, start struggle period.
        if detector_value.split(',')[0] == "0" and (sumCorrect <= threshold):
            initTime = current_time
            detector_value = "1, > " + str(seedTime) + " s, " + elaborationString
        elif detector_value.split(',')[0] != "0" and (sumCorrect <= threshold):
            # Optionally update elapsed time in detector_value (omitted here)
            pass
        else:
            detector_value = "0, > 0 s, " + elaborationString
            initTime = None

        updateHistory(e, help_vars, current_time, onboardSkills)

        # Instead of converting to boolean, append the full descriptive detector_value string.
        struggle_results.append(detector_value)

    return struggle_results

def process_csv(input_csv, output_csv):
    df = pd.read_csv(input_csv)
    df["Time"] = pd.to_datetime(df["Time"])
    df = df.sort_values(by=["Session Id", "Time"]).reset_index(drop=True)

    all_flags = []
    for session, group in df.groupby("Session Id", sort=False):
        group = group.copy()
        flags = process_session(group)
        all_flags.extend(flags)
    
    df["Struggle"] = all_flags
    df.to_csv(output_csv, index=False)
    print(f"Struggle detection applied. Output written to {output_csv}")

if __name__ == "__main__":
    INPUT_CSV = "data.csv"
    OUTPUT_CSV = "new_struggle_res_full.csv"
    process_csv(INPUT_CSV, OUTPUT_CSV)
