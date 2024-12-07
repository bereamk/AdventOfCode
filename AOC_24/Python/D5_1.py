import re

# ------------------ Input ------------------
def collect_input():
    print("Enter input, then press 'q' to finish:")
    lines = []
    while True:
        line = input().strip()
        if line.lower() == "q":
            break
        lines.append(line)
    return lines  # Return as a list of strings (rows)

# ------------------ Methods ------------------
# Extract the pairs and updates
def find_patterns(input_lines):
    pair_list = []
    updates = []

    for line in input_lines:
        if "|" in line:
            pair = re.split(r"\|", line)
            if len(pair) == 2:
                pair_list.append((int(pair[0].strip()), int(pair[1].strip())))
        elif "," in line:
            updates.append([int(num.strip()) for num in line.split(",")])

    return pair_list, updates


# Evaluate if the condition is met and collect correct updates
def evaluate(pairs, updates):
    correct_updates = []

    for update in updates:
        is_valid_update = True  # Assume this update is correct unless proven otherwise

        for pair in pairs:
            first_page, second_page = pair
            if first_page in update and second_page in update:
                first_index = update.index(first_page)
                second_index = update.index(second_page)

                if first_index >= second_index:
                    is_valid_update = False  # Pair condition failed
                    break  # No need to check further pairs for this update
            else:
                is_valid_update = True  # Pair is missing

        if is_valid_update:
            correct_updates.append(update)  # Add the valid update to the list

    return correct_updates


# Find middle number in update and return the sum
def find_middle_number(updates):
    middle_numbers = []

    for update in updates:
        if len(update) % 2 == 0:
            middle_i = len(update) // 2 - 1
        else:
            middle_i = len(update) // 2
        middle = update[middle_i]
        middle_numbers.append(middle)

    total_sum = sum(middle_numbers)

    return middle_numbers, total_sum


# =================== MAIN ====================
def main():
    userinput = collect_input()  # Returns a list of strings
    pairs, updates = find_patterns(userinput)  # Extract pairs and updates
    correct = evaluate(pairs, updates)  # Evaluate the condition
    middles, total = find_middle_number(correct)  # Find the middle number in the update

    print("\nThe sum of middle numbers: ", total)


# ---------------- Execution ------------------
if __name__ == "__main__":
    main()