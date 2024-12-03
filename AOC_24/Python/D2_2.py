def main():
    print("Enter the reports. Type 'q' to calculate and quit.")

    input_lines = collect_reports()  # List is stored in input_lines
    unsafe_reports, safe_reports = process_reports(input_lines)  # Process the reports

    print("\n--------------------------")
    print("Total reports:", len(input_lines))
    print("Unsafe reports:", len(unsafe_reports))
    print("Safe reports:", len(safe_reports))


def collect_reports():  # Collects reports from user input, accepting multiple lines.
    print("Enter all reports (one per line), then press Enter twice to finish:")
    lines = []
    while True:
        line = input().strip()
        if line == "":  # Stops on an empty line (double Enter)
            break
        lines.append(line)
    return lines


def calculate(report):  # Calculates the ranges between levels in a report.
    try:
        levels = [int(x) for x in report.split()]  # Convert levels to integers
        ranges = []

        for i in range(len(levels) - 1):
            ranges.append(levels[i] - levels[i + 1])

        # print("The ranges are:", ranges)
        return ranges
    except ValueError:
        print(f"Invalid report: {report}. Please enter numbers separated by spaces.")
        return []


def is_unsafe(ranges):  # Determines if a report is unsafe based on given criteria.
    if not ranges:
        return "\n    - Invalid input."
    if not (all(r <= 0 for r in ranges) or all(r >= 0 for r in ranges)):
        return "\n    - Values are not all positive or negative."
    if any(abs(r) > 3 for r in ranges):
        return "\n    - Difference between levels exceeds 3."
    if 0 in ranges:
        return "\n    - Difference between levels is 0."
    return None


def process_reports(reports):  # Processes reports to classify them as safe or unsafe.
    unsafe_reports = []
    safe_reports = []

    for report in reports:
        original_levels = report.split()  # Store the original levels for potential fixing
        ranges = calculate(report)  # Calculate the ranges between levels
        reason = is_unsafe(ranges)  # Check if the report is unsafe

        if reason:  # If the report is unsafe, print the reason
            # print(f"The report '{report}' is unsafe: {reason}")

            # Attempt to fix the report by removing levels
            fixed_levels = attempt_to_fix_report(original_levels)

            if fixed_levels:  # If a fix is found, print the fixed report
                fixed_report = ' '.join(fixed_levels)  # Convert levels back to a string
                # print(f"    - Fixed report '{fixed_report}' is now safe.")
                safe_reports.append(fixed_report)
            else:
                unsafe_reports.append(report)
        else:
            # print(f"    - The report '{report}' is safe.")
            safe_reports.append(report)

    return unsafe_reports, safe_reports


def attempt_to_fix_report(levels):  # Attempts to fix an unsafe report by removing levels.
    # Convert string levels to integers for processing
    levels = [int(x) for x in levels]

    for i in range(len(levels)):
        # Remove the i-th level from the levels list
        modified_levels = levels[:i] + levels[i + 1:]
        modified_ranges = []

        # Loop through the indices of modified_levels (except the last one)
        for j in range(len(modified_levels) - 1):
            diff = modified_levels[j] - modified_levels[j + 1]
            modified_ranges.append(diff)

        # Check if the modified ranges are safe
        if not is_unsafe(modified_ranges):
            # Convert back to strings for consistency with reports
            return [str(x) for x in modified_levels]
    return None  # Return None if no fix makes the report safe


if __name__ == '__main__':
    main()
