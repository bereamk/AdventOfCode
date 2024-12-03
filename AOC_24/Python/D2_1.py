# ------------------ Input ------------------
def collect_reports(): # Collects reports from user input, accepting multiple lines.
    lines = []
    while True:
        line = input().strip()
        if line == "":  # Stops on an empty line (double Enter)
            break
        lines.append(line)
    return lines


# ---------------- Calculation ----------------
def calculate(report):  # Calculates the ranges between levels in a report.
    try:
        levels = [int(x) for x in report.split()]  # Convert levels to integers
        ranges = []

        for i in range(len(levels) - 1):
            ranges.append(levels[i] - levels[i + 1])

        # print("The ranges are:", ranges) # Debug
        return ranges
    except ValueError:
        print(f"Invalid report: {report}. Please enter numbers separated by spaces.")
        return []


# ------------------ Methods ------------------
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


def process_reports(reports):
    """Processes reports to classify them as safe or unsafe."""
    unsafe_reports = []
    safe_reports = []

    for report in reports:
        ranges = calculate(report)
        reason = is_unsafe(ranges)

        if reason:
            # print(f"The report '{report}' is unsafe: {reason}")
            unsafe_reports.append(report)
        else:
            # print(f"    - The report '{report}' is safe.")
            safe_reports.append(report)

    return unsafe_reports, safe_reports


# =================== MAIN ====================
def main():
    print("Enter all reports (one per line), then press Enter twice to finish:")

    input_lines = collect_reports()  # List is stored in input_lines
    unsafe_reports, safe_reports = process_reports(input_lines) # Process the reports

    print("--------------------------")
    print("Total reports:", len(input_lines))
    print("Unsafe reports:", len(unsafe_reports))
    print("Safe reports:", len(safe_reports))


# ------------------ EXECUTE ------------------
if __name__ == '__main__':
    main()
