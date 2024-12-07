import re


# ------------------ Input ------------------
def collect_input():
    print("Enter input, then press Enter twice to finish:")
    lines = []
    while True:
        line = input().strip()
        if line == "":
            break
        lines.append(line)
    return " ".join(lines)


# ------------------ Methods ------------------
def extract_segments(input_string):
    # Split the string at every "don't()"
    dont_parts = input_string.split("don't()")
    # print("Don't Parts:", dont_parts)  # Debug

    # Initialize a list to store valid `mul(a, b)` results
    valid_muls = []

    # Process each segment split by "don't()"
    for i, part in enumerate(dont_parts):  # Breaks the string into parts and #'s them
        if i == 0:
            # First segment: Process all valid `mul(a, b)` before any "don't()"
            valid_muls.extend(re.findall(r"mul\(\d+,\d+\)", part))
            # print("Valid Mul Segments:", valid_muls)  # Debug
        else:
            # For other segments, check for "do()" and process all valid `mul(a, b)` after it
            if "do()" in part:
                # Split on "do()" and process all parts after it
                do_parts = part.split("do()")
                # print("Do Parts:", do_parts)  # Debug
                for segment in do_parts[1:]:
                    valid_muls.extend(re.findall(r"mul\(\d+,\d+\)", segment))

    # Print and return the final list of valid `mul(a, b)`
    # print("Valid Mul Segments:", valid_muls)  # Debug
    return valid_muls


def multiply(input_segments):
    numbers = []
    multiplied = []
    for line in input_segments:
        matches = re.findall(r"mul\((\d+),(\d+)\)", line)
        for match in matches:
            a, b = map(int, match)
            numbers.append((a, b))
            # print(numbers)  # Debug
    for num in numbers:
        mult = mul(num[0], num[1])
        multiplied.append(mult)
        # print(multiplied)  # Debug
    print("The multiplied sum: ", sum(multiplied))


# ---------------- Calculation ----------------
def mul(x, y):
    return x * y


# =================== MAIN ====================
def main():
    input_string = collect_input()  # Collect the input
    segments = extract_segments(input_string)  # Extract valid segments
    multiply(segments)  # Multiply the numbers in the segments


# ------------------ EXECUTE ------------------
if __name__ == "__main__":
    main()
