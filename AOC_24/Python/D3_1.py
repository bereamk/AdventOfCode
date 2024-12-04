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
    return lines


# ------------------ Methods ------------------
def splitting(user_input):
    numbers = []
    multiplied = []
    for line in user_input:
        matches = re.findall(r"mul\((\d+),(\d+)\)", line)
        for match in matches:
            a, b = map(int, match)
            numbers.append((a, b))
            # print(numbers) # Debug
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
    userinput = collect_input()  # returns a list of strings
    splitting(userinput)


# ------------------ EXECUTE ------------------
if __name__ == "__main__":
    main()
