def main():
    print("Enter the lists of numbers (format: number   number):")
    print("Type 'q' to calculate and quit.")

    # Step 1: Input Handling
    input_lines = []  # To store the raw input lines
    while True:
        line = input().strip()  # removes leading/trailing whitespaces
        if line.lower() == 'q':
            break
        input_lines.append(line)

    # Step 2: Storage
    left_list = []  # To store the left numbers
    right_list = []  # To store the right numbers

    for line in input_lines:
        parts = line.split()
        if len(parts) == 2:
            left_list.append(int(parts[0].strip()))
            right_list.append(int(parts[1].strip()))

    print("left_list:", left_list)
    print("right_list:", right_list)

    # Step 3: counting
    multiplied = []

    for i in range(len(left_list)):
        num = left_list[i]
        mult = left_list[i] * right_list.count(num)
        multiplied.append(mult)

    # Step 5: Summing the results
    total_distance = sum(multiplied)

    # Step 6: Output
    print("The total distance is:", total_distance)


# Run the program
if __name__ == '__main__':
    main()
