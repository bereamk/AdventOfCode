# ------------------ Input ------------------
def collect_input():
    print("Enter input, then press Enter twice to finish:")
    lines = []
    while True:
        line = input().strip()
        if line == "":
            break
        lines.append(line)
    return lines  # Return as a list of strings (rows)


# ------------------ Methods ------------------
def search_word(grid, word):
    rows = len(grid)
    cols = len(grid[0])
    total_count = 0

    # Define directions: (row_step, col_step)
    directions = [
        (0, 1),  # Right
        (0, -1),  # Left
        (1, 0),  # Down
        (-1, 0),  # Up
        (1, 1),  # Diagonal down-right
        (-1, -1),  # Diagonal up-left
        (1, -1),  # Diagonal down-left
        (-1, 1)  # Diagonal up-right
    ]

    # Iterate through each cell in the grid
    for row in range(rows):
        for col in range(cols):
            if grid[row][col] == word[0]:  # Found starting point ('X')
                # Check in all directions
                for row_step, col_step in directions:
                    if (is_valid(row, col, row_step, col_step, word, rows, cols)
                            and matches_word(row, col, row_step, col_step, word, grid)):
                        total_count += 1

    return total_count


# Check if a word can fit starting at (r, c) in a given direction
def is_valid(r, c, row_step, col_step, word, rows, cols):
    word_length = len(word)

    end_row = r + (word_length - 1) * row_step
    end_col = c + (word_length - 1) * col_step
    return 0 <= end_row < rows and 0 <= end_col < cols


# Check if the word matches starting at (r, c) in a given direction
def matches_word(r, c, row_step, col_step, word, grid):
    word_length = len(word)

    for i in range(word_length):
        if grid[r + i * row_step][c + i * col_step] != word[i]:
            return False
    return True


# =================== MAIN ====================
def main():
    word = "XMAS"
    grid = collect_input()
    count = search_word(grid, word)
    print(f"Total occurrences of '{word}': {count}")


# ---------------- Execution ------------------
if __name__ == "__main__":
    main()
